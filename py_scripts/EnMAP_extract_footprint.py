# A script to extract a footprint polygon and metadata from EnMAP GeoTIFF files in a folder.

import os
import json
import csv
from pathlib import Path
import numpy as np
from datetime import datetime
from shapely.geometry import Polygon, mapping
from scipy.spatial import ConvexHull
import rasterio
from rasterio.warp import transform_geom
from rasterio.mask import mask as rasterio_mask
import geopandas as gpd
from pyproj import Transformer

def extract_enmap_footprint(enmap_file, use_valid_data_mask=True, verbose=True, 
                           filter_large_areas=True, max_area=1200, target_crs='EPSG:32753'):
    """
    Parameters:
    enmap_file: path to EnMAP .tif file
    use_valid_data_mask: use actual valid data pixels (True) or just image bounds (False)
    verbose: print progress messages if True
    filter_large_areas: filter scenes with suspiciously large areas
    max_area: maximum allowed area in km² for UTM or sq degrees otherwise (default 1200 km²)
    target_crs: target CRS for output (default 'EPSG:32753'), set to None to keep native CRS
    
    Returns:
    a dictionary with metadata and geometry, or None if failed
    """
    try:
        if verbose:
            print(f"  Opening GeoTIFF...")
        
        scene_name = os.path.basename(enmap_file).replace('.tif', '').replace('.tiff', '')
        
        with rasterio.open(enmap_file) as src:
            
            if verbose:
                print(f"  Extracting metadata...")
            
            # Get metadata from tags
            tags = src.tags()
            
            # Get acquisition time
            time_coverage_start = None
            for time_key in ['ACQUISITION_DATE', 'DATE', 'acquisitionDate', 'TIFFTAG_DATETIME']:
                if time_key in tags:
                    time_coverage_start = tags[time_key]
                    break
            
            # Also check filename for date if not found in tags
            if not time_coverage_start:
                # EnMAP naming: ENMAP01-____L2A-DT0000001234_20210901T123456Z_...
                import re
                date_match = re.search(r'(\d{8})T(\d{6})', scene_name)
                if date_match:
                    date_str = date_match.group(1)
                    time_str = date_match.group(2)
                    time_coverage_start = f"{date_str[:4]}-{date_str[4:6]}-{date_str[6:8]}T{time_str[:2]}:{time_str[2:4]}:{time_str[4:6]}Z"
            
            native_crs = str(src.crs) if src.crs else 'unknown'
            
            if verbose:
                print(f"  Acquisition time: {time_coverage_start}")
                print(f"  Native CRS: {native_crs}")
                print(f"  Image shape: {src.height} x {src.width}")
                print(f"  Number of bands: {src.count}")
            
            # Get the image bounds in native CRS
            bounds = src.bounds
            transform_matrix = src.transform
            
            if verbose:
                print(f"  Bounds (native CRS): {bounds}")
            
            if use_valid_data_mask:
                if verbose:
                    print(f"  Reading valid data mask...")
                
                # Read the mask or first band to determine valid pixels
                # EnMAP may have nodata values or a mask band
                nodata = src.nodata
                
                # Try to use the dataset mask (combines all bands)
                try:
                    # Read at lower resolution for efficiency
                    # out_shape: (height // decimation, width // decimation)
                    decimation = max(1, src.height // 500)  # ~500x500 max
                    
                    if verbose:
                        print(f"  Reading mask at decimation factor: {decimation}")
                    
                    # Read first band to check for valid data
                    band1 = src.read(1, 
                                    out_shape=(src.height // decimation, src.width // decimation),
                                    resampling=rasterio.enums.Resampling.nearest)
                    
                    # Create valid mask
                    if nodata is not None:
                        valid_mask = band1 != nodata
                    else:
                        # If no nodata value, assume all non-zero pixels are valid
                        valid_mask = band1 > 0
                    
                    if verbose:
                        valid_count = valid_mask.sum()
                        total_count = valid_mask.size
                        print(f"  Valid pixels: {valid_count}/{total_count} ({100*valid_count/total_count:.1f}%)")
                    
                    if not valid_mask.any():
                        print(f"  WARNING: No valid pixels found, using image bounds")
                        use_valid_data_mask = False
                    else:
                        # Get pixel coordinates of valid data
                        valid_rows, valid_cols = np.where(valid_mask)
                        
                        # Scale back to full resolution coordinates
                        valid_rows = valid_rows * decimation
                        valid_cols = valid_cols * decimation
                        
                        # Check if data fills the image
                        data_height = valid_rows.max() - valid_rows.min() + 1
                        data_width = valid_cols.max() - valid_cols.min() + 1
                        fill_ratio = len(valid_rows) / (data_height * data_width)
                        
                        if verbose:
                            print(f"  Data extent: {data_height} x {data_width} pixels")
                            print(f"  Fill ratio: {fill_ratio*100:.1f}%")
                        
                        # Sample points around the perimeter of valid data
                        if verbose:
                            print(f"  Extracting footprint from valid data...")
                        
                        # Subsample if too many points
                        if len(valid_rows) > 5000:
                            step = len(valid_rows) // 5000
                            valid_rows = valid_rows[::step]
                            valid_cols = valid_cols[::step]
                            if verbose:
                                print(f"  Subsampled to {len(valid_rows)} points")
                        
                        # Convert pixel coordinates to geographic coordinates (in native CRS)
                        xs, ys = rasterio.transform.xy(transform_matrix, valid_rows, valid_cols)
                        
                        # Keep in native CRS (no transformation yet)
                        xs = np.array(xs)
                        ys = np.array(ys)
                        
                        # Remove any invalid coordinates
                        valid_coords = np.isfinite(xs) & np.isfinite(ys)
                        xs = xs[valid_coords]
                        ys = ys[valid_coords]
                        
                        if len(xs) < 3:
                            print(f"  WARNING: Not enough valid coordinates, using image bounds")
                            use_valid_data_mask = False
                        
                except Exception as e:
                    print(f"  WARNING: Could not read data mask: {str(e)}")
                    print(f"  Falling back to image bounds")
                    use_valid_data_mask = False
            
            # If not using valid data mask, use image corners
            if not use_valid_data_mask:
                if verbose:
                    print(f"  Using image corner coordinates...")
                
                # Get corner coordinates in pixel space
                corners_px = [
                    (0, 0),                          # top left
                    (src.width, 0),                  # top right
                    (src.width, src.height),         # bottom right
                    (0, src.height)                  # bottom left
                ]
                
                # Convert to geographic coordinates (in native CRS)
                xs = []
                ys = []
                
                for col, row in corners_px:
                    x, y = rasterio.transform.xy(transform_matrix, row, col)
                    xs.append(x)
                    ys.append(y)
                
                xs = np.array(xs)
                ys = np.array(ys)
            
            # Create convex hull or polygon (in native CRS)
            if verbose:
                print(f"  Computing footprint...")
            
            try:
                if use_valid_data_mask and len(xs) > 4:
                    # Use convex hull for valid data points
                    points = np.column_stack([xs, ys])
                    hull = ConvexHull(points)
                    hull_points = points[hull.vertices]
                    polygon = Polygon(hull_points)
                    
                    if verbose:
                        print(f"  Created convex hull with {len(hull.vertices)} vertices")
                else:
                    # Use corner coordinates
                    polygon = Polygon(zip(xs, ys))
                    
                    if verbose:
                        print(f"  Created polygon from corner coordinates")
                
                # Calculate area in native CRS
                if 'UTM' in native_crs or '327' in native_crs:
                    # UTM is in meters
                    area_value = polygon.area / 1_000_000  # Convert m² to km²
                    area_units = 'km2'
                else:
                    area_value = polygon.area
                    area_units = 'sq_degrees'
                
                if verbose:
                    if area_units == 'km2':
                        print(f"  Polygon area: {area_value:.2f} km²")
                    else:
                        print(f"  Polygon area: {area_value:.6f} sq degrees")
                
                # Filter by area if required
                if filter_large_areas and area_value > max_area:
                    if verbose:
                        print(f"  X FILTERED: Area too large ({area_value:.1f} {area_units} > {max_area} limit)")
                    return None
                
            except Exception as e:
                print(f"  X ERROR: Could not create polygon: {str(e)}")
                return None
            
            # Calculate center (in native CRS)
            centroid = polygon.centroid
            center_x = float(centroid.x)
            center_y = float(centroid.y)
            
            # Transform to target CRS if needed
            if target_crs and native_crs != target_crs:
                if verbose:
                    print(f"  Transforming from {native_crs} to {target_crs}...")
                
                try:
                    # Create transformer
                    transformer = Transformer.from_crs(native_crs, target_crs, always_xy=True)
                    
                    # Transform polygon coordinates
                    original_coords = list(polygon.exterior.coords)
                    transformed_coords = []
                    
                    for x, y in original_coords:
                        new_x, new_y = transformer.transform(x, y)
                        transformed_coords.append((float(new_x), float(new_y)))
                    
                    # Create new polygon in target CRS
                    polygon = Polygon(transformed_coords)
                    
                    # Transform center point
                    center_x, center_y = transformer.transform(center_x, center_y)
                    center_x = float(center_x)
                    center_y = float(center_y)
                    
                    # Recalculate area in target CRS
                    if 'UTM' in target_crs or '327' in target_crs:
                        area_value = polygon.area / 1_000_000  # m² to km²
                        area_units = 'km2'
                    else:
                        area_value = polygon.area
                        area_units = 'sq_degrees'
                    
                    if verbose:
                        print(f"  Transformed successfully")
                    
                    output_crs = target_crs
                    original_crs = native_crs
                    
                except Exception as e:
                    print(f"  WARNING: CRS transformation failed: {str(e)}")
                    print(f"  Keeping native CRS")
                    output_crs = native_crs
                    original_crs = None
            else:
                output_crs = native_crs
                original_crs = None
            
            if verbose:
                print(f"  Center: ({center_x:.2f}, {center_y:.2f})")
            
            # Store coordinates
            final_coords = [(float(x), float(y)) for x, y in polygon.exterior.coords]
            
            # Extract additional metadata
            metadata = {
                'name': scene_name,
                'source': 'EnMAP',
                'acquisitionDate': time_coverage_start,
                'acquisitionTime': time_coverage_start.split('T')[1] if time_coverage_start and 'T' in time_coverage_start else None,
                'centerX': center_x,
                'centerY': center_y,
                'geometry': mapping(Polygon(final_coords)),
                'coordinates': final_coords,
                'polygon_area': area_value,
                'area_units': area_units,
                'crs': output_crs,
                'image_width': src.width,
                'image_height': src.height,
                'num_bands': src.count
            }
            
            # Add original CRS if transformed
            if original_crs:
                metadata['original_crs'] = original_crs
            
            # Add any useful tags
            for tag_key in ['PROCESSING_LEVEL', 'PRODUCT_TYPE', 'SENSOR', 'SATELLITE']:
                if tag_key in tags:
                    metadata[tag_key] = tags[tag_key]
            
            if verbose:
                print(f"  √ Successfully extracted footprint")
            
            return metadata
    
    except Exception as e:
        print(f"  X ERROR processing file: {str(e)}")
        import traceback
        if verbose:
            traceback.print_exc()
        return None


if __name__ == "__main__":
    # USER CONFIGURATION
    enmap_folder = "Q:/EnMAP_images"
    output_format = 'geojson'  # 'geojson' or 'csv'
    target_crs = 'EPSG:32753'  # UTM Zone 53S for Kangaroo Island
    verbose = True
    
    # Find EnMAP files
    folder = Path(enmap_folder)
    enmap_files = []
    for pattern in ['*.tif', '*.tiff', '*enmap*.tif', '*ENMAP*.tif']:
        files = list(folder.glob(pattern))
        enmap_files.extend(files)
    
    # Remove duplicates
    enmap_files = list(set(enmap_files))
    
    if not enmap_files:
        print(f"X No .tif files found in {enmap_folder}")
        exit(1)
    
    print("="*80)
    print(f"EnMAP FOOTPRINT EXTRACTION")
    print("="*80)
    print(f"Found {len(enmap_files)} EnMAP files")
    print(f"Output format: {output_format.upper()}")
    print(f"Target CRS: {target_crs}")
    print("="*80)
    
    # Process each file
    footprints = []
    successful = 0
    failed = 0
    
    for idx, enmap_file in enumerate(enmap_files, 1):
        print(f"\n[{idx}/{len(enmap_files)}] Processing: {enmap_file.name}")
        print("-" * 80)
        
        footprint = extract_enmap_footprint(
            str(enmap_file),
            use_valid_data_mask=True,
            verbose=verbose,
            filter_large_areas=True,
            max_area=1200,
            target_crs=target_crs
        )
        
        if footprint:
            footprints.append(footprint)
            successful += 1
            print(f"  √ Scene {idx}/{len(enmap_files)} completed successfully")
        else:
            failed += 1
            print(f"  X Scene {idx}/{len(enmap_files)} failed or filtered")
    
    print("\n" + "="*80)
    print(f"PROCESSING COMPLETE")
    print("="*80)
    print(f"Successful: {successful}/{len(enmap_files)} ({100*successful/len(enmap_files) if enmap_files else 0:.1f}%)")
    print(f"Failed or filtered: {failed}/{len(enmap_files)}")
    
    if not footprints:
        print("X No valid footprints extracted")
        exit(1)
    
    # Check CRS
    unique_crs = set(fp.get('original_crs', fp['crs']) for fp in footprints)
    print(f"\nOriginal CRS found:")
    for crs in unique_crs:
        count = sum(1 for fp in footprints if fp.get('original_crs', fp['crs']) == crs)
        print(f"  {crs}: {count} file(s)")
    print(f"All footprints output in: {target_crs}")
    
    # Export results
    # USER CONFIGURATION
    print(f"\nExporting {len(footprints)} footprints to {output_format.upper()}...")
    output_file = folder / f"enmap_footprints_v9.{output_format}"
    
    if output_format == 'geojson':
        print("  Creating GeoJSON features...")
        
        features = []
        for fp in footprints:
            feature = {
                'type': 'Feature',
                'geometry': fp['geometry'],
                'properties': {k: v for k, v in fp.items() if k not in ['geometry', 'coordinates']}
            }
            features.append(feature)
        
        geojson = {
            'type': 'FeatureCollection',
            'crs': {
                'type': 'name',
                'properties': {
                    'name': target_crs
                }
            },
            'features': features
        }
        
        print(f"  Writing to {output_file.name}...")
        with open(output_file, 'w') as f:
            json.dump(geojson, f, indent=2)
    
    elif output_format == 'csv':
        print("  Creating CSV table...")
        with open(output_file, 'w', newline='') as f:
            fieldnames = ['name', 'source', 'acquisitionDate', 'acquisitionTime', 
                         'centerX', 'centerY', 'polygon_area', 'area_units',
                         'crs', 'original_crs', 'geometry_wkt']
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            
            for fp in footprints:
                polygon = Polygon(fp['coordinates'])
                row = {
                    'name': fp['name'],
                    'source': fp['source'],
                    'acquisitionDate': fp['acquisitionDate'],
                    'acquisitionTime': fp['acquisitionTime'],
                    'centerX': fp['centerX'],
                    'centerY': fp['centerY'],
                    'polygon_area': fp.get('polygon_area', 0),
                    'area_units': fp.get('area_units', 'unknown'),
                    'crs': fp.get('crs', 'unknown'),
                    'original_crs': fp.get('original_crs', ''),
                    'geometry_wkt': polygon.wkt
                }
                writer.writerow(row)
    
    print(f"\n √ Successfully exported {len(footprints)} footprints to:")
    print(f"  {output_file}")
    print("="*80)
