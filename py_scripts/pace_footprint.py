# A script to extract PACE OCI satellite footprints from downloaded netCDF files in a folder

import os
import json
import warnings
from pathlib import Path
import netCDF4 as nc
import numpy as np
from matplotlib.figure import Figure # need to calculate footprint boundary
from shapely.geometry import Polygon, MultiPolygon, mapping
from shapely.validation import make_valid

# Suppress runtime warnings
warnings.filterwarnings('ignore')

def extract_pace_footprint(pace_file, idx, total, verbose=True):
    filename = os.path.basename(pace_file)
    print(f"\n[{idx}/{total}] Processing: {filename}")
    print("-" * 80)
    
    ds = None
    try:
        if verbose: print(f"  Opening file...")
        ds = nc.Dataset(pace_file, 'r')
        
        # --- Store the Metadata as output ---
        time_start = None
        for attr in ['time_coverage_start', 'startTime', 'time_start']:
            if hasattr(ds, attr):
                time_start = getattr(ds, attr)
                break
        if verbose: print(f"  Acquisition time: {time_start}")

        # --- Location Data ---
        lat = None
        lon = None
        
        if 'navigation_data' in ds.groups:
            grp = ds.groups['navigation_data']
            lat = grp.variables['latitude'][:]
            lon = grp.variables['longitude'][:]
        elif 'geolocation_data' in ds.groups:
            grp = ds.groups['geolocation_data']
            lat = grp.variables['latitude'][:]
            lon = grp.variables['longitude'][:]
        else:
            print(f"ERROR: Could not find navigation/geolocation group")
            return None

        # --- Make a mask of valid data ignoring fill values ---
        lat_fill = -999.0
        if hasattr(ds.groups.get('navigation_data', ds).variables.get('latitude', object()), '_FillValue'):
            lat_fill = ds.groups['navigation_data'].variables['latitude']._FillValue
            
        # Create binary mask (1=valid, 0=invalid)
        valid_mask = ((lat != lat_fill) & np.isfinite(lat) & np.isfinite(lon)).astype(int)
        
        valid_count = np.sum(valid_mask)
        total_count = valid_mask.size
        if verbose: 
            print(f"  Valid coordinates: {valid_count}/{total_count} ({100*valid_count/total_count:.1f}%)")

        if valid_count == 0:
            print("ERROR: No valid coordinates found")
            return None

        # --- PADDING needed to ensure contour finds a closed loop ---
        padded_mask = np.pad(valid_mask, pad_width=1, mode='constant', constant_values=0)

        if verbose: print(f"  Tracing footprint boundary...")
        
        # --- Contouring ---
        fig = Figure()
        ax = fig.add_subplot(111)
        cntr = ax.contour(padded_mask, levels=[0.5])
        
        polygons = []
        
        if len(cntr.allsegs) > 0:
            for polygon_coords in cntr.allsegs[0]:
                if len(polygon_coords) < 3: continue
                
                # Adjust coordinates back (subtract 1 because of padding)
                cols = polygon_coords[:, 0] - 1
                rows = polygon_coords[:, 1] - 1
                
                # Clip to original array bounds
                cols = np.clip(cols.astype(int), 0, lat.shape[1]-1)
                rows = np.clip(rows.astype(int), 0, lat.shape[0]-1)
                
                path_lats = lat[rows, cols]
                path_lons = lon[rows, cols]
                
                if path_lons.size == 0: continue

                # Check for Antimeridian Wrap)
                if np.ptp(path_lons) > 180:
                    path_lons = np.where(path_lons < 0, path_lons + 360, path_lons)
                
                poly_coords = list(zip(path_lons, path_lats))
                if len(poly_coords) >= 3:
                    polygons.append(Polygon(poly_coords))

        del ax
        del fig

        if not polygons:
            print("ERROR: Could not trace valid footprint (polygons list empty)")
            return None
            
        if len(polygons) == 1:
            geometry = polygons[0]
        else:
            geometry = MultiPolygon(polygons)

        geometry = geometry.simplify(0.01, preserve_topology=True)
        if not geometry.is_valid:
            geometry = make_valid(geometry)
        centroid = geometry.centroid
        
        if verbose:
            print(f"✓ Extracted polygon with {len(polygons)} parts")
            print(f"✓ Scene {idx}/{total} completed successfully")

        metadata = {
            'name': filename.replace('.nc', ''),
            'source': 'PACE_OCI',
            'acquisitionDate': time_start,
            'centerLatitude': float(centroid.y),
            'centerLongitude': float(centroid.x),
            'geometry': mapping(geometry)
        }
        
        return metadata

    except Exception as e:
        print(f"ERROR processing file: {str(e)}")
        return None
    finally:
        if ds:
            ds.close()

def process_pace_folder(folder_path, output_format='geojson'):
    folder = Path(folder_path)
    
    # Check if folder exists
    if not folder.exists():
        print(f"ERROR: The folder '{folder_path}' does not exist.")
        print("Please edit the 'pace_folder' variable at the bottom of the script.")
        return

    patterns = ['*.nc', '*OCI*.nc', '*L2*.nc']
    pace_files = []
    for p in patterns:
        pace_files.extend(list(folder.glob(p)))
    pace_files = list(set(pace_files))
    pace_files.sort()
    
    total_files = len(pace_files)
    
    if total_files == 0:
        print(f"No PACE files found in {folder_path}")
        return None

    print("="*80)
    print(f"Found {total_files} files to process")
    print("="*80)

    footprints = []
    success_count = 0
    fail_count = 0

    for i, pace_file in enumerate(pace_files, 1):
        result = extract_pace_footprint(str(pace_file), i, total_files)
        if result:
            footprints.append(result)
            success_count += 1
        else:
            fail_count += 1

    print("\n" + "="*80)
    print("PROCESSING COMPLETE")
    print("="*80)
    print(f"Successful: {success_count}/{total_files}")

    if not footprints:
        return None

    output_file = folder / f"pace_footprints.{output_format}"
    print(f"\nExporting {len(footprints)} footprints to {output_format.upper()}...")
    
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
        'features': features
    }
    
    with open(output_file, 'w') as f:
        json.dump(geojson, f, indent=2)

    print(f"✓ Successfully exported to: {output_file}")
    return str(output_file)

if __name__ == "__main__":
    # USER SETTING: Change this path to your data location
    pace_folder = "/pace_oci" 
    
    print(f"Looking for data in: {pace_folder}")
    process_pace_folder(pace_folder)
