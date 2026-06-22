#!/usr/bin/env python3
"""
DESIS to Sentinel-2 (S2 A/B/C) & Landsat 8/9  Vegetation Index Extraction

- Wavelength range: 401-1000 nm (VNIR only, no SWIR).
- PSRI (500/680/750 nm), REIP (670/700/740/780 nm), CIre (705/783 nm).
- PSRI extracted but not active in analysis because of large bandpass differences
- Scene-level QA via QL_QUALITY-2.tif
- Per-band QL_QUALITY is used for diagnostic logging only, not used for pixel masking.

LANDSAT NOTES:
- No red-edge band (178 nm red/NIR gap): REIP and CIre are NaN
- QA_PIXEL Clear bit (bit 6) used for cloud masking
- L8 + L9 Collection 2 Level-2 SR merged in the same two-pass search
- rsr_base must additionally contain 'landsat8_rsr_csv' and
  'landsat9_rsr_csv' subfolders

Usage:
  python s1_desis_s2_landsat_v1.py
      --desis_dir    /path/to/DESIS/scenes
      --rsr_base     /path/to/RSR/csvs (must contain s2a/b/c*.csv AND landsat8_rsr_csv/, landsat9_rsr_csv/)
      --nvis         /path/to/nvis_mvg.tif
      --out          ./outputs/desis_s2_landsat_v1.csv
"""

import os, sys, glob, argparse, warnings, re, traceback, math
from pathlib import Path
from datetime import datetime, timedelta
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor, as_completed
import xml.etree.ElementTree as ET

import numpy as np
import pandas as pd
import rasterio
from rasterio.warp import reproject, Resampling as RioResampling
from rasterio.transform import array_bounds
from rasterio import Affine
from pyproj import Transformer

warnings.filterwarnings('ignore')


# LOGGER
# =============================================================================
class Tee:
    def __init__(self, *files): self.files = files
    def write(self, obj):
        for f in self.files: f.write(obj); f.flush()
    def flush(self):
        for f in self.files: f.flush()

LOG_FILE = './outputs/processing_log_desis_s2_landsat_v1.txt'

# CONFIG
# =============================================================================
SENSOR_NAME = 'DESIS'

S2_BAND_MAP = {
    'B2':'blue','B3':'green','B4':'red',
    'B5':'re1','B6':'re2','B7':'re3',
    'B8':'nir','B8A':'nir_narrow',
    'B11':'swir1','B12':'swir2',
}
S2_VEG_BAND_CODES = ['B2','B3','B4','B5','B6','B7','B8','B8A','B11','B12']
S2_VEG_NAMES      = [S2_BAND_MAP[c] for c in S2_VEG_BAND_CODES]

# Sentinel-2 band groups by native resolution (ESA L2A product spec).
S2_BANDS_10M = ['B2','B3','B4','B8']
S2_BANDS_20M = ['B5','B6','B7','B8A','B11','B12']

# Per-index S2 native resolution, derived from the band groups above.
S2_NATIVE_RES_M = {'NDVI':10,'EVI2':10,'SAVI':10,'PSRI':10,
                   'REIP':20,'NDMI':20,'CIre':20}

INDICES = ['NDVI','EVI2','SAVI','PSRI','REIP','NDMI','CIre']

# DESIS per-band critical quality bits (diagnostic only - not used for pixel masking)
CRITICAL_BITS = 0b01110011


# This is a vegetation-vs-non-vegetation filter, not a stratification filter.
# Excluded codes:
# 0 : null / raster nodata
# 24: Inland water
# 25: Cleared, non-native vegetation, buildings / roads
# 27: Naturally bare / sparsely vegetated (bare rock, sand)
# 28: Seas and estuaries
# 99: NVIS no-data / unclassified

NVIS_EXCLUDE_MVG = frozenset({0, 24, 25, 27, 28, 99})

# S2 SCL clear classes
S2_CLEAR_SCL = {4, 5, 6} # joint mask (6=water excluded by NVIS anyway)
S2_CLEAR_LAND_SCL = {4, 5} # f_clear scoring (vegetation land only)


# --- Landsat 8/9 config (v6) --------------------------------------------
# OLI surface reflectance band map (Collection 2 Level-2).
# DESIS has no SWIR to convolve.
# No red-edge band exists on Landsat at all (red B4 ~655nm, NIR B5 ~865nm
# REIP/CIre are therefore structurally impossible against this reference.
LS_BAND_MAP = {'B2': 'blue', 'B3': 'green', 'B4': 'red', 'B5': 'nir'}
LS_VEG_BAND_CODES = ['B2', 'B3', 'B4', 'B5']
LS_SR_BAND_CODES  = ['SR_B2', 'SR_B3', 'SR_B4', 'SR_B5']
LS_SR_TO_BAND_MAP = {'SR_B2': 'blue', 'SR_B3': 'green', 'SR_B4': 'red', 'SR_B5': 'nir'}
LS_SR_SCALE  = 0.0000275 # USGS Collection 2 Level-2 SR scale factor
LS_SR_OFFSET = -0.2 # USGS Collection 2 Level-2 SR additive offset
LS_INDICES   = ['NDVI', 'EVI2', 'SAVI', 'PSRI']

# Maps Landsat RSR CSV filenames to OLI band codes.
RSR_FILE_TO_BAND = {
    'coastalaerosol_rsr.csv': 'B1',
    'blue_rsr.csv':           'B2',
    'green_rsr.csv':          'B3',
    'red_rsr.csv':            'B4',
    'nir_rsr.csv':            'B5',
    'swir1_rsr.csv':          'B6',
    'swir2_rsr.csv':          'B7',
    'cirrus_rsr.csv':         'B9',
    'pan_rsr.csv':            'B8',
}

# Landsat C2L2 QA_PIXEL bit 6 = "Clear" (already excludes cloud/shadow/snow/cirrus per the official bit definition)
QA_PIXEL_CLEAR_BIT = 1 << 6

# 60 m aggregation: 2x2 pixel block
AGG60_OFFSETS = [(0,0),(0,1),(1,0),(1,1)]

# Spatial sampling parameters
MIN_PIXELS = 100 # minimum pixels required to retain a scene
MIN_F_CLEAR_VEG = 0.30 # minimum acceptable fraction of clear veg pixels for scene acceptance search before random pixel selection
MIN_DIST_DEFAULT_M = 300 # primary minimum sampling distance to avoid spatial autocorrelation
MIN_DIST_FALLBACK_M = 200 # fallback thinning distance


# DESIS READER
# =============================================================================
def read_desis(filepath):
    filepath = Path(filepath)
    prefix   = filepath.name.split('-SPECTRAL_IMAGE')[0]
    parent   = filepath.parent

    meta_file        = next(parent.glob(f'{prefix}-*METADATA.xml'), None)
    ql_files         = [p for p in parent.glob(f'{prefix}-*QL_QUALITY.tif')
                        if '-QL_QUALITY-2' not in p.name]
    ql_quality_file  = ql_files[0] if ql_files else None
    ql_quality2_file = next(parent.glob(f'{prefix}-*QL_QUALITY-2.tif'), None)

    wvl = None
    if meta_file and meta_file.exists():
        tree         = ET.parse(str(meta_file))
        wvl_elements = tree.getroot().findall('.//wavelengthCenterOfBand')
        if wvl_elements:
            wvl = np.array([float(e.text) for e in wvl_elements])
            print(f'  [DESIS] Read {len(wvl)} wavelengths from XML')

    with rasterio.open(str(filepath)) as src:
        refl_brc  = src.read().astype(np.float32)
        transform = src.transform
        crs_str   = str(src.crs)

    background_mask  = np.any(refl_brc == -32768, axis=0)
    refl_brc        *= 0.0001

    if wvl is None or len(wvl) != refl_brc.shape[0]:
        print('  [DESIS] Fallback linspace wavelengths (401-1000 nm)')
        wvl = np.linspace(401, 1000, refl_brc.shape[0])

    refl        = np.transpose(refl_brc, (1, 2, 0))   # (rows, cols, bands)
    nodata_mask = background_mask.copy()
    rows, cols  = nodata_mask.shape

    # Tier 1 QL_QUALITY-2.tif scene classification
    if ql_quality2_file and ql_quality2_file.exists():
        with rasterio.open(str(ql_quality2_file)) as q2:
            if q2.count < 8:
                print(f'  [DESIS] WARNING: QL_QUALITY-2 has {q2.count} layers')
            land_clear  = q2.read(2); water_clear = q2.read(8)
            shadow      = q2.read(1); snow        = q2.read(3)
            haze_land   = q2.read(4); haze_water  = q2.read(5)
            cloud_land  = q2.read(6); cloud_water = q2.read(7)
        clear_px = (land_clear == 1) | (water_clear == 1)
        contam   = ((cloud_land==1)|(cloud_water==1)|(haze_land==1)|
                    (haze_water==1)|(shadow==1)|(snow==1))
        ql2_mask = ~clear_px | contam
        nodata_mask |= ql2_mask
        n_clear = int(np.sum(clear_px & ~contam))
        print(f'  [DESIS] QL_QUALITY-2: {n_clear} clear px | '
              f'{int(ql2_mask.sum())} masked ({100*ql2_mask.sum()/ql2_mask.size:.1f}%)')
    else:
        print('  [DESIS] QL_QUALITY-2.tif not found - skipping scene QA')

    # Tier 2 per-band QL_QUALITY (logged only, not used for masking)
    if ql_quality_file and ql_quality_file.exists():
        with rasterio.open(str(ql_quality_file)) as qsrc:
            qdata = qsrc.read()
        bq     = (qdata & CRITICAL_BITS) > 0
        n_any  = int(np.any(bq, axis=0).sum())
        frac   = float(np.mean(bq)) * 100
        n_dead = int(np.any((qdata & 0b00000001) > 0, axis=0).sum())
        print(f'  [DESIS] QL_QUALITY (diagnostic): {frac:.1f}% band-px flagged | '
              f'{n_any}/{rows*cols} px with >=1 bad band | Dead px={n_dead}')
        del qdata, bq
    else:
        print('  [DESIS] QL_QUALITY.tif not found')

    tot = int(nodata_mask.sum())
    print(f'  [DESIS] {len(wvl)} bands {wvl[0]:.1f}-{wvl[-1]:.1f} nm | '
          f'shape ({rows},{cols}) | masked {tot}/{nodata_mask.size} '
          f'({100*tot/nodata_mask.size:.1f}%)')

    refl[nodata_mask, :] = np.nan
    return wvl, refl, transform, crs_str, nodata_mask


# NVIS mask for vege pixels
# =============================================================================
def load_nvis_for_scene(nvis_path, scene_transform, scene_crs, scene_shape):
    rows, cols = scene_shape
    dst = np.zeros((rows, cols), dtype=np.int16)
    with rasterio.open(nvis_path) as src:
        reproject(source=rasterio.band(src, 1), destination=dst,
                  src_transform=src.transform, src_crs=src.crs,
                  dst_transform=scene_transform, dst_crs=scene_crs,
                  resampling=RioResampling.nearest, dst_nodata=0)
    return dst

def build_veg_mask(nvis_arr):
    """Return True for pixels that are vegetation (i.e. NOT in the exclusion set).
    Excludes: water, bare ground, impervious/non-native, and NVIS no-data codes.
    Retains all vegetated MVGs including MVG 29 (Regrowth) - see NVIS_EXCLUDE_MVG.
    """
    return ~np.isin(nvis_arr, list(NVIS_EXCLUDE_MVG))


# GEE INITIALISATION
# =============================================================================
def init_gee():
    try:
        import ee
        try: ee.Initialize(opt_url='https://earthengine-highvolume.googleapis.com')
        except Exception: ee.Initialize()
        print('[INFO] GEE initialised'); return True
    except Exception as e:
        print(f'[ERROR] GEE init failed: {e}'); return False


# BOUNDING BOX derived from sensor-QA-clear NVIS veg pixels
# =============================================================================
def get_clear_veg_bbox(sensor_clear_veg_mask, transform, scene_crs):
    """
    Derive bounding box from sensor-QA-clear NVIS vegetation pixels.

    This is the search region passed to GEE. Using clear-veg pixels (not the full raster extent)
    prevents querying S2 over ocean, non-vegetated land, and
    cloud-masked areas that can never contribute valid paired measurements.

    Returns (long, lats) as lists for use with min()/max(), or (None, None) if
    no clear veg pixels exist (scene should be skipped before calling GEE).
    """
    rows_v, cols_v = np.where(sensor_clear_veg_mask)
    if len(rows_v) == 0:
        return None, None
    xs   = transform.c + (cols_v + 0.5) * transform.a
    ys   = transform.f + (rows_v + 0.5) * transform.e
    t    = Transformer.from_crs(scene_crs, 'EPSG:4326', always_xy=True)
    lons, lats = t.transform(xs, ys)
    return list(lons), list(lats)


# S2 SCL FETCH
# =============================================================================
def fetch_scl_array(asset_id, scene_transform, scene_shape, scene_crs):
    import ee, urllib.request, io
    rows, cols = scene_shape
    b = array_bounds(rows, cols, scene_transform)
    t = Transformer.from_crs(scene_crs, 'EPSG:4326', always_xy=True)
    lons, lats = t.transform([b[0],b[2],b[0],b[2]], [b[1],b[1],b[3],b[3]])
    region = ee.Geometry.Rectangle([min(lons)-0.01, min(lats)-0.01,
                                     max(lons)+0.01, max(lats)+0.01])
    url = ee.Image(asset_id).select('SCL').getDownloadURL(
        {'region':region, 'scale':20, 'format':'GEO_TIFF', 'crs':scene_crs})
    with urllib.request.urlopen(url) as resp: data = resp.read()
    with rasterio.open(io.BytesIO(data)) as src:
        scl_full = src.read(1).astype(np.uint8)
        scl_t    = src.transform; scl_epsg = src.crs.to_epsg()
    out = np.zeros((rows, cols), dtype=np.uint8)
    reproject(source=scl_full, destination=out,
              src_transform=scl_t,
              src_crs=f'EPSG:{scl_epsg}' if scl_epsg else scene_crs,
              dst_transform=scene_transform, dst_crs=scene_crs,
              resampling=RioResampling.nearest, dst_nodata=0)
    return out


# f_clear computed over sensor-QA-clear NVIS veg pixels
# =============================================================================
def compute_ms_f_clear_s2(scl_arr, sensor_clear_veg_mask, t_half, days_diff):
    """
    Fraction of sensor-QA-clear NVIS veg pixels that are also clear in the S2 scene.

    Denominator: pixels where DESIS is clean and NVIS is veg.
    Numerator: those pixels that S2 also marks as clear (SCL 4 or 5).

    This is the fraction of pixels that could contribute valid paired measurements
    """
    n_denom = int(sensor_clear_veg_mask.sum())
    if n_denom == 0:
        return 0.0, 0.0
    n_clear = int((np.isin(scl_arr, list(S2_CLEAR_LAND_SCL)) & sensor_clear_veg_mask).sum())
    fc      = n_clear / n_denom
    return fc, fc * math.exp(-days_diff / t_half)


def build_joint_clear_mask_s2(nodata_mask, scl_arr):
    """Pixels clear in BOTH DESIS (Tier 1) and S2 (SCL)."""
    return (~nodata_mask) & np.isin(scl_arr, list(S2_CLEAR_SCL))


# S2 SCENE SELECTION
# =============================================================================
def find_best_s2_scene(sensor_date, clear_veg_lons, clear_veg_lats,
                        scene_transform, scene_shape, scene_crs,
                        sensor_clear_veg_mask,
                        t_half=14, max_days=15, max_cloud_pct=70, n_candidates=10):
    """
    Two-pass S2 scene selection.

    Pass 1: GEE metadata filter within clear-veg bbox. Coarse CLOUDY_PIXEL_PERCENTAGE
            pre-filter keeps only plausible scenes before the per-pixel SCL fetch.
    Pass 2: Fetch SCL for each candidate; compute f_clear over sensor_clear_veg_mask then select highest composite score.

    The bbox is derived from sensor-QA-clear NVIS veg pixels (not the full raster),
    so Pass 1 never queries S2 scenes that cover only ocean or cloud-contaminated areas.
    """
    import ee

    bbox = ee.Geometry.Rectangle([min(clear_veg_lons)-0.05, min(clear_veg_lats)-0.05,
                                   max(clear_veg_lons)+0.05, max(clear_veg_lats)+0.05])

    def _pass1(wd, qlabel):
        start = (sensor_date - timedelta(days=wd)).isoformat()
        end   = (sensor_date + timedelta(days=wd+1)).isoformat()
        col   = (ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
                 .filterDate(start, end).filterBounds(bbox)
                 .filter(ee.Filter.lte('CLOUDY_PIXEL_PERCENTAGE', max_cloud_pct)))
        ids   = col.aggregate_array('system:id').getInfo()
        if not ids:
            print(f'    Pass 1: no S2 scenes within +/-{wd}d'); return []
        candidates = []
        for aid in ids:
            img  = ee.Image(aid)
            sc   = img.get('SPACECRAFT_NAME').getInfo() or ''
            sens = ('S2A' if '2A' in sc else 'S2B' if '2B' in sc
                    else 'S2C' if '2C' in sc else 'S2X')
            sd   = datetime.utcfromtimestamp(
                       img.date().millis().getInfo()/1000).date()
            dd   = abs((sd - sensor_date).days)
            cld  = float(img.get('CLOUDY_PIXEL_PERCENTAGE').getInfo() or 100)
            candidates.append(dict(asset_id=aid, sensor=sens, date=sd,
                                   days_diff=dd, cloud_pct=cld,
                                   scene_quality=qlabel))
        candidates.sort(key=lambda x: (x['days_diff'], x['cloud_pct']))
        finalists = candidates[:n_candidates]
        print(f'    Pass 1: {len(ids)} scenes - top {len(finalists)}:')
        for c in finalists:
            print(f'      {c["sensor"]} {c["date"]} '
                  f'(d{c["days_diff"]:>2d}) cloud={c["cloud_pct"]:.1f}%')
        return finalists

    def _pass2(finalists):
        best = None
        for c in finalists:
            print(f'    Pass 2: {c["sensor"]} {c["date"]} (d{c["days_diff"]}d) ...',
                  end=' ', flush=True)
            try:
                scl      = fetch_scl_array(c['asset_id'], scene_transform,
                                            scene_shape, scene_crs)
                fc, sv   = compute_ms_f_clear_s2(scl, sensor_clear_veg_mask,
                                              t_half, c['days_diff'])
                print(f'f_clear_veg={fc:.3f} score={sv:.3f}')
                c.update({'f_clear_veg': fc, 'score': sv, 'scl_arr': scl})
                if best is None or sv > best['score']:
                    best = c
            except Exception as e:
                print(f'FAILED ({e})')
        return best

    finalists = _pass1(max_days, 'standard')
    if finalists:
        best = _pass2(finalists)
        if best is not None and best['f_clear_veg'] >= MIN_F_CLEAR_VEG:
            print(f'    -> {best["sensor"]} {best["date"]} '
                  f'd{best["days_diff"]}d f_clear_veg={best["f_clear_veg"]:.3f} '
                  f'score={best["score"]:.3f}')
            return best
        elif best is not None:
            print(f'    -> Best f_clear_veg={best["f_clear_veg"]:.3f} '
                  f'< {MIN_F_CLEAR_VEG} - expanding window')

    if max_days < 30:
        print(f'    Expanding to +/-30d...')
        finalists = _pass1(30, 'expanded')
        if finalists:
            best = _pass2(finalists)
            if best is not None and best['f_clear_veg'] >= MIN_F_CLEAR_VEG:
                print(f'    -> {best["sensor"]} {best["date"]} (expanded)')
                return best
            elif best is not None:
                print(f'    -> Best f_clear_veg={best["f_clear_veg"]:.3f} '
                      f'< {MIN_F_CLEAR_VEG} - no valid scene')

    print('    No usable S2 scene'); return None


# =============================================================================
# LANDSAT SCENE SELECTION
# This arm is deliberately NOT a triple-joint extension of the S2 search above
# it searches and scores Landsat scenes against DESIS's own QA-clear veg mask only, exactly the way the S2 search above does.
# The two arms are independent pairwise comparisons (DESIS-vs-S2, DESIS-vs-Landsat), not a single 3-way comparison
# =============================================================================
def fetch_qa_pixel_array(asset_id, scene_transform, scene_shape, scene_crs):
    """Download Landsat QA_PIXEL band for the DESIS scene footprint, reprojected onto the DESIS grid."""
    import ee, urllib.request, io
    rows, cols = scene_shape
    b = array_bounds(rows, cols, scene_transform)
    t = Transformer.from_crs(scene_crs, 'EPSG:4326', always_xy=True)
    lons, lats = t.transform([b[0], b[2], b[0], b[2]], [b[1], b[1], b[3], b[3]])
    region = ee.Geometry.Rectangle([min(lons)-0.01, min(lats)-0.01,
                                     max(lons)+0.01, max(lats)+0.01])
    url = ee.Image(asset_id).select('QA_PIXEL').getDownloadURL(
        {'region': region, 'scale': 30, 'format': 'GEO_TIFF', 'crs': scene_crs})
    with urllib.request.urlopen(url) as resp:
        data = resp.read()
    with rasterio.open(io.BytesIO(data)) as src:
        qa_full = src.read(1).astype(np.uint16)
        qa_t = src.transform; qa_epsg = src.crs.to_epsg()
    out = np.zeros((rows, cols), dtype=np.uint16)
    reproject(source=qa_full, destination=out,
              src_transform=qa_t,
              src_crs=f'EPSG:{qa_epsg}' if qa_epsg else scene_crs,
              dst_transform=scene_transform, dst_crs=scene_crs,
              resampling=RioResampling.nearest, dst_nodata=0)
    return out


def compute_ms_f_clear_landsat(qa_arr, sensor_clear_veg_mask, t_half, days_diff):
    """Fraction of DESIS-QA-clear NVIS veg pixels where the Landsat QA_PIXEL
    'Clear' bit (bit 6) is set. Same denominator convention same as the S2 arm
"""
    n_denom = int(sensor_clear_veg_mask.sum())
    if n_denom == 0:
        return 0.0, 0.0
    is_clear = (qa_arr.astype(np.uint32) & QA_PIXEL_CLEAR_BIT) != 0
    n_clear  = int((is_clear & sensor_clear_veg_mask).sum())
    fc       = n_clear / n_denom
    return fc, fc * math.exp(-days_diff / t_half)


def find_best_landsat_scene(sensor_date, clear_veg_lons, clear_veg_lats,
                            scene_transform, scene_shape, scene_crs,
                            sensor_clear_veg_mask,
                            t_half=14, max_days=15, max_cloud_pct=70, n_candidates=10):
    """
    Two-pass Landsat (L8+L9) scene selection identical design to find_best_s2_scene,
    using CLOUD_COVER for Pass 1 and QA_PIXEL for Pass 2.
    Searched against the SAME DESIS-QA-clear veg bbox/mask as the S2 search,
    but completely independently scored and selected.
    """
    import ee

    bbox = ee.Geometry.Rectangle([min(clear_veg_lons)-0.05, min(clear_veg_lats)-0.05,
                                   max(clear_veg_lons)+0.05, max(clear_veg_lats)+0.05])

    def _pass1(wd, qlabel):
        start = (sensor_date - timedelta(days=wd)).isoformat()
        end   = (sensor_date + timedelta(days=wd+1)).isoformat()
        l8    = (ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
                 .filterDate(start, end).filterBounds(bbox)
                 .filter(ee.Filter.lte('CLOUD_COVER', max_cloud_pct)))
        l9    = (ee.ImageCollection('LANDSAT/LC09/C02/T1_L2')
                 .filterDate(start, end).filterBounds(bbox)
                 .filter(ee.Filter.lte('CLOUD_COVER', max_cloud_pct)))
        ids   = l8.merge(l9).aggregate_array('system:id').getInfo()
        if not ids:
            print(f'    Landsat Pass 1: no scenes within +/-{wd}d'); return []
        candidates = []
        for aid in ids:
            img  = ee.Image(aid)
            sens = 'L8' if 'LC08' in aid else 'L9'
            sd   = datetime.utcfromtimestamp(
                       img.date().millis().getInfo()/1000).date()
            dd   = abs((sd - sensor_date).days)
            cld  = float(img.get('CLOUD_COVER').getInfo() or 100)
            candidates.append(dict(asset_id=aid, sensor=sens, date=sd,
                                   days_diff=dd, cloud_pct=cld,
                                   scene_quality=qlabel))
        candidates.sort(key=lambda x: (x['days_diff'], x['cloud_pct']))
        finalists = candidates[:n_candidates]
        print(f'    Landsat Pass 1: {len(ids)} scenes - top {len(finalists)}:')
        for c in finalists:
            print(f'      {c["sensor"]} {c["date"]} '
                  f'(d{c["days_diff"]:>2d}) cloud={c["cloud_pct"]:.1f}%')
        return finalists

    def _pass2(finalists):
        best = None
        for c in finalists:
            print(f'    Landsat Pass 2: {c["sensor"]} {c["date"]} (d{c["days_diff"]}d) ...',
                  end=' ', flush=True)
            try:
                qa       = fetch_qa_pixel_array(c['asset_id'], scene_transform,
                                                scene_shape, scene_crs)
                fc, sv   = compute_ms_f_clear_landsat(qa, sensor_clear_veg_mask,
                                                      t_half, c['days_diff'])
                print(f'f_clear_veg={fc:.3f} score={sv:.3f}')
                c.update({'f_clear_veg': fc, 'score': sv, 'qa_arr': qa})
                if best is None or sv > best['score']:
                    best = c
            except Exception as e:
                print(f'FAILED ({e})')
        return best

    finalists = _pass1(max_days, 'standard')
    if finalists:
        best = _pass2(finalists)
        if best is not None and best['f_clear_veg'] >= MIN_F_CLEAR_VEG:
            print(f'    -> {best["sensor"]} {best["date"]} '
                  f'd{best["days_diff"]}d f_clear_veg={best["f_clear_veg"]:.3f} '
                  f'score={best["score"]:.3f}')
            return best
        elif best is not None:
            print(f'    -> Best f_clear_veg={best["f_clear_veg"]:.3f} '
                  f'< {MIN_F_CLEAR_VEG} - expanding window')

    if max_days < 30:
        print(f'    Expanding Landsat search to +/-30d...')
        finalists = _pass1(30, 'expanded')
        if finalists:
            best = _pass2(finalists)
            if best is not None and best['f_clear_veg'] >= MIN_F_CLEAR_VEG:
                print(f'    -> {best["sensor"]} {best["date"]} (expanded)')
                return best
            elif best is not None:
                print(f'    -> Best f_clear_veg={best["f_clear_veg"]:.3f} '
                      f'< {MIN_F_CLEAR_VEG} - no valid scene')

    print('    No usable Landsat scene'); return None


def build_joint_clear_mask_landsat(nodata_mask, qa_arr):
    """Pixels clear in BOTH DESIS (Tier 1) and Landsat (QA_PIXEL Clear bit).
    Independent of build_joint_clear_mask_s2 - not AND"""
    is_clear = (qa_arr.astype(np.uint32) & QA_PIXEL_CLEAR_BIT) != 0
    return (~nodata_mask) & is_clear


# =============================================================================
# SPATIAL SAMPLING - greedy thinning
# =============================================================================
def pixel_latlon(row, col, transform, crs):
    x = transform.c + (col + 0.5) * transform.a
    y = transform.f + (row + 0.5) * transform.e
    lon, lat = Transformer.from_crs(crs, 'EPSG:4326', always_xy=True).transform(x, y)
    return float(lat), float(lon)


def greedy_spatial_thin(rows_e, cols_e, transform, min_dist_m, max_pixels, rng):
    """
    Randomly shuffles eligible pixels and accepts each one only if it is
    at least min_dist_m from all already-accepted pixels. Stops at max_pixels.
    Returns an index array into rows_e/cols_e. Length <= max_pixels.
    k is capped at 100 and the loop terminates early once k is reached.
    """
    n = len(rows_e)
    if n == 0:
        return np.array([], dtype=int)

    xs     = transform.c + (cols_e + 0.5) * transform.a
    ys     = transform.f + (rows_e + 0.5) * transform.e
    coords = np.column_stack([xs, ys])
    order  = rng.permutation(n)

    selected, sel_coords = [], []
    for i in order:
        if len(selected) >= max_pixels:
            break
        xy = coords[i]
        if sel_coords:
            sc    = np.array(sel_coords)
            dists = np.sqrt(np.sum((sc - xy) ** 2, axis=1))
            if dists.min() < min_dist_m:
                continue
        selected.append(i)
        sel_coords.append(xy)

    return np.array(selected, dtype=int)


def sample_pixels_thinned(nvis_arr, eligible_mask, transform, scene_crs,
                           max_pixels, random_seed, scene_idx, arm,
                           min_dist_primary=MIN_DIST_DEFAULT_M,
                           min_dist_fallback=MIN_DIST_FALLBACK_M):
    """
    Select up to max_pixels spatially independent pixels from the eligible pool.

    Two-attempt spatial thinning:
      Attempt 1: min_dist_primary (default 300 m)
      Attempt 2: min_dist_fallback (default 200 m)

    Scene is dropped (returns empty DataFrame, None) if < max_pixels thinned pixels are available at min_dist_fallback.

    Returns (pixels_df, actual_min_dist_m) or (empty_df, None).
    """
    rows_e, cols_e = np.where(eligible_mask)
    n_el           = len(rows_e)

    if n_el == 0:
        print(f'  [SAMPLE-{arm}] No eligible pixels in joint clear & veg mask')
        return pd.DataFrame(), None

    print(f'  [SAMPLE-{arm}] {n_el} eligible pixels before thinning')

    sel          = np.array([], dtype=int)
    actual_dist  = None

    for min_dist_m in [min_dist_primary, min_dist_fallback]:
        rng_attempt = np.random.default_rng(random_seed) # same seed each attempt
        sel         = greedy_spatial_thin(rows_e, cols_e, transform,
                                          min_dist_m, max_pixels, rng_attempt)
        n_thinned   = len(sel)
        print(f'  [SAMPLE-{arm}] {min_dist_m} m thinning → {n_thinned} pixels')
        if n_thinned >= max_pixels:
            actual_dist = min_dist_m
            break

    if actual_dist is None:
        print(f'  [SAMPLE-{arm}] Scene dropped: {len(sel)} pixels at '
              f'{min_dist_fallback} m < {max_pixels} required')
        return pd.DataFrame(), None

    records = []
    for k, i in enumerate(sel, 1):
        r, c       = int(rows_e[i]), int(cols_e[i])
        lat, lon   = pixel_latlon(r, c, transform, scene_crs)
        records.append({
            'pixel_id':      f'S{scene_idx:02d}_{arm}_px{k:04d}',
            'nvis_mvg_code': int(nvis_arr[r, c]),
            'pixel_row':     r,
            'pixel_col':     c,
            'pixel_lat':     round(lat, 7),
            'pixel_lon':     round(lon, 7),
        })

    df = pd.DataFrame(records)
    print(f'  [SAMPLE-{arm}] Retained {len(df)} pixels at {actual_dist} m spacing')
    return df, actual_dist


# SPECTRAL PROCESSING
# =============================================================================
def get_band_1d(spectrum, waves, target_nm, tol=15):
    idx = np.argmin(np.abs(waves - target_nm))
    if np.abs(waves[idx] - target_nm) > tol: return np.nan, True
    return float(spectrum[idx]), False


def compute_native_indices(spectrum, waves):
    """DESIS native VIs."""
    def _g(t, tol=15): return get_band_1d(spectrum, waves, t, tol)
    red,  br  = _g(665);  nir,  bn  = _g(842)
    r500, b50 = _g(500);  r680, b68 = _g(680); r750, b75 = _g(750)
    r670, b67 = _g(670);  r700, b70 = _g(700)
    r740, b74 = _g(740);  r780, b78 = _g(780)
    r705, b705 = _g(705); r783, b783 = _g(783)
    eps = 1e-8
    def _s(n, d, bn, bd): return np.nan if (bn or bd or abs(d)<eps) else float(n/d)
    ndvi = _s(nir-red, nir+red+eps, bn, br)
    evi2 = np.nan if (bn or br) else float(2.5*(nir-red)/(nir+2.4*red+1+eps))
    savi = np.nan if (bn or br) else float(1.5*(nir-red)/(nir+red+0.5+eps))
    psri = _s(r680-r500, r750+eps, b68|b50, b75)
    ndmi = np.nan
    reip = np.nan
    if not(b67 or b70 or b74 or b78):
        den = r740 - r700
        if abs(den) > 1e-8:
            reip = float(700 + 40*((r670+r780)/2 - r700) / den)
    # CIre = (783 / 705) − 1
    # denominator (705 nm) must be non-zero; no minimum floor to avoid introducing bias
    # Tier 3 bounds in the analysis script catch physiologically impossible values.
    cire = np.nan
    if not(b705 or b783) and abs(r705) > eps:
        cire = float(r783 / r705 - 1)
    return {'NDVI':ndvi,'EVI2':evi2,'SAVI':savi,'PSRI':psri,
            'REIP':reip,'NDMI':ndmi,'CIre':cire}


def convolve_1d(spectrum, waves, srf_df, band_codes):
    """Convolve 1D DESIS spectrum to S2-equivalent bands."""
    out = {}
    for code in band_codes:
        if code not in srf_df.columns: out[code] = np.nan; continue
        srf   = np.interp(waves, srf_df['WAVELENGTH'].values,
                          srf_df[code].values, left=0.0, right=0.0)
        valid = ~np.isnan(spectrum) & (srf > 0)
        if not valid.any(): out[code] = np.nan; continue
        w, s, r = waves[valid], spectrum[valid], srf[valid]
        tot = np.trapz(r, w)
        if tot < 1e-10: out[code] = np.nan; continue
        out[code] = float(np.trapz(s*r, w) / tot)
    return out


def conv_bands_to_indices(cb):
    """Vegetation indices from convolved band dictionary. NDMI = NaN for DESIS (no SWIR).
    CIre uses re1 (S2 B5 ~705 nm) and re3 (S2 B7 ~783 nm)"""
    eps = 1e-8; out = {}
    def _v(k): x = cb.get(k); return x if (x is not None and not np.isnan(x)) else None
    red = _v('red'); nir = _v('nir'); green = _v('green')
    re1 = _v('re1'); re2 = _v('re2'); re3   = _v('re3'); sw1 = _v('swir1')
    if red is None or nir is None: return out
    out['NDVI'] = float((nir-red)/(nir+red+eps))
    out['EVI2'] = float(2.5*(nir-red)/(nir+2.4*red+1+eps))
    out['SAVI'] = float(1.5*(nir-red)/(nir+red+0.5+eps))
    if green is not None: out['PSRI'] = float((red-green)/(nir+eps))
    if all(x is not None for x in [re1, re2, re3]):
        den = re2-re1
        if abs(den) > 1e-8: out['REIP'] = float(700+40*((red+re3)/2-re1)/den)
    if sw1 is not None: out['NDMI'] = float((nir-sw1)/(nir+sw1+eps))
    # CIre = (re3 / re1) − 1  →  (B7 ~783 nm) / (B5 ~705 nm) − 1
    if re1 is not None and re3 is not None and abs(re1) > eps:
        out['CIre'] = float(re3 / re1 - 1)
    return out


def get_60m_spectrum(refl, nodata_mask, row, col):
    """2x2 block mean spectrum for 60 m aggregation."""
    rt, ct, nb = refl.shape; specs = []
    for dr, dc in AGG60_OFFSETS:
        r2, c2 = row+dr, col+dc
        if 0<=r2<rt and 0<=c2<ct and not nodata_mask[r2, c2]:
            specs.append(refl[r2, c2, :])
    n = len(specs)
    if n == 0: return np.full(nb, np.nan, np.float32), 0
    return np.nanmean(np.stack(specs), axis=0).astype(np.float32), n

# S2 RSR LOADING
# =============================================================================
def load_s2_rsr(rsr_path):
    df  = pd.read_csv(rsr_path)
    wc  = next((c for c in df.columns if 'wl' in c.lower()), df.columns[0])
    mg  = df[[wc]].copy(); mg.columns = ['WAVELENGTH']
    for col in df.columns:
        if col == wc: continue
        m = re.search(r'_B(\d+A?)$', col, re.IGNORECASE)
        if m: mg[f"B{m.group(1).upper()}"] = np.maximum(df[col].values, 0.0)
    for code in S2_VEG_BAND_CODES:
        if code not in mg.columns: mg[code] = 0.0
    mg = mg.sort_values('WAVELENGTH').reset_index(drop=True)
    for col in mg.columns:
        if col != 'WAVELENGTH':
            mg[col] = mg[col].interpolate('linear').fillna(0.0)
    print(f'  RSR: {Path(rsr_path).name} ({len(mg)} rows)'); return mg


# LANDSAT RSR LOADING
# =============================================================================
def load_landsat_rsr(rsr_dir):
    files = sorted(Path(rsr_dir).glob('*_RSR.csv'))
    if not files:
        raise FileNotFoundError(f"No *_RSR.csv files found in {rsr_dir}")
    merged = None
    for fpath in files:
        fname = fpath.name.lower()
        band_code = None
        for key, code in RSR_FILE_TO_BAND.items():
            if key in fname:
                band_code = code; break
        if band_code is None:
            print(f"  [WARN] Unknown RSR file {fpath.name}, skipping")
            continue
        df = pd.read_csv(fpath)
        wvl_col = [c for c in df.columns if 'wavelength' in c.lower()]
        wvl_col = wvl_col[0] if wvl_col else df.columns[0]
        resp_col = [c for c in df.columns if c.lower() not in (wvl_col.lower(), 'stdev')]
        if not resp_col:
            continue
        resp_col = resp_col[0]
        sub = df[[wvl_col, resp_col]].copy()
        sub.columns = ['WAVELENGTH', band_code]
        sub[band_code] = np.maximum(sub[band_code].values, 0.0)
        merged = sub if merged is None else pd.merge(merged, sub, on='WAVELENGTH', how='outer')
    merged = merged.sort_values('WAVELENGTH').reset_index(drop=True)
    for col in merged.columns:
        if col != 'WAVELENGTH':
            merged[col] = merged[col].interpolate(method='linear').fillna(0.0)
    print(f'  RSR: {Path(rsr_dir).name} ({len(merged)} rows)'); return merged


# GEE S2 EXTRACTION - 3 scales, parallel threads
# =============================================================================
def _mask_scl(image):
    import ee
    scl = image.select('SCL')
    return image.updateMask(scl.eq(4).Or(scl.eq(5)).Or(scl.eq(6)))

def _build_fc(pixels_df, buffer_m=None):
    import ee
    feats = []
    for _, r in pixels_df.iterrows():
        pt = ee.Geometry.Point(float(r['pixel_lon']), float(r['pixel_lat']))
        if buffer_m: pt = pt.buffer(buffer_m)
        feats.append(ee.Feature(pt, {'pixel_id': str(r['pixel_id'])}))
    return ee.FeatureCollection(feats)

def _parse_gee(info, expected_bands):
    """ Parse GEE reduceRegions output."""
    SCALE = 0.0001; out = {}
    for feat in info['features']:
        p = feat['properties']; pid = p.get('pixel_id')
        if pid is None: continue
        rec = {}; ok = True
        for b in expected_bands:
            nm = S2_BAND_MAP[b]
            dn = p.get(b)
            if dn is not None: rec[nm] = float(dn)*SCALE
            else: ok = False; rec[nm] = None
        if ok: out[pid] = rec
    return out

def _extract_s2(asset_id, pixels_df, buffer_m, is_point, scale, bands):
    import ee
    img = ee.Image(asset_id) if is_point else _mask_scl(ee.Image(asset_id))
    red = ee.Reducer.first() if is_point else ee.Reducer.mean()
    fc  = _build_fc(pixels_df, buffer_m)
    return _parse_gee(img.select(bands).reduceRegions(
        collection=fc, reducer=red, scale=scale).getInfo(), bands)

def extract_s2_scales(asset_id, pixels_df):
    """
    Extract S2 SR at three comparison tiers - parallel GEE calls.

    'native': point sample. Each band group queried at its TRUE native scale:
      10m bands (B2/B3/B4/B8) at scale=10, 20m bands (B5/B6/B7/B8A/B11/B12) at scale=20.
      Results merged per pixel_id. This avoids the GEE artefact
      where point-sampling a 20m-native band at a finer nominal scale just
      returns the same 20m pixel value duplicated at adjacent point locations

    '30m': 15m buffer mean (~30m footprint), all bands together, scale=10.
    '60m': 30m buffer mean (~60m footprint), all bands together, scale=10.
    """
    tasks = {
        'native_10m_group': dict(buffer_m=None, is_point=True,  scale=10, bands=S2_BANDS_10M),
        'native_20m_group': dict(buffer_m=None, is_point=True,  scale=20, bands=S2_BANDS_20M),
        '30m':              dict(buffer_m=15,   is_point=False, scale=10, bands=S2_VEG_BAND_CODES),
        '60m':              dict(buffer_m=30,   is_point=False, scale=10, bands=S2_VEG_BAND_CODES),
    }
    results = {}
    with ThreadPoolExecutor(max_workers=4) as pool:
        futs = {pool.submit(_extract_s2, asset_id, pixels_df, **cfg): k
                for k, cfg in tasks.items()}
        for fut in as_completed(futs):
            k = futs[fut]
            try: results[k] = fut.result()
            except Exception as e:
                print(f'  [WARN] S2 {k} failed: {e}'); results[k] = {}

    # Merge native 10m-group + 20m-group results per pixel_id.
    # A pixel missing one group still keeps whatever the other group found
    native = {}
    for pid in set(results['native_10m_group']) | set(results['native_20m_group']):
        rec = {}
        rec.update(results['native_10m_group'].get(pid, {}))
        rec.update(results['native_20m_group'].get(pid, {}))
        native[pid] = rec

    return native, results['30m'], results['60m']


# GEE LANDSAT EXTRACTION
# =============================================================================
def _extract_landsat(asset_id, pixels_df, buffer_m, is_point, bands):
    import ee
    img = ee.Image(asset_id)
    red = ee.Reducer.first() if is_point else ee.Reducer.mean()
    fc  = _build_fc(pixels_df, buffer_m)
    info = img.select(bands).reduceRegions(
        collection=fc, reducer=red, scale=30).getInfo()
    out = {}
    for feat in info['features']:
        p = feat['properties']; pid = p.get('pixel_id')
        if pid is None: continue
        rec = {}; ok = True
        for b in bands:
            nm = LS_SR_TO_BAND_MAP[b]
            dn = p.get(b)
            if dn is not None: rec[nm] = float(dn) * LS_SR_SCALE + LS_SR_OFFSET
            else: ok = False; rec[nm] = None
        if ok: out[pid] = rec
    return out


def extract_landsat_scales(asset_id, pixels_df):
    """
    Extract Landsat C2L2 SR at two tiers, mirroring extract_s2_scales' 'native'/'60m' design.

    'native': point sample (Reducer.first()) at scale=30 res.
    '60m':    30m-radius buffer mean (Reducer.mean()), matching the 60m buffer convention used on the S2 side.
    """
    native = _extract_landsat(asset_id, pixels_df, buffer_m=None, is_point=True,
                              bands=LS_SR_BAND_CODES)
    s60m   = _extract_landsat(asset_id, pixels_df, buffer_m=30,   is_point=False,
                              bands=LS_SR_BAND_CODES)
    return native, s60m

# CPU PRE-PROCESSING
# =============================================================================
def preprocess_scene(dfile_str, nvis_path):
    res = {'file': dfile_str, 'error': None}
    try:
        wvl, refl, transform, crs, nd = read_desis(Path(dfile_str))
        shape = (refl.shape[0], refl.shape[1])
        nvis  = load_nvis_for_scene(nvis_path, transform, crs, shape)
        veg   = build_veg_mask(nvis)
        res.update({'wvl':wvl,'refl':refl,'transform':transform,'crs':crs,
                    'nd':nd,'nvis':nvis,'veg':veg})
    except Exception as e:
        res['error'] = f"{e}\n{traceback.format_exc()}"
    return res


def parse_desis_date(filepath):
    stem = Path(filepath).stem
    for part in stem.split('-'):
        if len(part) >= 8 and part[:4].isdigit() and part[4:6].isdigit():
            return datetime.strptime(part[:8], '%Y%m%d').date()
    raise ValueError(f"Cannot parse DESIS date: {stem}")


def _fmt(v):
    try: f = float(v); return round(f, 6) if np.isfinite(f) else None
    except (TypeError, ValueError): return None


# MAIN
# =============================================================================
def _process_s2_arm(scene_idx, dfile, d_date, refl, wvl, transform, crs, nd,
                     nvis, veg, sensor_clear_veg, cv_lons, cv_lats, rsr_map,
                     n_pixels, t_half, max_days, max_cloud_pct, n_candidates,
                     min_dist_m, min_dist_fallback_m, random_seed, skip_counts):
    """Independent S2 search & extraction pass for DESIS scenes.
    Returns a list of record dicts (possibly empty)."""
    rows, cols = refl.shape[:2]
    records = []

    print('  [S2] Searching nearest clearest S2 scene...')
    best = find_best_s2_scene(
        sensor_date=d_date,
        clear_veg_lons=cv_lons, clear_veg_lats=cv_lats,
        scene_transform=transform, scene_shape=(rows, cols),
        scene_crs=crs, sensor_clear_veg_mask=sensor_clear_veg,
        t_half=t_half, max_days=max_days,
        max_cloud_pct=max_cloud_pct, n_candidates=n_candidates)
    if best is None:
        print('  [S2] [SKIP] No valid S2 scene')
        skip_counts['no_s2_scene'] += 1
        return records

    jc       = build_joint_clear_mask_s2(nd, best['scl_arr'])
    eligible = jc & veg
    n_el     = int(eligible.sum())
    print(f'  [S2] Joint clear: {int(jc.sum())}/{jc.size} ({100*jc.mean():.1f}%) | '
          f'eligible (clear + veg): {n_el}')

    px_df, actual_dist = sample_pixels_thinned(
        nvis_arr=nvis, eligible_mask=eligible,
        transform=transform, scene_crs=crs,
        max_pixels=n_pixels, random_seed=random_seed,
        scene_idx=scene_idx, arm='S2',
        min_dist_primary=min_dist_m,
        min_dist_fallback=min_dist_fallback_m)
    if px_df.empty:
        skip_counts['insufficient_pixels_s2'] += 1
        return records

    s2_sensor = best['sensor']
    rsr       = rsr_map.get(s2_sensor, rsr_map['S2B'])
    px_data   = {}; n_bad = 0

    for _, px in px_df.iterrows():
        pid   = px['pixel_id']
        r, cc = int(px['pixel_row']), int(px['pixel_col'])
        spec  = refl[r, cc, :].copy()
        if np.all(np.isnan(spec)): n_bad += 1; continue

        nat = compute_native_indices(spec, wvl)
        if np.isnan(nat.get('NDVI', np.nan)): n_bad += 1; continue

        cb_raw = convolve_1d(spec, wvl, rsr, S2_VEG_BAND_CODES)
        cb     = {S2_BAND_MAP[k]: v for k, v in cb_raw.items()}
        ci     = conv_bands_to_indices(cb)

        s60, n60   = get_60m_spectrum(refl, nd, r, cc)
        cb60_raw   = convolve_1d(s60, wvl, rsr, S2_VEG_BAND_CODES)
        cb60       = {S2_BAND_MAP[k]: v for k, v in cb60_raw.items()}
        ci60       = conv_bands_to_indices(cb60)

        px_data[pid] = {'nat_idx': nat,
                        'conv_30m_bands': cb,   'conv_30m_idx': ci,
                        'conv_60m_bands': cb60, 'conv_60m_idx': ci60,
                        'n60': n60}

    print(f'  [S2] Convolved {len(px_data)}/{len(px_df)} | bad spectra skipped: {n_bad}')
    if not px_data:
        print('  [S2] [SKIP] No valid DESIS spectra')
        skip_counts['no_valid_spectra_s2'] += 1
        return records

    valid_df = px_df[px_df['pixel_id'].isin(px_data)].reset_index(drop=True)

    print(f'  [S2] Extracting S2 at {len(valid_df)} centroids [native/30m/60m]...')
    s2_native, s2_30m, s2_60m = extract_s2_scales(best['asset_id'], valid_df)

    n_miss = 0
    for _, px in valid_df.iterrows():
        pid = px['pixel_id']
        pd_ = px_data.get(pid)
        if pd_ is None: continue

        b_native = s2_native.get(pid)
        if not b_native or 'red' not in b_native or 'nir' not in b_native:
            n_miss += 1; continue

        rec = {
            'pixel_id':              pid,
            'sensor':                SENSOR_NAME,
            'reference_type':        'S2',
            'nvis_mvg_code':         px['nvis_mvg_code'],
            'pixel_row':             px['pixel_row'],
            'pixel_col':             px['pixel_col'],
            'pixel_lat':             px['pixel_lat'],
            'pixel_lon':             px['pixel_lon'],
            'hyperspectral_scene':   dfile.stem,
            'hyperspectral_date':    d_date.isoformat(),
            'ms_sensor':             s2_sensor,
            'ms_date':               best['date'].isoformat(),
            'ms_scene_id':           best['asset_id'],
            'days_diff':             best['days_diff'],
            'ms_cloud_pct':          round(best['cloud_pct'], 2),
            'ms_f_clear_veg':        round(best['f_clear_veg'], 4),
            'ms_score':              round(best['score'], 4),
            'scene_selection_quality': best['scene_quality'],
            't_half':                t_half,
            'actual_min_dist_m':     actual_dist,
            'n_pixels_sampled':      len(valid_df),
            'sensor_60m_n_px':       pd_['n60'],
            'ndmi_native_available': False,
        }

        # DESIS native indices
        for nm in INDICES:
            rec[f'DESIS_{nm}'] = _fmt(pd_['nat_idx'].get(nm))

        # DESIS convolved 30m (bands then indices)
        for nm in S2_VEG_NAMES:
            rec[f'DESIS_conv_30m_{nm}'] = _fmt(pd_['conv_30m_bands'].get(nm))
        for nm in INDICES:
            rec[f'DESIS_conv_30m_{nm}'] = _fmt(pd_['conv_30m_idx'].get(nm))

        # DESIS convolved 60m (bands then indices)
        for nm in S2_VEG_NAMES:
            rec[f'DESIS_conv_60m_{nm}'] = _fmt(pd_['conv_60m_bands'].get(nm))
        for nm in INDICES:
            rec[f'DESIS_conv_60m_{nm}'] = _fmt(pd_['conv_60m_idx'].get(nm))

        # S2 at native / 30m / 60m (bands then indices)
        for scale, sr_d in [('native', s2_native), ('30m', s2_30m), ('60m', s2_60m)]:
            b     = sr_d.get(pid)
            bvals = b if b else {}
            for nm in S2_VEG_NAMES:
                rec[f'S2_{scale}_{nm}'] = _fmt(bvals.get(nm))
            for nm in INDICES:
                rec[f'S2_{scale}_{nm}'] = _fmt(
                    conv_bands_to_indices(bvals).get(nm) if bvals else None)

        records.append(rec)

    print(f'[S2] Scene done: {len(valid_df)-n_miss} complete | '
          f'S2 miss: {n_miss} | bad spectra: {n_bad}')
    if not records:
        # print the asset_id so this is identifiable without re-running.
        print(f'  [S2] [WARN] All {len(valid_df)} pixels missing S2 reference '
              f'-- asset_id={best["asset_id"]}')
        skip_counts['all_pixels_missing_ref_s2'] += 1
    return records


def _process_landsat_arm(scene_idx, dfile, d_date, refl, wvl, transform, crs, nd,
                         nvis, veg, sensor_clear_veg, cv_lons, cv_lats, ls_rsr_map,
                         n_pixels, t_half, max_days, max_cloud_pct, n_candidates,
                         min_dist_m, min_dist_fallback_m, random_seed, skip_counts):
    """Independent Landsat search & extraction pass for DESIS scenes.
    Structurally identical to s2, same methodology framework.
    REIP/CIre never appear for this arm, no red-edge bands on Landsat to compute them from)."""
    rows, cols = refl.shape[:2]
    records = []

    print('[LS] Searching nearest clearest Landsat scene')
    best = find_best_landsat_scene(
        sensor_date=d_date,
        clear_veg_lons=cv_lons, clear_veg_lats=cv_lats,
        scene_transform=transform, scene_shape=(rows, cols),
        scene_crs=crs, sensor_clear_veg_mask=sensor_clear_veg,
        t_half=t_half, max_days=max_days,
        max_cloud_pct=max_cloud_pct, n_candidates=n_candidates)
    if best is None:
        print('  [LS] [SKIP] No valid Landsat scene')
        skip_counts['no_landsat_scene'] += 1
        return records

    jc       = build_joint_clear_mask_landsat(nd, best['qa_arr'])
    eligible = jc & veg
    n_el     = int(eligible.sum())
    print(f'  [LS] Joint clear: {int(jc.sum())}/{jc.size} ({100*jc.mean():.1f}%) | '
          f'eligible (clear + veg): {n_el}')

    px_df, actual_dist = sample_pixels_thinned(
        nvis_arr=nvis, eligible_mask=eligible,
        transform=transform, scene_crs=crs,
        max_pixels=n_pixels, random_seed=random_seed,
        scene_idx=scene_idx, arm='LS',
        min_dist_primary=min_dist_m,
        min_dist_fallback=min_dist_fallback_m)
    if px_df.empty:
        skip_counts['insufficient_pixels_landsat'] += 1
        return records

    ls_sensor = best['sensor'] # 'L8' or 'L9'
    rsr       = ls_rsr_map.get(ls_sensor, ls_rsr_map['L9'])
    px_data   = {}; n_bad = 0

    for _, px in px_df.iterrows():
        pid   = px['pixel_id']
        r, cc = int(px['pixel_row']), int(px['pixel_col'])
        spec  = refl[r, cc, :].copy()
        if np.all(np.isnan(spec)): n_bad += 1; continue

        nat = compute_native_indices(spec, wvl)
        if np.isnan(nat.get('NDVI', np.nan)): n_bad += 1; continue

        cb_raw = convolve_1d(spec, wvl, rsr, LS_VEG_BAND_CODES)
        cb     = {LS_BAND_MAP[k]: v for k, v in cb_raw.items()}
        ci     = conv_bands_to_indices(cb) # blue/green/red/nir only -> NDVI/EVI2/SAVI/PSRI

        s60, n60   = get_60m_spectrum(refl, nd, r, cc)
        cb60_raw   = convolve_1d(s60, wvl, rsr, LS_VEG_BAND_CODES)
        cb60       = {LS_BAND_MAP[k]: v for k, v in cb60_raw.items()}
        ci60       = conv_bands_to_indices(cb60)

        px_data[pid] = {'nat_idx': nat,
                        'conv_30m_bands': cb,   'conv_30m_idx': ci,
                        'conv_60m_bands': cb60, 'conv_60m_idx': ci60,
                        'n60': n60}

    print(f'  [LS] Convolved {len(px_data)}/{len(px_df)} | bad spectra skipped: {n_bad}')
    if not px_data:
        print('  [LS] [SKIP] No valid DESIS spectra')
        skip_counts['no_valid_spectra_landsat'] += 1
        return records

    valid_df = px_df[px_df['pixel_id'].isin(px_data)].reset_index(drop=True)

    print(f'  [LS] Extracting Landsat at {len(valid_df)} centroids [native/60m]...')
    ls_native, ls_60m = extract_landsat_scales(best['asset_id'], valid_df)

    n_miss = 0
    for _, px in valid_df.iterrows():
        pid = px['pixel_id']
        pd_ = px_data.get(pid)
        if pd_ is None: continue

        b_native = ls_native.get(pid)
        if not b_native or 'red' not in b_native or 'nir' not in b_native:
            n_miss += 1; continue

        rec = {
            'pixel_id':              pid,
            'sensor':                SENSOR_NAME,
            'reference_type':        'Landsat',
            'nvis_mvg_code':         px['nvis_mvg_code'],
            'pixel_row':             px['pixel_row'],
            'pixel_col':             px['pixel_col'],
            'pixel_lat':             px['pixel_lat'],
            'pixel_lon':             px['pixel_lon'],
            'hyperspectral_scene':   dfile.stem,
            'hyperspectral_date':    d_date.isoformat(),
            'ms_sensor':             ls_sensor,
            'ms_date':               best['date'].isoformat(),
            'ms_scene_id':           best['asset_id'],
            'days_diff':             best['days_diff'],
            'ms_cloud_pct':          round(best['cloud_pct'], 2),
            'ms_f_clear_veg':        round(best['f_clear_veg'], 4),
            'ms_score':              round(best['score'], 4),
            'scene_selection_quality': best['scene_quality'],
            't_half':                t_half,
            'actual_min_dist_m':     actual_dist,
            'n_pixels_sampled':      len(valid_df),
            'sensor_60m_n_px':       pd_['n60'],
            'ndmi_native_available': False,
        }

        # DESIS native indices (identical formula regardless of arm)
        for nm in INDICES:
            rec[f'DESIS_{nm}'] = _fmt(pd_['nat_idx'].get(nm))

        # DESIS convolved to Landsat SRF, native ~30m (bands then indices)
        for nm in LS_BAND_MAP.values():
            rec[f'DESIS_conv_LS_30m_{nm}'] = _fmt(pd_['conv_30m_bands'].get(nm))
        for nm in LS_INDICES:
            rec[f'DESIS_conv_LS_30m_{nm}'] = _fmt(pd_['conv_30m_idx'].get(nm))

        # DESIS convolved to Landsat SRF, 60m aggregated (bands then indices)
        for nm in LS_BAND_MAP.values():
            rec[f'DESIS_conv_LS_60m_{nm}'] = _fmt(pd_['conv_60m_bands'].get(nm))
        for nm in LS_INDICES:
            rec[f'DESIS_conv_LS_60m_{nm}'] = _fmt(pd_['conv_60m_idx'].get(nm))

        # Landsat at native / 60m (bands then indices)
        for scale, sr_d in [('native', ls_native), ('60m', ls_60m)]:
            b     = sr_d.get(pid)
            bvals = b if b else {}
            for nm in LS_BAND_MAP.values():
                rec[f'LS_{scale}_{nm}'] = _fmt(bvals.get(nm))
            for nm in LS_INDICES:
                rec[f'LS_{scale}_{nm}'] = _fmt(
                    conv_bands_to_indices(bvals).get(nm) if bvals else None)

        records.append(rec)

    print(f'  [LS] Scene done: {len(valid_df)-n_miss} complete'
          f'Landsat miss: {n_miss} | bad spectra: {n_bad}')
    if not records:
        print(f'  [LS] [WARN] All {len(valid_df)} pixels missing Landsat reference '
              f'-- asset_id={best["asset_id"]}')
        skip_counts['all_pixels_missing_ref_landsat'] += 1
    return records


def main(desis_dir, rsr_base_dir, nvis_path, out_csv,
         n_pixels=MIN_PIXELS, t_half=14, max_days=15,
         max_cloud_pct=70, n_candidates=10,
         min_dist_m=MIN_DIST_DEFAULT_M,
         min_dist_fallback_m=MIN_DIST_FALLBACK_M,
         random_seed=207, n_workers=4):

    os.makedirs(os.path.dirname(out_csv) or '.', exist_ok=True)
    print('='*70)
    print('DESIS to S2 & Landsat 8/9: Vegetation Index Comparison')
    print('='*70)
    print(f'  n_pixels={n_pixels} (min required + ceiling, PER ARM, per scene)')
    print(f'  Spatial thinning: {min_dist_m} m primary, {min_dist_fallback_m} m fallback')
    print(f'  T_half={t_half}d | max_days=+/-{max_days}')
    print(f'  Pass1 max_cloud_pct={max_cloud_pct}% | n_candidates={n_candidates}')
    print(f'  Min f_clear_veg for scene acceptance: {MIN_F_CLEAR_VEG} (each arm independently)')
    print(f'  DESIS 401-1000nm: NDMI native=NaN (no SWIR, both arms)')
    print(f'  Landsat arm: REIP/CIre never computed (no red-edge band on Landsat)')
    print(f'  Search bbox: derived from sensor-QA-clear NVIS veg pixels (not full footprint)')

    if not init_gee(): sys.exit(1)

    rsr_base = Path(rsr_base_dir)
    s2a = next(rsr_base.glob('s2a*.csv'), None)
    s2b = next(rsr_base.glob('s2b*.csv'), None)
    s2c = next(rsr_base.glob('s2c*.csv'), None)
    if not (s2a and s2b and s2c):
        raise FileNotFoundError(f'Missing s2a/b/c RSR csvs in {rsr_base_dir}')
    print('\n[INFO] Loading S2 RSRs...')
    rsr_map = {'S2A':load_s2_rsr(s2a), 'S2B':load_s2_rsr(s2b), 'S2C':load_s2_rsr(s2c)}

    l8_rsr_dir = os.path.join(rsr_base_dir, 'landsat8_rsr_csv')
    l9_rsr_dir = os.path.join(rsr_base_dir, 'landsat9_rsr_csv')
    print('[INFO] Loading Landsat RSRs...')
    ls_rsr_map = {'L8': load_landsat_rsr(l8_rsr_dir), 'L9': load_landsat_rsr(l9_rsr_dir)}

    desis_files = sorted(Path(desis_dir).rglob('*-SPECTRAL_IMAGE.tif'))
    if not desis_files:
        raise FileNotFoundError(f'No *-SPECTRAL_IMAGE.tif in {desis_dir}')
    print(f'\n[INFO] Found {len(desis_files)} DESIS scenes')

    print(f'\n[INFO] Pre-processing ({min(n_workers, len(desis_files))} workers)...')
    cpu = {}
    with ProcessPoolExecutor(max_workers=n_workers) as pool:
        futs = {pool.submit(preprocess_scene, str(df), nvis_path): str(df)
                for df in desis_files}
        for fut in as_completed(futs):
            dfile_str = futs[fut]
            try:
                r = fut.result(); cpu[dfile_str] = r
                if r['error']: print(f'  [ERROR] {Path(dfile_str).name}: {r["error"][:120]}')
                else:          print(f'  [OK] {Path(dfile_str).name}')
            except Exception as e:
                print(f'  [ERROR] {Path(dfile_str).name}: {e}')
                cpu[dfile_str] = {'file':dfile_str, 'error':str(e)}

    all_records = []
    skip_counts = {'no_clear_veg': 0,
                   'no_s2_scene': 0, 'insufficient_pixels_s2': 0, 'no_valid_spectra_s2': 0,
                   'all_pixels_missing_ref_s2': 0,
                   'no_landsat_scene': 0, 'insufficient_pixels_landsat': 0,
                   'no_valid_spectra_landsat': 0, 'all_pixels_missing_ref_landsat': 0}

    for scene_idx, dfile in enumerate(desis_files):
        dfile_str = str(dfile)
        d_date    = parse_desis_date(dfile)
        print(f'\n{"-"*70}')
        print(f'Processing {dfile.name}  |  {d_date}  [scene {scene_idx}]')

        c = cpu.get(dfile_str, {})
        if c.get('error'): print('[SKIP] Pre-processing failed'); continue

        wvl=c['wvl']; refl=c['refl']; transform=c['transform']
        crs=c['crs']; nd=c['nd']; nvis=c['nvis']; veg=c['veg']
        rows, cols = refl.shape[:2]

        n_veg = int(veg.sum())
        print(f'  Cube: {refl.shape} | {wvl[0]:.1f}-{wvl[-1]:.1f}nm | '
              f'veg px: {n_veg} ({100*n_veg/(rows*cols):.1f}%)')

        # Build sensor-QA-clear NVIS veg mask.
        sensor_clear_veg = (~nd) & veg
        n_scv = int(sensor_clear_veg.sum())
        print(f'  Sensor-QA-clear veg pixels: {n_scv}')

        if n_scv == 0:
            print('[SKIP] No sensor-QA-clear veg pixels')
            skip_counts['no_clear_veg'] += 1
            continue

        cv_lons, cv_lats = get_clear_veg_bbox(sensor_clear_veg, transform, crs)

        # Pass S2 (independent)
        s2_records = _process_s2_arm(
            scene_idx, dfile, d_date, refl, wvl, transform, crs, nd,
            nvis, veg, sensor_clear_veg, cv_lons, cv_lats, rsr_map,
            n_pixels, t_half, max_days, max_cloud_pct, n_candidates,
            min_dist_m, min_dist_fallback_m, random_seed, skip_counts)
        all_records.extend(s2_records)

        # Pass Landsat (independent)
        ls_records = _process_landsat_arm(
            scene_idx, dfile, d_date, refl, wvl, transform, crs, nd,
            nvis, veg, sensor_clear_veg, cv_lons, cv_lats, ls_rsr_map,
            n_pixels, t_half, max_days, max_cloud_pct, n_candidates,
            min_dist_m, min_dist_fallback_m, random_seed, skip_counts)
        all_records.extend(ls_records)

    if not all_records:
        print('\n[WARNING] No records produced.'); return

    print(f'\n{"="*70}')
    print(f'[INFO] Skip summary:')
    for reason, n in skip_counts.items():
        if n: print(f'  {reason}: {n} scenes')

    out_df = pd.DataFrame(all_records)
    out_df = out_df.sort_values(['hyperspectral_date', 'reference_type', 'pixel_id']).reset_index(drop=True)
    out_df.to_csv(out_csv, index=False)
    print(f'\n[INFO] Saved {len(out_df)} rows -> {out_csv}')
    print(f'       Columns ({len(out_df.columns)}): {list(out_df.columns)[:10]} ...')
    for ref in ('S2', 'Landsat'):
        sub = out_df[out_df['reference_type'] == ref]
        print(f'       {ref} arm: {len(sub)} rows | '
              f'{sub["hyperspectral_scene"].nunique()} / {len(desis_files)} scenes retained')
    print(f'       actual_min_dist_m distribution (by arm):')
    print(out_df.groupby(['reference_type', 'hyperspectral_scene'])['actual_min_dist_m']
          .first().groupby('reference_type').value_counts().to_string())



# ARGUMENTS
# =============================================================================
if __name__ == '__main__':
    ap = argparse.ArgumentParser(
        description='DESIS to S2 & Landsat 8/9 convolution'
                    '(DESIS+S2 joint-clear and DESIS+Landsat joint-clear are searched,'
                    'scored and thinned separately '
                    'f_clear>=0.30 per arm for scene acceptance'
                    'REIP/CIre never computed for the Landsat arm (no red-edge band)')
    ap.add_argument('--desis_dir',           required=True)
    ap.add_argument('--rsr_base',            required=True,
                    help='Must contain s2a/b/c*.csv AND landsat8_rsr_csv/, '
                         'landsat9_rsr_csv/ subfolders')
    ap.add_argument('--nvis',                required=True)
    ap.add_argument('--out',                 default='desis_s2_landsat_v6.csv')
    ap.add_argument('--n_pixels',            type=int,   default=MIN_PIXELS,
                    help=f'Pixels per scene PER ARM - ceiling and minimum (default {MIN_PIXELS})')
    ap.add_argument('--t_half',              type=float, default=14)
    ap.add_argument('--max_days',            type=int,   default=15)
    ap.add_argument('--max_cloud_pct',       type=float, default=70)
    ap.add_argument('--n_candidates',        type=int,   default=10,
                    help='Scenes passed from Pass 1 to Pass 2 evaluation, per arm (default 10). '
                         'Sorted by days_diff then cloud_pct')
    ap.add_argument('--min_dist_m',          type=int,   default=MIN_DIST_DEFAULT_M,
                    help='Primary spatial thinning distance in metres (default 300)')
    ap.add_argument('--min_dist_fallback_m', type=int,   default=MIN_DIST_FALLBACK_M,
                    help='Fallback spatial thinning distance in metres (default 200)')
    ap.add_argument('--random_seed',         type=int,   default=207,
                    help='Default 207 (project-wide convention).')
    ap.add_argument('--n_workers',           type=int,   default=4)
    args = ap.parse_args()

    log_dir = os.path.dirname(LOG_FILE)
    if log_dir: os.makedirs(log_dir, exist_ok=True)
    old = sys.stdout
    with open(LOG_FILE, 'w', encoding='utf-8') as lf:
        tee = Tee(old, lf); sys.stdout = tee
        try:
            main(desis_dir=args.desis_dir, rsr_base_dir=args.rsr_base,
                 nvis_path=args.nvis, out_csv=args.out,
                 n_pixels=args.n_pixels, t_half=args.t_half,
                 max_days=args.max_days, max_cloud_pct=args.max_cloud_pct,
                 n_candidates=args.n_candidates,
                 min_dist_m=args.min_dist_m,
                 min_dist_fallback_m=args.min_dist_fallback_m,
                 random_seed=args.random_seed, n_workers=args.n_workers)
        except Exception as e:
            print(f'\n[FATAL] {e}'); traceback.print_exc(); raise
        finally: sys.stdout = old
