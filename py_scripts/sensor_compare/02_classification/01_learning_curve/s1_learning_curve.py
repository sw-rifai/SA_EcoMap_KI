#!/usr/bin/env python3
"""
Unified hyperspectral learning-curve classifier.

Trains three RF classifiers (hyperspec vs ref_all vs ref_vnir) at increasing sample sizes [5,10,20,50,100] per class 
for one sensor x one reference combination. Outputs per-iteration results CSV and per-scene AULC summary CSV.

Inputs
------
 --sensor      : PRISMA, DESIS, EMIT, WYVERN
 --sensor_dir  : directory containing sensor scene files
 --reference   : s2, landsat
 --nvis_raster : NVIS v7 MVG GeoTIFF (100 m res, cropped over KI)
 --out_csv     : results csv
 --out_summary : summary csv (AULC per scene)

Usage
-----
  python s1_learning_curve.py \\
     --sensor      PRISMA \\
     --sensor_dir  "Q:/PRISMA" \\
     --reference   s2 \\
     --nvis_raster "Q:/NVIS/nvis_mvg_v7.tif" \\
     --out_csv     "./lc_prisma_s2_results.csv" \\
     --out_summary "./lc_prisma_s2_summary.csv"
"""

# 1. IMPORTS
# ═════════════════════════════════════════════════════════════════════════

import os, sys, gc, math, time, warnings
import argparse, traceback
import zipfile, tempfile, shutil
import xml.etree.ElementTree as ET
from pathlib import Path
from datetime import datetime, timedelta

import numpy as np
import pandas as pd
import h5py
import rasterio
from rasterio.transform import Affine
from rasterio.crs import CRS
from rasterio.warp import reproject, Resampling, calculate_default_transform
import rioxarray
from pyproj import Transformer
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import (accuracy_score, cohen_kappa_score, confusion_matrix, recall_score)
import ee

warnings.filterwarnings('ignore')

# 2. DEFAULT CONFIG
# ═════════════════════════════════════════════════════════════════════════

SENSOR_NAME  = 'WYVERN'          # PRISMA | DESIS | EMIT | WYVERN
REFERENCE    = 's2'               # s2     | landsat
SENSOR_DIR   = r'C:\path\to\scenes'
NVIS_RASTER  = r"C:\Users\Annie\Documents\local_docs\ki_local\spatial_layers\nvis_mvg_rast_cropped_ki.tif"
OUT_CSV      = './lc_results.csv'
OUT_SUMMARY  = './lc_summary.csv'
CM_DIR       = './confusion_matrices' # set to None to skip

EMIT_MASK_DIR      = None # overwritten by --emit_mask_dir arg; None = search RFL only
EMIT_UNCERT_THRESH = 0.05

# Sampling / temporal
SAMPLE_SIZES         = [5, 10, 20, 50, 100]
N_ITERATIONS         = 10
TEST_PER_CLASS       = 50
POOL_CAP             = 100
MAX_CANDIDATES_PC    = 300
MIN_CLASS_CANDIDATES = 100
MIN_POOL_PER_CLASS   = 100
MIN_N_CLASSES        = 3

# Every retained class must have >= MIN_JOINT_CLEAR clear pixels in the
# chosen reference scene (= TEST_PER_CLASS + MIN_POOL_PER_CLASS)
MIN_JOINT_CLEAR      = TEST_PER_CLASS + MIN_POOL_PER_CLASS   # = 150

# AULC reference range. FIXED across all scenes for cross-scene/sensor comparability
AULC_X_MIN           = 5
AULC_X_MAX           = 100
MIN_DIST_PRIMARY     = 500.0     # metres, primary spatial thinning
MIN_DIST_FALLBACK    = 200.0     # metres, fallback (NVIS 100 m artefact risk below this)
T_HALF               = 14        # days, temporal penalty half-life
MAX_DAYS             = 30        # +- days to search GEE for reference scene
RF_TREES             = 500
SEED                 = 207

#NVIS MVG
# 0 = null / raster nodata
# 24 = Inland water
# 25 = Cleared, non-native vegetation, buildings / roads / impervious
# 27 = Naturally bare / sparsely vegetated (bare rock, sand)
# 28 = Seas and estuaries
# 99 = NVIS unknown / no data
NVIS_EXCLUDE = {0, 24, 25, 27, 28, 99}

# Sentinel-2 band config
S2_BANDS     = ['B2','B3','B4','B5','B6','B7','B8','B8A','B11','B12']
S2_SCALE     = 0.0001
S2_CLEAR_SCL = {4, 5, 6}
S2_ALL_IDX   = list(range(10))
S2_VNIR_IDX  = list(range(8))    # B2–B8A (no SWIR)

# Landsat 8/9 Collection 2 L2 band config
LS_BANDS     = ['SR_B2','SR_B3','SR_B4','SR_B5','SR_B6','SR_B7']
LS_SCALE     = 0.0000275
LS_OFFSET    = -0.2
LS_CLEAR_BIT = 64 # QA_PIXEL bit 6
LS_ALL_IDX   = list(range(6))
LS_VNIR_IDX  = list(range(4)) # SR_B2–SR_B5 (no SWIR)

# EMIT atmospheric absorption windows (nm), dropped from feature matrix
EMIT_ATM_WINDOWS = [
    (900, 1000), (1080, 1180), (1300, 1500), (1750, 2000), (2400, 2500),
]
EMIT_MIN_FINITE_FRAC  = 0.95
EMIT_MAX_CONTIGUOUS_NAN = 3

# Wyvern Dragonette-004 band centres (nm), 31 VNIR bands
WYVERN_WAVES = np.array([
    444, 464, 480, 490, 503, 510, 520, 535, 549, 569,
    584, 600, 615, 634, 650, 659, 669, 680, 690, 699,
    711, 722, 735, 750, 765, 782, 799, 815, 832, 850, 870
], dtype=np.float32)



# 3. TEXT LOGGER
# ═════════════════════════════════════════════════════════════════════════

class Tee:
    def __init__(self, *files):
        self.files = files
    def write(self, obj):
        for f in self.files:
            f.write(obj); f.flush()
    def flush(self):
        for f in self.files:
            f.flush()


def _log_exc(scene_name, exc):
    msg = (f'\n[ERROR] Scene failed: {scene_name}\n'
           f'  {type(exc).__name__}: {exc}\n{traceback.format_exc()}')
    print(msg)


# 4. CRS UTILITIES
# ═════════════════════════════════════════════════════════════════════════

def _utm_epsg_from_lonlat(lon, lat):
    """Return the EPSG code for the UTM zone covering (long, lat)."""
    zone = int((float(lon) + 180.0) / 6.0) + 1
    zone = max(1, min(zone, 60))
    return (32600 if float(lat) >= 0 else 32700) + zone


def _crs_from_rasterio(fpath_str):
    """Read CRS from a raster file; returns string or None."""
    try:
        with rasterio.open(fpath_str) as src:
            if src.crs is not None:
                return src.crs.to_string()
    except Exception:
        pass
    return None


# 5. SENSOR READERS
# ══════════════════════════════════════════════════════════════════════════════
# All readers return:
# refl_yxc  : (rows, cols, bands)  float32 - NaN where masked
# waves     : (bands,)             float32 - band centre wavelengths (nm)
# transform : rasterio.Affine
# crs_str   : str CRS (authoritative - read from file metadata, never assumed)
# nodata    : (rows, cols) bool, True = BAD / invalid pixel


# EMIT ─────────────────────────────────────────────────────────────────────

EMIT_NODATA = -9999.0

def _emit_obs_key(fname):
    p = Path(fname).stem.split('_')
    return '_'.join(p[4:7]) if len(p) >= 7 else None

def _emit_date(fname):
    p = Path(fname).stem.split('_')
    d = p[4] if len(p) >= 5 else ''
    return f'{d[:4]}-{d[4:6]}-{d[6:8]}' if len(d) >= 8 else 'unknown'

def parse_emit_date(fpath):
    d = _emit_date(Path(fpath).name)
    return datetime.strptime(d, '%Y-%m-%d').date() if d != 'unknown' else None

def _find_emit_mask(rfl_path, mask_dir):
    key = _emit_obs_key(Path(rfl_path).name)
    if key is None:
        return None
    for d in [mask_dir, Path(rfl_path).parent]:
        if d and Path(d).exists():
            hits = list(Path(d).glob(f'*MASK*{key}*.nc'))
            if hits:
                return hits[0]
    return None

def _find_emit_uncert(rfl_path):
    key = _emit_obs_key(Path(rfl_path).name)
    if key is None:
        return None
    for f in Path(rfl_path).parent.glob(f'*RFLUNCERT*{key}*.nc'):
        return f
    return None

def _parse_geotransform(attr):
    if attr is None:
        return None
    if isinstance(attr, np.ndarray):
        gt = attr.tolist()
    elif isinstance(attr, bytes):
        gt = attr.decode()
    else:
        gt = attr
    if isinstance(gt, str):
        gt = [float(x) for x in gt.strip().strip('[]').replace('\n',' ').split() if x]
    return Affine.from_gdal(*gt) if len(gt) == 6 else None

def read_emit(fpath, mask_dir=None, uncert_thresh=0.05):
    """
    Read EMIT L2A reflectance.
    Target CRS is derived from the scene's own lat/lon centroid -
    no hardcoded EPSG, ensuring correct UTM zone for every scene.
    """
    fpath = Path(fpath)
    print(f'  Reading EMIT: {fpath.name}')
    with h5py.File(str(fpath), 'r') as f:
        wvl       = np.array(f['sensor_band_parameters/wavelengths'][:]).flatten()
        good_wvl  = np.array(f['sensor_band_parameters/good_wavelengths'][:]).flatten().astype(bool)
        refl      = f['reflectance'][:]
        lats      = f['location/lat'][:]
        lons      = f['location/lon'][:]
        glt_x     = f['location/glt_x'][:] if 'location/glt_x' in f else None
        glt_y     = f['location/glt_y'][:] if 'location/glt_y' in f else None
        gt_affine = _parse_geotransform(f.attrs.get('geotransform'))

    finite_lons = lons[np.isfinite(lons)]
    finite_lats = lats[np.isfinite(lats)]
    if finite_lons.size == 0 or finite_lats.size == 0:
        raise ValueError(f'No finite lat/lon values in {fpath.name}')
    centroid_lon = float(np.median(finite_lons))
    centroid_lat = float(np.median(finite_lats))
    target_epsg  = _utm_epsg_from_lonlat(centroid_lon, centroid_lat)
    print(f'  Scene centroid: ({centroid_lon:.3f} E, {centroid_lat:.3f})')
    print(f'  Auto target CRS: EPSG:{target_epsg} '
          f'(UTM zone {target_epsg % 100}{"N" if centroid_lat >= 0 else "S"})')

    n_down, n_cross, n_bands = refl.shape
    print(f'  Swath: {n_down}×{n_cross}, {n_bands} bands ({int(good_wvl.sum())} good)')

    # GLT check
    if glt_x is not None and not np.any(np.abs(glt_x) > 0):
        glt_x = glt_y = None

    # Tier 1 mask (scene-level flags)
    swath_bad = np.zeros((n_down, n_cross), dtype=bool)
    mask_fp = _find_emit_mask(fpath, mask_dir)
    if mask_fp and Path(mask_fp).exists():
        with h5py.File(str(mask_fp), 'r') as mf:
            md = None
            for k in ['mask','Mask','mask_bands']:
                if k in mf:
                    md = mf[k][:]
                    break
            if md is not None and md.ndim == 3:
                if md.shape[0] <= 20 and md.shape[-1] > 20:
                    md = np.transpose(md, (1,2,0))
                if md.shape[:2] == (n_down, n_cross):
                    for fi in [0,1,2,3,4,7,9]:
                        if fi < md.shape[2]:
                            swath_bad |= (md[:,:,fi] == 1)
        print(f'  Tier 1 flagged: {int(swath_bad.sum())} px')
    else:
        print('  WARNING: no mask file found')

    # Tier 2 mask (per-band uncertainty)
    swath_band_err = np.zeros((n_down, n_cross, n_bands), dtype=bool)
    unc_fp = _find_emit_uncert(fpath)
    if unc_fp and unc_fp.exists():
        with h5py.File(str(unc_fp), 'r') as uf:
            if 'reflectance_uncertainty' in uf:
                uc = uf['reflectance_uncertainty'][:]
                if uc.shape[:2] == (n_down, n_cross):
                    bad_u = (uc >= uncert_thresh)
                    bad_u[np.isnan(uc)] = False
                    swath_band_err |= bad_u
    swath_band_err[swath_bad, :] = True
    refl[swath_bad, :] = np.nan

    # GLT orthorectification
    if glt_x is not None and glt_y is not None:
        or_, oc_ = glt_x.shape
        gx = np.clip(np.abs(glt_x).astype(int) - 1, 0, n_cross-1)
        gy = np.clip(np.abs(glt_y).astype(int) - 1, 0, n_down-1)
        valid_glt = (np.abs(glt_x) > 0) & (np.abs(glt_y) > 0)
        ort = np.full((n_bands, or_, oc_), np.nan, dtype=np.float32)
        ort[:, valid_glt] = refl[gy[valid_glt], gx[valid_glt], :].T
        obe = np.ones((n_bands, or_, oc_), dtype=bool)
        obe[:, valid_glt] = swath_band_err[gy[valid_glt], gx[valid_glt], :].T
        nodata_2d = ~valid_glt
        if gt_affine is not None:
            t4326, c4326 = gt_affine, CRS.from_epsg(4326)
        else:
            w = float(np.nanmin(lons)); e = float(np.nanmax(lons))
            s = float(np.nanmin(lats)); n = float(np.nanmax(lats))
            t4326 = Affine((e-w)/oc_, 0, w, 0, (s-n)/or_, n)
            c4326 = CRS.from_epsg(4326)
    else:
        # GLT all-zero fill: fall back to naive affine georeferencing
        ort = np.transpose(refl, (2,0,1)).astype(np.float32)
        or_, oc_ = n_down, n_cross
        obe = np.transpose(swath_band_err, (2,0,1))
        lat_d = np.nanmean(lats[-1,:]) - np.nanmean(lats[0,:])
        if lat_d < 0:
            ort = ort[:,::-1,:]; obe = obe[:,::-1,:]
        dy = np.nanmedian(np.diff(lats[:,0])) if lats.shape[0]>1 else 0.01
        dx = np.nanmedian(np.diff(lons[0,:])) if lons.shape[1]>1 else 0.01
        t4326 = Affine(abs(dx), 0, float(np.nanmin(lons)), 0, -abs(dy), float(np.nanmax(lats)))
        c4326 = CRS.from_epsg(4326)
        nodata_2d = np.zeros((or_, oc_), dtype=bool)

    # Keep only good wavelengths, then drop atmospheric windows
    good_idx = np.where(good_wvl)[0]
    wvl_g   = wvl[good_idx]
    ort_g   = ort[good_idx]

    atm_mask = np.zeros(len(wvl_g), dtype=bool)
    for lo, hi in EMIT_ATM_WINDOWS:
        atm_mask |= (wvl_g >= lo) & (wvl_g <= hi)
    cont_idx = np.where(~atm_mask)[0]
    wvl_out  = wvl_g[cont_idx].astype(np.float32)
    ort_out  = ort_g[cont_idx]                     # (bands_out, rows, cols)
    print(f'  Bands after atm drop: {len(wvl_out)} / {len(wvl_g)} good')

    # Reproject to target_epsg
    tc  = CRS.from_epsg(target_epsg)
    dst_t, dst_w, dst_h = calculate_default_transform(
        c4326, tc, oc_, or_,
        left=t4326.c, bottom=t4326.f + t4326.e*or_,
        right=t4326.c + t4326.a*oc_, top=t4326.f)

    refl_proj = np.full((len(wvl_out), dst_h, dst_w), np.nan, dtype=np.float32)
    nodata_sent = -32768.0
    for bi in range(len(wvl_out)):
        src = ort_out[bi].copy()
        src[nodata_2d] = nodata_sent
        dst = np.full((dst_h, dst_w), nodata_sent, dtype=np.float32)
        reproject(source=src, destination=dst,
                  src_transform=t4326, src_crs=c4326,
                  dst_transform=dst_t, dst_crs=tc,
                  resampling=Resampling.bilinear,
                  src_nodata=nodata_sent, dst_nodata=nodata_sent)
        band_out = dst.copy()
        band_out[dst == nodata_sent] = np.nan
        refl_proj[bi] = band_out

    nodata_out = np.all(~np.isfinite(refl_proj), axis=0)
    refl_yxc   = np.transpose(refl_proj, (1,2,0))   # (rows, cols, bands)
    print(f'  Reprojected: {dst_h}×{dst_w}, EPSG:{target_epsg}')
    return refl_yxc, wvl_out, dst_t, f'EPSG:{target_epsg}', nodata_out


# PRISMA ────────────────────────────────────────────────────────────────────

def parse_prisma_date(fpath):
    s = Path(fpath).stem
    for part in s.split('_'):
        if len(part) >= 8 and part[:8].isdigit():
            return datetime.strptime(part[:8], '%Y%m%d').date()
    raise ValueError(f'Cannot parse PRISMA date: {s}')


def read_prisma_from_zip(zip_path):
    import zipfile, tempfile, shutil
    tmpdir = tempfile.mkdtemp(prefix='prisma_')
    try:
        with zipfile.ZipFile(zip_path, 'r') as z:
            z.extractall(tmpdir)
        he5_files = list(Path(tmpdir).rglob('*.he5'))
        if not he5_files:
            raise FileNotFoundError(f"No .he5 in {zip_path}")
        he5_path = str(he5_files[0])

        with h5py.File(he5_path, 'r') as f:
            base = 'HDFEOS/SWATHS/PRS_L2D_HCO/Data Fields'
            vnir = f[f'{base}/VNIR_Cube'][()]
            swir = f[f'{base}/SWIR_Cube'][()]
            vnir_wvl = np.array(f.attrs['List_Cw_Vnir']).flatten()
            swir_wvl = np.array(f.attrs['List_Cw_Swir']).flatten()
            vnir_min = float(f.attrs['L2ScaleVnirMin'])
            vnir_max = float(f.attrs['L2ScaleVnirMax'])
            swir_min = float(f.attrs['L2ScaleSwirMin'])
            swir_max = float(f.attrs['L2ScaleSwirMax'])
            epsg  = int(f.attrs['Epsg_Code'])
            ul_e  = float(f.attrs['Product_ULcorner_easting'])
            ul_n  = float(f.attrs['Product_ULcorner_northing'])
            lr_e  = float(f.attrs['Product_LRcorner_easting'])
            lr_n  = float(f.attrs['Product_LRcorner_northing'])
            l1_paths = {
                'Cloud_Mask':    'HDFEOS/SWATHS/PRS_L1_HCO/Data Fields/Cloud_Mask',
                'SunGlint_Mask': 'HDFEOS/SWATHS/PRS_L1_HCO/Data Fields/SunGlint_Mask',
            }
            l1_masks = {n: f[p][()] for n, p in l1_paths.items() if p in f}

        # DN -> reflectance
        max_val = 65535.0
        vnir_fill = (vnir == 0); swir_fill = (swir == 0)
        vnir = vnir_min + (vnir.astype(np.float32) / max_val) * (vnir_max - vnir_min)
        swir = swir_min + (swir.astype(np.float32) / max_val) * (swir_max - swir_min)
        vnir[vnir_fill] = np.nan; swir[swir_fill] = np.nan

        # Merge and sort by wavelength
        refl  = np.concatenate((vnir, swir), axis=1)   # axis=1 is band axis in PRISMA raw
        waves = np.concatenate((vnir_wvl, swir_wvl))
        valid = waves > 0
        refl  = refl[:, valid, :]; waves = waves[valid]
        order = np.argsort(waves); waves = waves[order]; refl = refl[:, order, :]
        refl  = np.transpose(refl, (0, 2, 1))          # -> (rows, cols, bands)
        rows, cols, _ = refl.shape

        # Nodata mask
        nodata = np.all(np.isnan(refl), axis=2)
        if 'Cloud_Mask' in l1_masks:
            cm = l1_masks['Cloud_Mask'] > 0
            if cm.shape == (rows, cols): nodata |= cm
        if 'SunGlint_Mask' in l1_masks:
            gm = l1_masks['SunGlint_Mask'] > 0
            if gm.shape == (rows, cols): nodata |= gm

        from rasterio.transform import from_bounds
        transform = from_bounds(ul_e, lr_n, lr_e, ul_n, cols, rows)
        return refl, waves, transform, f'EPSG:{epsg}', nodata

    finally:
        shutil.rmtree(tmpdir, ignore_errors=True)

# DESIS ─────────────────────────────────────────────────────────────────────

def parse_desis_date(fpath):
    s = Path(fpath).stem
    for part in s.split('-'):
        if len(part) >= 8 and part[:8].isdigit():
            return datetime.strptime(part[:8], '%Y%m%d').date()
    raise ValueError(f'Cannot parse DESIS date: {s}')

def read_desis(fpath):
    fpath = Path(fpath)
    prefix = fpath.name.split('-SPECTRAL_IMAGE')[0]
    parent = fpath.parent
    meta = next(parent.glob(f'{prefix}-*METADATA.xml'), None)
    ql2  = next(parent.glob(f'{prefix}-*QL_QUALITY-2.tif'), None)

    if meta and meta.exists():
        tree = ET.parse(str(meta))
        wvl_elems = tree.getroot().findall('.//wavelengthCenterOfBand')
        waves = np.array([float(e.text) for e in wvl_elems], dtype=np.float32)
    else:
        waves = None

    with rasterio.open(str(fpath)) as src:
        refl      = src.read().astype(np.float32)
        transform = src.transform
        crs       = src.crs

    if crs is None:
        raise ValueError(
            f'DESIS file {fpath.name} has no embedded CRS.'
            f'Check that the GeoTIFF was correctly georeferenced on delivery.')
    crs_str = crs.to_string()
    print(f'  CRS: {crs_str} (read from GeoTIFF metadata)')

    bg_mask = np.any(refl == -32768, axis=0)
    refl *= 0.0001
    refl[:, bg_mask] = np.nan

    if waves is None or len(waves) != refl.shape[0]:
        waves = np.linspace(401, 1000, refl.shape[0], dtype=np.float32)

    nodata = bg_mask.copy()
    if ql2 and ql2.exists():
        with rasterio.open(str(ql2)) as q:
            land_clear  = q.read(2)
            water_clear = q.read(8)
            shadow      = q.read(1); snow      = q.read(3)
            haze_l      = q.read(4); haze_w    = q.read(5)
            cloud_l     = q.read(6); cloud_w   = q.read(7)
        clear      = (land_clear == 1) | (water_clear == 1)
        contam     = (cloud_l|cloud_w|haze_l|haze_w|shadow|snow) == 1
        nodata    |= (~clear) | contam

    refl_yxc = np.transpose(refl, (1,2,0))
    return refl_yxc, waves, transform, crs_str, nodata

#  WYVERN ────────────────────────────────────────────────────────────────────

def parse_wyvern_date(fpath):
    stem = Path(fpath).stem
    for part in stem.split('_'):
        if len(part) >= 14 and part[:8].isdigit():
            return datetime.strptime(part[:8], '%Y%m%d').date()
    raise ValueError(f'Cannot parse Wyvern date: {stem}')

def read_wyvern(fpath):
    fpath = Path(fpath)
    base = str(fpath).replace('_l2a.tiff', '')
    dm_path = base + '_l2a_data_mask.tiff'
    pq_path = base + '_l2a_pixel_quality_mask.tiff'

    ds       = rioxarray.open_rasterio(str(fpath),
                                       chunks={'band':-1,'x':512,'y':512})
    dm       = rioxarray.open_rasterio(dm_path)
    pq       = rioxarray.open_rasterio(pq_path)

    assert ds.shape[0] == len(WYVERN_WAVES), (
        f'Expected {len(WYVERN_WAVES)} bands, got {ds.shape[0]}')

    data_bad = dm.sel(band=1) != 1
    pq_bad   = (pq == 255).any(dim='band')
    fill_bad = ds.sel(band=1) == 65535
    invalid  = (data_bad | pq_bad | fill_bad).values

    refl = ds.values.astype(np.float32) / 10000.0   # (bands, rows, cols)
    refl[:, invalid] = np.nan

    transform = ds.rio.transform()
    rio_crs   = ds.rio.crs
    if rio_crs is None:
        raise ValueError(
            f'Wyvern file {fpath.name} has no embedded CRS.'
            f'Check that the GeoTIFF was correctly georeferenced on delivery.')
    crs_str = rio_crs.to_string()
    print(f'  CRS: {crs_str} (read from GeoTIFF metadata)')
    refl_yxc  = np.transpose(refl, (1,2,0))         # (rows, cols, bands)
    nodata    = invalid.copy()

    n_valid = int((~nodata).sum())
    print(f'  [QA] Valid pixels: {n_valid}/{nodata.size} '
          f'({100*n_valid/nodata.size:.1f}%)')
    return refl_yxc, WYVERN_WAVES.copy(), transform, crs_str, nodata


# 5. NVIS + SPATIAL SAMPLING
# ══════════════════════════════════════════════════════════════════════════

def sample_nvis_at_points(nvis_path, xs, ys, from_crs):
    """Sample NVIS raster at (xs, ys) projected coordinates.  Returns int32 array."""
    with rasterio.open(nvis_path) as src:
        to_crs = src.crs.to_string()
        tr = Transformer.from_crs(from_crs, to_crs, always_xy=True)
        xt, yt = tr.transform(xs, ys)
        vals = np.array([v[0] for v in src.sample(zip(xt, yt))], dtype=np.int32)
    return vals


def _to_metres(xs, ys, crs_str):
    """Convert projected or geographic coordinates to metres via local UTM."""
    src = CRS.from_user_input(crs_str)
    if not src.is_geographic:
        return xs.copy(), ys.copy()
    # xs = lons, ys = lats (WGS84)
    med_lon = float(np.nanmedian(xs))
    med_lat = float(np.nanmedian(ys))
    zone    = int((med_lon + 180) / 6) + 1
    epsg    = (32600 if med_lat >= 0 else 32700) + zone
    tr = Transformer.from_crs('EPSG:4326', f'EPSG:{epsg}', always_xy=True)
    xm, ym = tr.transform(xs, ys)
    return xm, ym


def _px_to_lonlat(xs, ys, crs_str):
    """Convert projected (or geographic) pixel coords to WGS84 lon, lat."""
    src = CRS.from_user_input(crs_str)
    if src.is_geographic:
        return xs.copy(), ys.copy()
    tr = Transformer.from_crs(src, 'EPSG:4326', always_xy=True)
    return tr.transform(xs, ys)


def greedy_spatial_sample(xm, ym, target_n, min_dist_m, rng):
    """
    Greedy random spatial thinning in metric coordinates.
    Returns index array (into xm/ym) of selected points.
    """
    idx = np.arange(len(xm))
    rng.shuffle(idx)
    sel = []
    for i in idx:
        if len(sel) >= target_n:
            break
        x0, y0 = xm[i], ym[i]
        if not sel:
            sel.append(i)
        else:
            sxm = xm[sel]; sym = ym[sel]
            if np.all(np.sqrt((sxm-x0)**2 + (sym-y0)**2) >= min_dist_m):
                sel.append(i)
    return np.array(sel, dtype=int)


def adaptive_spatial_sample(cand_rr, cand_cc, cand_xs, cand_ys,
                             cand_classes, crs_str,
                             max_per_class, min_dist_primary, min_dist_fallback,
                             min_test_pixels, seed):
    """
    For each NVIS class:
      1. Convert to metres (in case CRS is geographic)
      2. Thin the data at min_dist_primary -> n_sel
      3. If n_sel < min_test_pixels: retry at min_dist_fallback
      4. If still < min_test_pixels: drop class
      5. Keep up to max_per_class candidates

    Returns
   -------
    pixels_df   : DataFrame  Pixel_ID | MVG_class | row | col | Lat | Long
    class_info  : dict  {class_id: {'n_candidates': int, 'min_dist_m': float}}
    """
    rng = np.random.RandomState(seed)
    classes = sorted(int(c) for c in set(cand_classes))

    # Project to metres once
    xm, ym = _to_metres(cand_xs, cand_ys, crs_str)

    records    = []
    class_info = {}

    for cls in classes:
        mask = (cand_classes == cls)
        cx, cy = xm[mask], ym[mask]
        cxr, cyc = cand_xs[mask], cand_ys[mask]
        crr, ccc = cand_rr[mask], cand_cc[mask]
        n_avail  = len(cx)

        sel_idx  = np.array([], dtype=int)
        used_dist = min_dist_primary

        for min_d in [min_dist_primary, min_dist_fallback]:
            used_dist = min_d
            sel_idx   = greedy_spatial_sample(cx, cy, max_per_class, min_d, rng)
            if len(sel_idx) >= min_test_pixels:
                break

        if len(sel_idx) < min_test_pixels:
            print(f'    MVG {cls:>3d}: {len(sel_idx)}/{min_test_pixels} at '
                  f'{int(min_dist_fallback)} m - DROPPED')
            continue

        n_sel = len(sel_idx)
        print(f'MVG {cls:>3d}: {n_sel} candidates '
              f'(dist={int(used_dist)} m  pool={n_avail})')
        class_info[cls] = {'n_candidates': n_sel, 'min_dist_m': float(used_dist)}

        lons, lats = _px_to_lonlat(cxr[sel_idx], cyc[sel_idx], crs_str)

        for i, (r, c, lon, lat) in enumerate(
                zip(crr[sel_idx], ccc[sel_idx], lons, lats)):
            records.append({
                'Pixel_ID':  f'MVG{cls:02d}_{i:04d}',
                'MVG_class': int(cls),
                'row':       int(r),
                'col':       int(c),
                'Lat':       float(lat),
                'Long':      float(lon),
            })

    return pd.DataFrame(records), class_info


# 6. GEE: INIT, SCENE SELECTION, EXTRACTION
# ═════════════════════════════════════════════════════════════════════════

def init_gee():
    try:
        try:
            ee.Initialize(opt_url='https://earthengine-highvolume.googleapis.com')
        except Exception:
            ee.Initialize()
        print('[INFO] GEE initialised')
        return True
    except Exception as e:
        print(f'[ERROR] GEE init failed: {e}')
        return False


def _build_fc(pixels_df):
    """Build GEE FeatureCollection from Pixel_ID / Lat / Long DataFrame."""
    feats = []
    for _, row in pixels_df.iterrows():
        pt = ee.Geometry.Point(float(row['Long']), float(row['Lat']))
        feats.append(ee.Feature(pt, {'Pixel_ID': str(row['Pixel_ID'])}))
    return ee.FeatureCollection(feats)


def _score_pixels_s2(img, sites_fc, n_pixels):
    """
    Sample SCL at all sites.  Returns (f_clear, clear_ids).
    SCL ∈ {4,5,6} = clear land / bare / water.
    """
    sampled = (img.select('SCL')
               .reduceRegions(collection=sites_fc,
                              reducer=ee.Reducer.first(), scale=10)
               .getInfo())
    clear_ids = []
    for feat in sampled['features']:
        val = feat['properties'].get('first')
        pid = feat['properties']['Pixel_ID']
        if val is not None and int(val) in S2_CLEAR_SCL:
            clear_ids.append(pid)
    return len(clear_ids) / n_pixels if n_pixels > 0 else 0.0, clear_ids


def _score_pixels_landsat(img, sites_fc, n_pixels):
    """
    Sample QA_PIXEL at all sites.  Returns (f_clear, clear_ids).
    Bit 6 (value 64) = clear.
    """
    sampled = (img.select('QA_PIXEL')
               .reduceRegions(collection=sites_fc,
                              reducer=ee.Reducer.first(), scale=30)
               .getInfo())
    clear_ids = []
    for feat in sampled['features']:
        val = feat['properties'].get('first')
        pid = feat['properties']['Pixel_ID']
        if val is not None and (int(val) & LS_CLEAR_BIT):
            clear_ids.append(pid)
    return len(clear_ids) / n_pixels if n_pixels > 0 else 0.0, clear_ids


def find_best_scene(reference, target_date, pixels_df, max_days=MAX_DAYS,
                    t_half=T_HALF, max_candidates=25,
                    min_joint_clear=MIN_JOINT_CLEAR):
    """
    Adaptive candidate-evaluation S2 / Landsat scene selection.

    Scores all candidates within +- max_days by
    score = f_clear x exp(-|Δt| / t_half), then iterates in descending
    score order, accepting the first candidate where every MVG class has
    >= min_joint_clear jointly-clear pixels.

    Parameters
    reference       : 's2' or 'landsat'
    target_date     : datetime.date
    pixels_df       : DataFrame with columns Pixel_ID, Lat, Long, MVG_class
    max_days        : int - search window +- days
    t_half          : int - temporal penalty half-life in days
    max_candidates  : int - max candidates to score (ranked by temporal weight)
    min_joint_clear : int - minimum jointly-clear pixels required per class.
                             Set to 0 to accept the best-scoring candidate
                             without a per-class check.

    Returns dict with keys:
        asset_id, sensor, date, days_diff, f_clear, score,
        clear_pixel_ids, window_extended
    or None if no candidate satisfies all classes within ±max_days.
    """
    n = len(pixels_df)
    if n == 0:
        return None

    start = (target_date - timedelta(days=max_days)).isoformat()
    end   = (target_date + timedelta(days=max_days + 1)).isoformat()

    sites_fc = _build_fc(pixels_df)
    lats = pixels_df['Lat'].values.astype(float)
    lons = pixels_df['Long'].values.astype(float)
    bbox = ee.Geometry.Rectangle([
        float(lons.min())-0.05, float(lats.min())-0.05,
        float(lons.max())+0.05, float(lats.max())+0.05,
    ])

    if reference == 's2':
        coll = (ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
                .filterDate(start, end).filterBounds(bbox))
        sensor_label = 'S2'
    else:
        l8 = (ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
              .filterDate(start, end).filterBounds(bbox))
        l9 = (ee.ImageCollection('LANDSAT/LC09/C02/T1_L2')
              .filterDate(start, end).filterBounds(bbox))
        coll = l8.merge(l9)
        sensor_label = 'LS'

    scene_ids = coll.aggregate_array('system:id').getInfo()
    if not scene_ids:
        print(f'    No {sensor_label} scenes within ±{max_days} days')
        return None

    ranked = []
    for aid in scene_ids:
        img = ee.Image(aid)
        ts  = img.date().millis().getInfo()
        sd  = datetime.utcfromtimestamp(ts / 1000).date()
        dd  = abs((sd - target_date).days)
        tw  = math.exp(-dd / t_half)
        if reference == 's2':
            sc = img.get('SPACECRAFT_NAME').getInfo() or ''
            sn = ('S2A' if '2A' in sc else 'S2B' if '2B' in sc
                  else 'S2C' if '2C' in sc else 'S2X')
        else:
            sn = 'L8' if 'LC08' in aid else 'L9'
        ranked.append(dict(asset_id=aid, sensor=sn, date=sd,
                           days_diff=dd, tw=tw))

    ranked.sort(key=lambda x: -x['tw'])
    ranked = ranked[:max_candidates]

    pid_to_class    = {row['Pixel_ID']: int(row['MVG_class'])
                       for _, row in pixels_df.iterrows()}
    retained_classes = sorted(int(c) for c in pixels_df['MVG_class'].unique())
    n_classes        = len(retained_classes)

    print(f'    {len(scene_ids)} {sensor_label} candidates - '
          f'scoring top {len(ranked)} (need {min_joint_clear} px/class '
          f'across {n_classes} classes):')

    for cand in ranked:
        img = ee.Image(cand['asset_id'])
        print(f'      {cand["sensor"]} {cand["date"]} '
              f'(d{cand["days_diff"]:>2d}) ... ', end='', flush=True)

        try:
            if reference == 's2':
                fc, cids = _score_pixels_s2(img, sites_fc, n)
            else:
                fc, cids = _score_pixels_landsat(img, sites_fc, n)
        except Exception as exc:
            print(f'FAILED ({exc})')
            continue

        score = fc * cand['tw']

        if min_joint_clear > 0:
            clear_per_class = {}
            for pid in cids:
                cls = pid_to_class.get(pid)
                if cls is not None:
                    clear_per_class[cls] = clear_per_class.get(cls, 0) + 1

            classes_ok = all(
                clear_per_class.get(cls, 0) >= min_joint_clear
                for cls in retained_classes
            )

            cls_summary = ' '.join(
                f'MVG{c}:{clear_per_class.get(c, 0)}'
                for c in retained_classes
            )
            status = 'ACCEPTED' if classes_ok else 'rejected'
            print(f'f_clear={fc:.2f}  score={score:.4f}  '
                  f'{status}  [{cls_summary}]')

            if classes_ok:
                print(f'   -> Selected: {cand["sensor"]} {cand["date"]} '
                      f'(d{cand["days_diff"]}, f_clear={fc:.2f})')
                return {**cand, 'f_clear': fc, 'score': score,
                        'clear_pixel_ids': cids, 'window_extended': False}

        else:
            print(f'f_clear={fc:.2f}  score={score:.4f}')
            if len(cids) > 0:
                print(f'   -> Selected: {cand["sensor"]} {cand["date"]} '
                      f'(d{cand["days_diff"]}, f_clear={fc:.2f})')
                return {**cand, 'f_clear': fc, 'score': score,
                        'clear_pixel_ids': cids, 'window_extended': False}

    print(f'[SKIP reason] No {sensor_label} candidate within ±{max_days} d '
          f'has >= {min_joint_clear} jointly-clear pixels for all '
          f'{n_classes} classes.')
    return None


def extract_reference_bands(asset_id, pixels_df, reference,
                             batch_size=2500):
    """
    Extract all reference sensor bands at every pixel in pixels_df.
    Returns dict: {Pixel_ID: np.array(float32, n_bands)} or None if invalid.
    S2: 10 bands in S2_BANDS order
    Landsat: 6 bands in LS_BANDS order
    """
    if reference == 's2':
        extract_bands = S2_BANDS
        scale         = S2_SCALE
        offset        = 0.0
        gee_scale     = 10
    else:
        extract_bands = LS_BANDS
        scale         = LS_SCALE
        offset        = LS_OFFSET
        gee_scale     = 30

    img  = ee.Image(asset_id)
    rows = list(pixels_df.iterrows())
    result = {}
    n = len(rows)

    for start in range(0, n, batch_size):
        batch = rows[start:start+batch_size]
        feats = []
        for _, row in batch:
            pt = ee.Geometry.Point(float(row['Long']), float(row['Lat']))
            feats.append(ee.Feature(pt, {'Pixel_ID': str(row['Pixel_ID'])}))
        fc  = ee.FeatureCollection(feats)
        sam = (img.select(extract_bands)
               .reduceRegions(collection=fc, reducer=ee.Reducer.first(),
                              scale=gee_scale)
               .getInfo())
        for feat in sam['features']:
            pid  = feat['properties']['Pixel_ID']
            vals = [feat['properties'].get(b) for b in extract_bands]
            if all(v is not None for v in vals):
                arr = np.array(vals, dtype=np.float32) * scale + offset
                arr = np.clip(arr, -0.1, 1.5)
                result[pid] = arr if np.all(np.isfinite(arr)) else None
            else:
                result[pid] = None

        if n > batch_size:
            print(f'     ... batch {start}–{start+len(batch)} / {n}')

    n_ok = sum(1 for v in result.values() if v is not None)
    print(f'  Extracted {n_ok}/{n} pixels from {asset_id.split("/")[-1]}')
    return result


# 7. BAND QUALITY UTILITIES
# ═════════════════════════════════════════════════════════════════════════

def check_contiguous_gaps(spectra, max_gap=3):
    """Return bool mask (n_px): True = pixel has no contiguous NaN run > max_gap."""
    nan_m = ~np.isfinite(spectra)
    valid = np.ones(len(spectra), dtype=bool)
    for i in range(len(spectra)):
        row = nan_m[i]
        if not row.any():
            continue
        cur, bad = 0, False
        for v in row:
            cur = cur+1 if v else 0
            if cur > max_gap:
                bad = True; break
        valid[i] = not bad
    return valid


def median_impute(X):
    """Column-wise median imputation; all-NaN columns changed to 0."""
    X = X.copy()
    for j in range(X.shape[1]):
        col = X[:, j]
        fm  = np.isfinite(col)
        X[~fm, j] = np.median(col[fm]) if fm.any() else 0.0
    return X


# 8. METRICS + AULC
# ══════════════════════════════════════════════════════════════════════════

def macro_specificity(y_true, y_pred, labels):
    cm = confusion_matrix(y_true, y_pred, labels=labels)
    specs = []
    for i in range(len(labels)):
        tp = cm[i,i]; fp = cm[:,i].sum()-tp
        fn = cm[i,:].sum()-tp; tn = cm.sum()-tp-fp-fn
        specs.append(tn/(tn+fp) if (tn+fp)>0 else 0.0)
    return float(np.mean(specs))


def compute_aulc(df, metric_col,
                  x_min=None, x_max=None):
    """
    AULC over a fixed x-axis reference range.
    x normalised to [0,1] relative to [x_min, x_max]. Scenes where
    valid_sizes do not span the full range return aulc_full_range=False;
    callers requiring strictly comparable values should filter on that flag.

    Returns (aulc_value: float, full_range: bool)
    """
    if x_min is None:
        x_min = AULC_X_MIN
    if x_max is None:
        x_max = AULC_X_MAX

    x = df['n_samples_per_class'].values
    y = df[metric_col].values
    order = np.argsort(x)
    x, y  = x[order], y[order]
    if len(x) < 2:
        return np.nan, False

    full_range = (int(x.min()) <= x_min) and (int(x.max()) >= x_max)

    xn = (x - x_min) / (x_max - x_min + 1e-9)
    xn = np.clip(xn, 0.0, 1.0)
    return float(np.trapz(y, xn)), full_range


def fit_rf(X_train, y_train, seed):
    clf = RandomForestClassifier(
        n_estimators=RF_TREES, max_features='sqrt',
        min_samples_leaf=5, class_weight='balanced',
        n_jobs=-1, random_state=seed)
    clf.fit(X_train, y_train)
    return clf


def eval_rf(clf, X_test, y_test, classes):
    pred = clf.predict(X_test)
    return {
        'oa':   float(accuracy_score(y_test, pred)),
        'kap':  float(cohen_kappa_score(y_test, pred)),
        'sens': float(recall_score(y_test, pred, labels=classes,
                                   average='macro', zero_division=0)),
        'spec': float(macro_specificity(y_test, pred, classes)),
    }


# 9. LEARNING CURVE
# ══════════════════════════════════════════════════════════════════════════

def run_learning_curve(X_hyp, X_ref_all, X_ref_vnir,
                       y, test_mask, classes,
                       sample_sizes, n_iter, seed,
                       scene_name='', cm_dir=None):
    """
    Train three RF classifiers at increasing training set sizes.
      hyp = X_hyp
      ref_all = X_ref_all
      ref_vnir = X_ref_vnir (may be same as ref_all if sensor has no SWIR restriction)

    test_mask : bool array, True = held-out test pixel (NOT used for training).
    Pool pixels = ~test_mask. Training draws n per class from pool.

    Returns DataFrame with one row per (n_samples_per_class, iteration).
    """
    X_tr_hyp   = X_hyp[~test_mask];   X_te_hyp   = X_hyp[test_mask]
    X_tr_all   = X_ref_all[~test_mask]; X_te_all   = X_ref_all[test_mask]
    X_tr_vnir  = X_ref_vnir[~test_mask]; X_te_vnir  = X_ref_vnir[test_mask]
    y_pool     = y[~test_mask]
    y_test     = y[test_mask]

    pool_counts  = {c: int((y_pool == c).sum()) for c in classes}
    min_pool     = min(pool_counts.values()) if pool_counts else 0
    valid_sizes  = [s for s in sample_sizes if s <= min_pool]
    if not valid_sizes:
        print(f'    [SKIP] Insufficient pool (min per class = {min_pool})')
        return None
    if len(valid_sizes) < len(sample_sizes):
        print(f'[INFO] Trimmed sample sizes to {valid_sizes}  '
              f'(min pool={min_pool})')

    records = []
    total   = len(valid_sizes) * n_iter
    ctr     = 0; t0 = time.time()
    cm_done = False

    for n_samp in valid_sizes:
        for it in range(n_iter):
            ctr += 1
            it_rng = np.random.RandomState(seed + n_samp*100 + it)
            train_idx = []
            ok = True
            for cls in classes:
                ci = np.where(y_pool == cls)[0]
                nd = min(n_samp, len(ci))
                if nd == 0:
                    ok = False; break
                train_idx.append(it_rng.choice(ci, size=nd, replace=False))
            if not ok:
                continue
            ti = np.concatenate(train_idx)

            # hyp
            m_hyp  = eval_rf(fit_rf(X_tr_hyp[ti], y_pool[ti], seed),
                              X_te_hyp, y_test, classes)
            # ref_all
            m_all  = eval_rf(fit_rf(X_tr_all[ti], y_pool[ti], seed),
                              X_te_all, y_test, classes)
            # ref_vnir
            m_vnir = eval_rf(fit_rf(X_tr_vnir[ti], y_pool[ti], seed),
                              X_te_vnir, y_test, classes)

            records.append({
                'n_samples_per_class': n_samp,
                'iteration':           it + 1,
                'hyp_oa':    m_hyp['oa'],  'hyp_kappa':    m_hyp['kap'],
                'hyp_sens':  m_hyp['sens'],'hyp_spec':     m_hyp['spec'],
                'ref_all_oa':m_all['oa'],  'ref_all_kappa':m_all['kap'],
                'ref_all_sens':m_all['sens'],'ref_all_spec':m_all['spec'],
                'ref_vnir_oa':m_vnir['oa'],'ref_vnir_kappa':m_vnir['kap'],
                'ref_vnir_sens':m_vnir['sens'],'ref_vnir_spec':m_vnir['spec'],
            })

            # Save confusion matrix once at max sample size
            if cm_dir and n_samp == max(valid_sizes) and it == 0 and not cm_done:
                os.makedirs(cm_dir, exist_ok=True)
                clf_cm = fit_rf(X_tr_hyp[ti], y_pool[ti], seed)
                pred_cm = clf_cm.predict(X_te_hyp)
                cm = confusion_matrix(y_test, pred_cm, labels=classes)
                cm_df = pd.DataFrame(cm,
                                     index=[f'true_{c}' for c in classes],
                                     columns=[f'pred_{c}' for c in classes])
                safe = scene_name.replace('/','_').replace('\\','_')
                cm_df.to_csv(os.path.join(cm_dir, f'{safe}_cm.csv'))
                cm_done = True

            eta = (time.time()-t0)/ctr*(total-ctr)
            best_lbl = ('hyp' if m_hyp['oa'] >= m_all['oa'] and
                         m_hyp['oa'] >= m_vnir['oa'] else
                        'ref_all' if m_all['oa'] >= m_vnir['oa'] else 'ref_vnir')
            print(f'      [{ctr:>2d}/{total}] n={n_samp:<3d} it={it+1}  '
                  f'hyp={100*m_hyp["oa"]:.1f}%  '
                  f'ref_all={100*m_all["oa"]:.1f}%  '
                  f'ref_vnir={100*m_vnir["oa"]:.1f}%  '
                  f'({best_lbl})  ~{eta:.0f}s')

    return pd.DataFrame(records) if records else None


# 10. MAIN
# ══════════════════════════════════════════════════════════════════════════

def main(sensor_name, sensor_dir, reference, nvis_raster,
         out_csv, out_summary, cm_dir, emit_mask_dir=None):

    sep = '='*70
    print(sep)
    print(f'  {sensor_name} vs {reference.upper()} - Learning Curve')
    print(f'  Reference      : {reference}')
    print(f'  T_half         : {T_HALF} d   max_days: ±{MAX_DAYS} d')
    print(f'  Spatial thin   : {int(MIN_DIST_PRIMARY)} m -> {int(MIN_DIST_FALLBACK)} m fallback')
    print(f'  Candidates/cls : {MAX_CANDIDATES_PC}  (pool_cap={POOL_CAP}  test={TEST_PER_CLASS}  min_joint={MIN_JOINT_CLEAR})')
    print(f'  Sample sizes   : {SAMPLE_SIZES}')
    print(f'  Min class px   : {MIN_CLASS_CANDIDATES}')
    print(f'  Min pool/class : {MIN_POOL_PER_CLASS}')
    print(f'  Min n_classes  : {MIN_N_CLASSES}')
    print(f'  AULC ref range : [{AULC_X_MIN}, {AULC_X_MAX}] (fixed for cross-scene comparability)')
    print(f'  Out CSV        : {out_csv}')
    print(f'  Out summary    : {out_summary}')
    print(sep)

    if not init_gee():
        sys.exit(1)

    # Locate scene files ────────────────────────────────────────────────
    if sensor_name == 'PRISMA':
        scene_files = sorted(Path(sensor_dir).glob('*.zip'))
        parse_date  = parse_prisma_date
        read_sensor = read_prisma_from_zip
    elif sensor_name == 'DESIS':
        scene_files = sorted(Path(sensor_dir).rglob('*-SPECTRAL_IMAGE.tif'))
        parse_date  = parse_desis_date
        read_sensor = read_desis
    elif sensor_name == 'EMIT':
        all_nc = sorted(Path(sensor_dir).rglob('*.nc'))
        scene_files = [p for p in all_nc
                       if '_RFL_' in p.name.upper()
                       and 'UNCERT' not in p.name.upper()
                       and 'MASK'   not in p.name.upper()]
        parse_date  = parse_emit_date
        _emit_mask_dir = emit_mask_dir
        read_sensor = lambda p: read_emit(
            p, mask_dir=_emit_mask_dir,
            uncert_thresh=EMIT_UNCERT_THRESH)
    elif sensor_name == 'WYVERN':
        scene_files = sorted(Path(sensor_dir).rglob('*_l2a.tiff'))
        parse_date  = parse_wyvern_date
        read_sensor = read_wyvern
    else:
        raise ValueError(f'Unknown sensor: {sensor_name}')

    if not scene_files:
        raise FileNotFoundError(f'No scenes found in {sensor_dir} for {sensor_name}')
    print(f'\n[INFO] Found {len(scene_files)} {sensor_name} scene(s)')

    # Resume state ──────────────────────────────────────────────────────
    processed_scenes = set()
    if Path(out_csv).exists():
        prev = pd.read_csv(out_csv, usecols=['scene_name'])
        processed_scenes = set(prev['scene_name'].unique())
        print(f'[INFO] Resuming - {len(processed_scenes)} scenes already processed')

    all_results  = []
    all_summaries = []
    rng_master   = np.random.RandomState(SEED)

    # Scene loop ────────────────────────────────────────────────────────
    for sfile in scene_files:
        scene_name = sfile.stem
        s_date     = parse_date(sfile)
        print(f'\n{"-"*70}')
        print(f'{scene_name}  |  date: {s_date}')

        if scene_name in processed_scenes:
            print('  [SKIP] Already processed (resume)')
            continue

        try:
            # 1. Read sensor ────────────────────────────────────────────
            p_refl, p_waves, p_transform, p_crs, p_nodata = read_sensor(
                str(sfile))
            rows, cols, n_bands_raw = p_refl.shape
            print(f'  Cube: {rows}×{cols}  |  {n_bands_raw} bands  |  {p_crs}')

            # 2. Pixel quality filter ───────────────────────────────────
            if sensor_name == 'EMIT':
                finite_frac = np.sum(np.isfinite(p_refl), axis=2) / n_bands_raw
                basic_ok    = finite_frac >= EMIT_MIN_FINITE_FRAC
                ngc_px      = basic_ok & (finite_frac < 1.0)
                if ngc_px.any():
                    ngc_r, ngc_c = np.where(ngc_px)
                    gap_ok = check_contiguous_gaps(
                        p_refl[ngc_r, ngc_c, :], EMIT_MAX_CONTIGUOUS_NAN)
                    gap_bad = np.zeros((rows, cols), dtype=bool)
                    gap_bad[ngc_r, ngc_c] = ~gap_ok
                else:
                    gap_bad = np.zeros((rows, cols), dtype=bool)
                p_valid = ~p_nodata & basic_ok & ~gap_bad
            else:
                p_valid = ~p_nodata & np.all(np.isfinite(p_refl), axis=2)

            # 3. Pixel coordinates + NVIS ───────────────────────────────
            rr, cc = np.mgrid[0:rows, 0:cols]
            xs, ys = rasterio.transform.xy(
                p_transform, rr.ravel(), cc.ravel(), offset='center')
            xs = np.array(xs, dtype=np.float64)
            ys = np.array(ys, dtype=np.float64)

            nvis_vals = sample_nvis_at_points(nvis_raster, xs, ys, p_crs)
            nvis_vals = nvis_vals.reshape(rows, cols)

            nvis_ok       = ~np.isin(nvis_vals, list(NVIS_EXCLUDE))
            candidate_mask = p_valid & nvis_ok
            n_cand         = int(candidate_mask.sum())
            print(f'  Hyperspec-clear + NVIS-veg candidates: {n_cand:,}')

            if n_cand < MIN_CLASS_CANDIDATES:
                print(f'  [SKIP] Too few candidates')
                continue

            # 4. Class census ───────────────────────────────────────────
            cand_flat  = np.where(candidate_mask.ravel())[0]
            cand_rr_   = cand_flat // cols
            cand_cc_   = cand_flat %  cols
            cand_cls_  = nvis_vals[cand_rr_, cand_cc_]
            cand_xs_   = xs[cand_flat]
            cand_ys_   = ys[cand_flat]

            class_counts = pd.Series(cand_cls_).value_counts()
            retained_cls = [int(c) for c,n in class_counts.items()
                            if n >= MIN_CLASS_CANDIDATES]
            dropped_cls  = [int(c) for c,n in class_counts.items()
                            if n < MIN_CLASS_CANDIDATES]

            print(f'  Class census: {len(class_counts)} classes present')
            for c, n in sorted(class_counts.items(), key=lambda x: -x[1]):
                tag = 'KEEP' if c in retained_cls else f'DROP (<{MIN_CLASS_CANDIDATES})'
                print(f'    MVG {int(c):>3d}: {n:>7,} px  [{tag}]')

            if len(retained_cls) < 2:
                print(f'  [SKIP] Fewer than 2 classes with ≥{MIN_CLASS_CANDIDATES} candidates')
                continue

            keep_mask  = np.isin(cand_cls_, retained_cls)
            cand_rr_   = cand_rr_[keep_mask]
            cand_cc_   = cand_cc_[keep_mask]
            cand_cls_  = cand_cls_[keep_mask]
            cand_xs_   = cand_xs_[keep_mask]
            cand_ys_   = cand_ys_[keep_mask]

            # 5. Adaptive spatial sampling (over-sampled for cloud buffer)
            print(f'  Adaptive spatial sampling '
                  f'(target {MAX_CANDIDATES_PC}/class, '
                  f'{int(MIN_DIST_PRIMARY)} m -> {int(MIN_DIST_FALLBACK)} m):')
            thinned_df, class_info = adaptive_spatial_sample(
                cand_rr_, cand_cc_, cand_xs_, cand_ys_,
                cand_cls_, p_crs,
                MAX_CANDIDATES_PC, MIN_DIST_PRIMARY, MIN_DIST_FALLBACK,
                MIN_JOINT_CLEAR, SEED)

            if thinned_df.empty:
                print('  [SKIP] No classes passed spatial thinning')
                continue

            retained_after_thin = sorted(thinned_df['MVG_class'].unique())
            print(f'  Retained {len(retained_after_thin)} classes after thinning '
                  f'({len(thinned_df)} total candidates)')

            if len(retained_after_thin) < 2:
                print('  [SKIP] Fewer than 2 classes after spatial thinning')
                continue

            # 6. Find best reference scene (adaptive)
            print(f'  Searching GEE for best {reference.upper()} scene '
                  f'(adaptive, min {MIN_JOINT_CLEAR} px/class):')
            best_ref = find_best_scene(
                reference, s_date, thinned_df,
                max_days=MAX_DAYS, t_half=T_HALF,
                min_joint_clear=MIN_JOINT_CLEAR)

            if best_ref is None:
                print('  [SKIP] No reference scene satisfied all class '
                      'thresholds within the search window')
                continue

            # 7. Extract reference bands at accepted candidates
            print(f'  Extracting {reference.upper()} bands at '
                  f'{len(thinned_df)} pixels...')
            ref_bands = extract_reference_bands(
                best_ref['asset_id'], thinned_df, reference)

            # 8. Build joint-clear pixel pool
            # The adaptive scene search verified every class meets MIN_JOINT_CLEAR.
            # This step subsets to pixels clear in SCL/QA *and* with finite
            # extracted band values. Stage 9 is a safety net for edge cases.
            clear_set  = set(best_ref['clear_pixel_ids'])
            joint_mask = np.array([
                (row['Pixel_ID'] in clear_set) and
                (ref_bands.get(row['Pixel_ID']) is not None)
                for _, row in thinned_df.iterrows()
            ], dtype=bool)

            n_joint = int(joint_mask.sum())
            print(f'  Joint-clear pixels: {n_joint} / {len(thinned_df)}')

            dual_df = thinned_df[joint_mask].reset_index(drop=True)

            # 9. Safety-net class filter
            # Catches pixels lost when extraction returned None
            # (e.g. pixel fell outside the S2 tile footprint despite SCL scoring).
            # If this fires repeatedly, investigate tile coverage.
            combined_thresh = TEST_PER_CLASS + MIN_POOL_PER_CLASS # = 150

            class_joint_counts = dual_df['MVG_class'].value_counts()
            valid_classes = sorted(
                int(c) for c, n in class_joint_counts.items()
                if n >= combined_thresh)
            dropped_jc = sorted(
                int(c) for c, n in class_joint_counts.items()
                if n < combined_thresh)

            n_pool_filtered = sum(
                1 for c, n in class_joint_counts.items()
                if TEST_PER_CLASS <= n < combined_thresh)

            if dropped_jc:
                print(f'  [SAFETY NET] Dropped MVG {dropped_jc} after '
                      f'extraction (< {combined_thresh} px; '
                      f'{n_pool_filtered} marginal). '
                      f'Investigate tile coverage if this occurs repeatedly.')
            if len(valid_classes) < MIN_N_CLASSES:
                print(f'  [SKIP] Fewer than {MIN_N_CLASSES} valid classes '
                      f'after safety-net filter (got {len(valid_classes)}). '
                      f'Extraction losses exceeded adaptive-search guarantee.')
                continue

            dual_df = dual_df[dual_df['MVG_class'].isin(valid_classes)
                              ].reset_index(drop=True)
            classes_arr = np.array(sorted(valid_classes))
            print(f'  Final classes: {list(classes_arr)}  '
                  f'({len(dual_df)} dual-clear pixels)')

            # 10. Build feature matrices ────────────────────────────────
            # Hyperspectral features
            pid_to_rc = {row['Pixel_ID']: (row['row'], row['col'])
                         for _, row in dual_df.iterrows()}
            all_rc    = np.array([pid_to_rc[pid] for pid in dual_df['Pixel_ID']])
            X_hyp_raw = p_refl[all_rc[:,0], all_rc[:,1], :]  # (n_dual, n_bands_raw)

            # Drop all-NaN bands, then impute sparse NaNs
            good_bands = ~np.all(np.isnan(X_hyp_raw), axis=0)
            X_hyp = median_impute(X_hyp_raw[:, good_bands])
            n_hyp_bands = X_hyp.shape[1]
            if n_hyp_bands == 0:
                print('  [SKIP] All hyperspectral bands are NaN')
                continue

            # Reference features
            ref_arr = np.array([ref_bands[pid]
                                 for pid in dual_df['Pixel_ID']])   # (n, n_ref)

            if reference == 's2':
                X_ref_all  = ref_arr[:, S2_ALL_IDX]
                X_ref_vnir = ref_arr[:, S2_VNIR_IDX]
            else:
                X_ref_all  = ref_arr[:, LS_ALL_IDX]
                X_ref_vnir = ref_arr[:, LS_VNIR_IDX]

            y = dual_df['MVG_class'].values.astype(int)

            # 11. Test / pool split
            # From each class: first TEST_PER_CLASS jointly-clear pixels -> test
            # (already spatially thinned); remainder (up to POOL_CAP) -> pool
            test_mask = np.zeros(len(dual_df), dtype=bool)
            for cls in valid_classes:
                cls_idx = np.where(y == cls)[0]
                test_idx = cls_idx[:TEST_PER_CLASS]
                test_mask[test_idx] = True

            n_test = int(test_mask.sum())
            n_pool = int((~test_mask).sum())
            print(f'  Test: {n_test} | Pool: {n_pool}')

            if n_pool < 1:
                print('  [SKIP] Empty pool')
                continue

            # Pool cap per class
            pool_indices = np.where(~test_mask)[0]
            pool_mask_final = np.zeros(len(dual_df), dtype=bool)
            for cls in valid_classes:
                cls_pool = pool_indices[y[pool_indices] == cls][:POOL_CAP]
                pool_mask_final[cls_pool] = True
            # Rebuild final test / pool (test_mask unchanged, pool respects cap)
            final_use = test_mask | pool_mask_final
            X_hyp      = X_hyp[final_use]
            X_ref_all  = X_ref_all[final_use]
            X_ref_vnir = X_ref_vnir[final_use]
            y          = y[final_use]
            test_mask  = test_mask[final_use]

            # 12. Run learning curve ─────────────────────────────────────
            print(f'  Running learning curve  '
                  f'(hyp={n_hyp_bands}b  ref_all={X_ref_all.shape[1]}b  '
                  f'ref_vnir={X_ref_vnir.shape[1]}b):')
            lc_df = run_learning_curve(
                X_hyp, X_ref_all, X_ref_vnir,
                y, test_mask, classes_arr,
                SAMPLE_SIZES, N_ITERATIONS, SEED,
                scene_name=scene_name,
                cm_dir=cm_dir)

            if lc_df is None:
                print('  [SKIP] Learning curve returned no results')
                continue

            # 13. Attach metadata ───────────────────────────────────────
            min_dists = [class_info.get(c, {}).get('min_dist_m', MIN_DIST_PRIMARY)
                         for c in valid_classes]
            meta = dict(
                scene_name   = scene_name,
                sensor       = sensor_name,
                reference    = reference,
                scene_date   = str(s_date),
                ref_sensor   = best_ref['sensor'],
                ref_date     = str(best_ref['date']),
                ref_days_diff= int(best_ref['days_diff']),
                ref_f_clear  = round(float(best_ref['f_clear']), 4),
                ref_window_extended = bool(best_ref.get('window_extended', False)),
                n_classes    = len(valid_classes),
                classes_list = ','.join(str(c) for c in valid_classes),
                actual_min_dist_m = ','.join(f'{int(d)}' for d in min_dists),
                n_test_total = int(test_mask.sum()),
                n_pool_total = int((~test_mask).sum()),
                n_hyp_bands  = n_hyp_bands,
                n_ref_all_bands  = int(X_ref_all.shape[1]),
                n_ref_vnir_bands = int(X_ref_vnir.shape[1]),
                n_pool_filtered_classes = n_pool_filtered,
                min_pool_per_class = MIN_POOL_PER_CLASS,
                min_joint_clear    = MIN_JOINT_CLEAR,
                min_n_classes      = MIN_N_CLASSES,
            )
            for k, v in meta.items():
                lc_df[k] = v

            all_results.append(lc_df)

            # 14. Summary (AULC per metric, fixed reference range) ──────
            # Primary metric is kappa; OA is secondary.
            summary = dict(**meta)
            for prefix in ['hyp', 'ref_all', 'ref_vnir']:
                for met in ['oa', 'kappa']:
                    col = f'{prefix}_{met}'
                    if col in lc_df.columns:
                        aulc_val, full_rng = compute_aulc(lc_df, col)
                        summary[f'{prefix}_aulc_{met}']       = round(aulc_val, 4)
                        summary[f'{prefix}_aulc_{met}_full']  = bool(full_rng)
                        max_n    = lc_df['n_samples_per_class'].max()
                        max_rows = lc_df[lc_df['n_samples_per_class'] == max_n]
                        summary[f'{prefix}_max_{met}'] = round(
                            float(max_rows[col].mean()), 4)
            summary['aulc_x_min'] = AULC_X_MIN
            summary['aulc_x_max'] = AULC_X_MAX
            all_summaries.append(summary)

            # 15. Flush to CSV ──────────────────────────────────────────
            out_df = pd.concat(all_results, ignore_index=True)
            out_df.to_csv(out_csv, index=False)

            sum_df = pd.DataFrame(all_summaries)
            sum_df.to_csv(out_summary, index=False)

            print(f'  Saved {len(lc_df)} rows -> {out_csv}')

        except Exception as e:
            _log_exc(scene_name, e)
            continue

    # save ────────────────────────────────────────────────────────
    if all_results:
        pd.concat(all_results, ignore_index=True).to_csv(out_csv, index=False)
        pd.DataFrame(all_summaries).to_csv(out_summary, index=False)
        print(f'\n{sep}')
        print(f'[DONE] {len(all_summaries)} scene(s) processed')
        print(f'       Results -> {out_csv}')
        print(f'       Summary -> {out_summary}')
    else:
        print('\n[WARNING] No scenes produced results')


# 11. ARGPARSE + MAIN
# ═════════════════════════════════════════════════════════════════════════

def _parse_args():
    ap = argparse.ArgumentParser(
        description='Unified hyperspectral learning-curve classifier')
    ap.add_argument('--sensor',
                    default=SENSOR_NAME,
                    choices=['PRISMA','DESIS','EMIT','WYVERN'],
                    help='Hyperspectral sensor name')
    ap.add_argument('--sensor_dir',
                    default=SENSOR_DIR,
                    help='Directory containing sensor scene files')
    ap.add_argument('--reference',
                    default=REFERENCE,
                    choices=['s2','landsat'],
                    help='Multispectral reference sensor: s2 or landsat')
    ap.add_argument('--nvis_raster',
                    default=NVIS_RASTER,
                    help='Path to NVIS v7 MVG GeoTIFF')
    ap.add_argument('--out_csv',
                    default=None,
                    help='Output results CSV (default: lc_{sensor}_{ref}_results.csv)')
    ap.add_argument('--out_summary',
                    default=None,
                    help='Output summary CSV (default: lc_{sensor}_{ref}_summary.csv)')
    ap.add_argument('--cm_dir',
                    default=None,
                    help='Directory for confusion matrix CSVs (omit to skip)')
    ap.add_argument('--emit_mask_dir',
                    default=None,
                    help='Directory containing EMIT L2A MASK V002 .nc files.'
                         'If omitted, the script searches alongside each RFL file.')
    ap.add_argument('--t_half',  type=int, default=T_HALF,
                    help='Temporal penalty half-life in days (default %(default)s)')
    ap.add_argument('--max_days', type=int, default=MAX_DAYS,
                    help='Reference scene search window ± days (default %(default)s).'
                         'Increase to 45 if too many scenes are lost at ±30 d.')
    ap.add_argument('--seed', type=int, default=SEED)
    ap.add_argument('--min_pool_per_class', type=int, default=MIN_POOL_PER_CLASS,
                    help='Min training pool pixels per class after joint-clear '
                         '(default %(default)s). MIN_JOINT_CLEAR is derived as '
                         'TEST_PER_CLASS + this value.')
    ap.add_argument('--min_n_classes', type=int, default=MIN_N_CLASSES,
                    help='Min MVG classes required per scene (default %(default)s)')
    return ap.parse_args()


if __name__ == '__main__':
    args = _parse_args()

    sn  = args.sensor
    ref = args.reference

    out_csv_ = args.out_csv     or f'./lc_{sn.lower()}_{ref}_results.csv'
    out_sum_ = args.out_summary or f'./lc_{sn.lower()}_{ref}_summary.csv'
    cm_dir_  = args.cm_dir      or f'./cm_{sn.lower()}_{ref}'

    T_HALF             = args.t_half
    MAX_DAYS           = args.max_days
    SEED               = args.seed
    MIN_POOL_PER_CLASS = args.min_pool_per_class
    MIN_N_CLASSES      = args.min_n_classes
    # Re-derive if min_pool_per_class was overridden via CLI
    MIN_JOINT_CLEAR    = TEST_PER_CLASS + MIN_POOL_PER_CLASS

    log_path = f'./processing_log_lc_{sn.lower()}_{ref}.txt'
    os.makedirs(os.path.dirname(log_path) or '.', exist_ok=True)

    old_stdout = sys.stdout
    with open(log_path, 'w', encoding='utf-8') as log_f:
        tee = Tee(old_stdout, log_f)
        sys.stdout = tee
        try:
            main(sn, args.sensor_dir, ref, args.nvis_raster,
                 out_csv_, out_sum_, cm_dir_,
                 emit_mask_dir=args.emit_mask_dir)
        except Exception as e:
            print(f'\n[FATAL] {e}')
            traceback.print_exc()
            raise
        finally:
            sys.stdout = old_stdout
