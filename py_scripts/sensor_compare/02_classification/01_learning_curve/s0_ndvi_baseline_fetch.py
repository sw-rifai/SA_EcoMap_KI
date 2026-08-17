"""
Fetch the Landsat NDVI baseline

Stage 1 
loads NVIS, filters to pure core nvis pixels 
(a pixel is pure-core only where all eight neighbours share its class)
and counts pure-core pixels per MVG class inside burned NP. 
A class earns a baseline only if it clears the pure-core floor (default 20). 

Stage 2 
fetch the monthly MVG class-stratified NDVI series only for the surviving classes, 
over pure-core pixels, so no GEE effort is spent on classes that can never be trained.

The NP floor (20) is not the learning-curve floor. 
s1 needs 100 train + 50 test pure-core pixels in the sensor scene, 
which may extend beyond NP. NP is only the focus region for the de-char baseline, 
so its bar is lower. Both count the same unit: pure-core pixels after the same 3x3 filter.

excluded MVG (0, 24, 25, 27, 28, 99) is non-vegetation or invalid. 
Non-thematic (26, 30) are unclassified catch-alls, not valid targets.
"""

import argparse
from pathlib import Path

import numpy as np
import pandas as pd

NVIS_ASSET = "projects/kangaroo-island-ecology/assets/nvis_mvg_rast_cropped_ki"
BURNED_FC = "projects/kangaroo-island-ecology/assets/burned_np"

# Non-vegetation or invalid codes. Never a target, regardless of pixel count.
MVG_BLACKLIST = [0, 24, 25, 27, 28, 99]
# Native vegetation, but non-thematic unclassified catch-alls. Not valid targets.
MVG_NONTHEMATIC = [26, 30]

MVG_LABELS = {
    5: "Eucalypt Woodlands", 8: "Casuarina Forests", 11: "Eucalypt Open Woodlands",
    13: "Acacia Open Woodlands", 14: "Mallee Woodlands", 15: "Low Closed Forests",
    16: "Acacia Shrublands", 17: "Other Shrublands", 18: "Heathlands",
    22: "Chenopod Shrublands", 31: "Other Open Woodlands", 32: "Mallee Open Woodlands",
}


def parse_args():
    ap = argparse.ArgumentParser(
        description="Fetch the pure-core-gated stratified Landsat NDVI baseline (GEE).")
    ap.add_argument("--outdir", default="outputs/ndvi_baseline")
    ap.add_argument("--out_csv", default="landsat_ndvi_baseline_stratified_burnedNP.csv")
    ap.add_argument("--census_csv", default="pure_core_census_burnedNP.csv")
    ap.add_argument("--nvis_asset", default=NVIS_ASSET)
    ap.add_argument("--burned_fc", default=BURNED_FC)
    ap.add_argument("--start_date", default="2014-01-01")
    ap.add_argument("--end_date", default="2026-01-01")
    ap.add_argument("--scale", type=float, default=None,
                    help="Reduce scale in metres. Default None uses the NVIS native scale, "
                         "so pure-core counts are in NVIS pixels.")
    ap.add_argument("--min_pure_core_np", type=int, default=20,
                    help="A class needs at least this many pure-core pixels in burned NP.")
    ap.add_argument("--keep_nonthematic", action="store_true",
                    help="Do not exclude the unclassified catch-all codes (MVG 26, 30).")
    ap.add_argument("--baseline_pixels", choices=["pure_core", "all"], default="pure_core",
                    help="Compute the monthly baseline over pure-core pixels (default) or "
                         "all class pixels in NP.")
    return ap.parse_args()


# --- Pure-core logic, testable without GEE -------------------------------------
# A pixel is pure-core where the 3x3 neighbourhood min equals its max, 
# with masked neighbours filled by a sentinel so any real edge breaks homogeneity.

def _shift(a, dy, dx, fill):
    """Return a where each cell holds the neighbour at offset (dy, dx), edge-filled."""
    out = np.full_like(a, fill)
    y0d, y1d = max(0, -dy), a.shape[0] - max(0, dy)
    x0d, x1d = max(0, -dx), a.shape[1] - max(0, dx)
    y0s, y1s = max(0, dy), a.shape[0] - max(0, -dy)
    x0s, x1s = max(0, dx), a.shape[1] - max(0, -dx)
    out[y0d:y1d, x0d:x1d] = a[y0s:y1s, x0s:x1s]
    return out


def pure_core_mask_np(arr, nodata):
    """Boolean mask where all eight neighbours are valid and share the centre class."""
    valid = arr != nodata
    core = valid.copy()
    for dy in (-1, 0, 1):
        for dx in (-1, 0, 1):
            if dy == 0 and dx == 0:
                continue
            nb = _shift(arr, dy, dx, nodata)
            core &= (nb != nodata) & (nb == arr)
    return core


def pure_core_census_np(arr, nodata, exclude):
    """Per-class (total, pure-core) pixel counts, excluded classes omitted."""
    core = pure_core_mask_np(arr, nodata)
    out = {}
    for c in np.unique(arr[arr != nodata]):
        c = int(c)
        if c in exclude:
            continue
        out[c] = (int(np.sum(arr == c)), int(np.sum(core & (arr == c))))
    return out


# --- GEE stages ---

def compute_census(nvis_asset, burned_fc, exclude, min_core, scale):
    """Stage 1. Pure-core census per class in burned NP. Returns (viable, census_df, scale)."""
    import ee
    try:
        ee.Initialize()
    except Exception:
        ee.Authenticate()
        ee.Initialize()

    nvis = ee.Image(nvis_asset)
    band = nvis.bandNames().get(0)
    mvg = nvis.select([band]).rename("mvg").toInt()
    proj = mvg.projection()
    native = proj.nominalScale().getInfo()
    use_scale = float(scale) if scale is not None else float(native)

    burned_geom = ee.FeatureCollection(burned_fc).geometry()
    k = ee.Kernel.square(radius=1, units="pixels")

    # Homogeneous 3x3 where min equals max.
    filled = mvg.unmask(-1).reproject(proj)
    fmin = filled.reduceNeighborhood(ee.Reducer.min(), k)
    fmax = filled.reduceNeighborhood(ee.Reducer.max(), k)
    is_core = fmin.eq(fmax).reproject(proj)

    # Class labels, excluded codes removed. remap sends excluded -> 0, else -> 1.
    allowed = mvg.remap(exclude, [0] * len(exclude), 1)
    core_allowed = mvg.updateMask(is_core).updateMask(allowed)
    total_allowed = mvg.updateMask(allowed)

    def hist(img):
        d = img.reduceRegion(reducer=ee.Reducer.frequencyHistogram(),
                             geometry=burned_geom, scale=use_scale, maxPixels=1e10)
        raw = ee.Dictionary(d.get("mvg")).getInfo() or {}
        return {int(round(float(kk))): int(round(float(vv))) for kk, vv in raw.items()}

    core_counts = hist(core_allowed)
    total_counts = hist(total_allowed)

    rows = []
    for c in sorted(set(core_counts) | set(total_counts)):
        pc = core_counts.get(c, 0)
        rows.append({"mvg": c, "label": MVG_LABELS.get(c, ""),
                     "total_px_np": total_counts.get(c, 0), "pure_core_px_np": pc,
                     "viable": bool(pc >= min_core)})
    census = pd.DataFrame(rows).sort_values("pure_core_px_np", ascending=False)
    viable = sorted(census.loc[census["viable"], "mvg"].astype(int).tolist())
    return viable, census, use_scale


def fetch_baseline(nvis_asset, burned_fc, viable, exclude, baseline_pixels,
                   start, end, scale):
    """Stage 2. Monthly stratified NDVI over the viable classes only."""
    import ee

    nvis = ee.Image(nvis_asset)
    band = nvis.bandNames().get(0)
    mvg = nvis.select([band]).rename("mvg").toInt()
    proj = mvg.projection()
    burned_geom = ee.FeatureCollection(burned_fc).geometry()

    filled = mvg.unmask(-1).reproject(proj)
    k = ee.Kernel.square(radius=1, units="pixels")
    is_core = filled.reduceNeighborhood(ee.Reducer.min(), k).eq(
        filled.reduceNeighborhood(ee.Reducer.max(), k)).reproject(proj)

    viable_mask = mvg.remap(viable, [1] * len(viable), 0)
    if baseline_pixels == "pure_core":
        sample_mvg = mvg.updateMask(is_core).updateMask(viable_mask)
    else:
        sample_mvg = mvg.updateMask(viable_mask)

    def prep(image):
        qa = image.select("QA_PIXEL")
        clean = (qa.bitwiseAnd(1 << 1).eq(0)
                 .And(qa.bitwiseAnd(1 << 2).eq(0))
                 .And(qa.bitwiseAnd(1 << 3).eq(0))
                 .And(qa.bitwiseAnd(1 << 4).eq(0)))
        opt = image.select(["SR_B4", "SR_B5"]).multiply(0.0000275).add(-0.2)
        ndvi = opt.normalizedDifference(["SR_B5", "SR_B4"]).rename("NDVI")
        return ndvi.addBands(sample_mvg).updateMask(clean)

    def month_coll(a, b):
        l8 = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2").filterBounds(burned_geom).filterDate(a, b)
        l9 = ee.ImageCollection("LANDSAT/LC09/C02/T1_L2").filterBounds(burned_geom).filterDate(a, b)
        return l8.merge(l9).map(prep)

    end_excl = pd.Timestamp(end) - pd.Timedelta(days=1)
    months = pd.period_range(pd.Timestamp(start).to_period("M"),
                             end_excl.to_period("M"), freq="M")

    rows = []
    for i, p in enumerate(months):
        s = ee.Date(f"{p.year}-{p.month:02d}-01")
        e = s.advance(1, "month")
        coll = month_coll(s, e)
        n_scenes = coll.size().getInfo()
        if n_scenes == 0:
            continue
        med = coll.median()
        gm = ee.Reducer.median().group(groupField=1, groupName="mvg")
        gc = ee.Reducer.count().group(groupField=1, groupName="mvg")
        d_med = ee.Dictionary(med.select(["NDVI", "mvg"]).reduceRegion(
            reducer=gm, geometry=burned_geom, scale=scale, maxPixels=1e10, bestEffort=False))
        d_cnt = ee.Dictionary(med.select(["NDVI", "mvg"]).reduceRegion(
            reducer=gc, geometry=burned_geom, scale=scale, maxPixels=1e10, bestEffort=False))
        stats_med = d_med.get("groups").getInfo()
        stats_cnt = d_cnt.get("groups").getInfo()
        cnt_lut = {it["mvg"]: it["count"] for it in stats_cnt if "mvg" in it}
        for it in stats_med:
            mvg_id = it.get("mvg")
            rows.append({
                "date": f"{p.year}-{p.month:02d}-15", "year": p.year, "month": p.month,
                "mvg": mvg_id, "n_scenes": n_scenes,
                "valid_pixels": cnt_lut.get(mvg_id, 0), "median_ndvi": it.get("median"),
            })
        if (i + 1) % 12 == 0 or (i + 1) == len(months):
            print(f"  [{i + 1}/{len(months)}] fetched to {p.year}-{p.month:02d}")

    if not rows:
        return pd.DataFrame()
    df = pd.DataFrame(rows)
    df["date"] = pd.to_datetime(df["date"])
    df["mvg"] = df["mvg"].astype(int)
    df["n_scenes"] = df["n_scenes"].astype(int)
    df["valid_pixels"] = df["valid_pixels"].astype(int)
    df["median_ndvi"] = pd.to_numeric(df["median_ndvi"], errors="coerce")
    return df


def main():
    args = parse_args()
    if args.selftest:
        selftest()
        return

    outdir = Path(args.outdir)
    outdir.mkdir(parents=True, exist_ok=True)
    exclude = list(MVG_BLACKLIST) + ([] if args.keep_nonthematic else list(MVG_NONTHEMATIC))

    print("Stage 1: pure-core viability census over burned NP (cheap, pre-fetch)...")
    viable, census, use_scale = compute_census(
        args.nvis_asset, args.burned_fc, exclude, args.min_pure_core_np, args.scale)

    census_out = outdir / args.census_csv
    census.to_csv(census_out, index=False, float_format="%.6f")
    print(f"  NVIS reduce scale: {use_scale:.1f} m  |  pure-core floor: {args.min_pure_core_np}")
    print(f"  excluded (semantic): {sorted(exclude)}")
    print("  class census (total px, pure-core px, viable):")
    for _, r in census.iterrows():
        star = "keep" if r["viable"] else "DROP"
        print(f"    MVG {int(r['mvg']):2d} {r['label']:<24} "
              f"total={int(r['total_px_np']):8d}  pure_core={int(r['pure_core_px_np']):7d}  [{star}]")
    print(f"  viable classes: {viable}")
    if not viable:
        print("No class cleared the pure-core floor. Nothing to fetch.")
        return

    print(f"\nStage 2: fetching monthly NDVI for {len(viable)} viable classes "
          f"({args.baseline_pixels} pixels)...")
    df = fetch_baseline(args.nvis_asset, args.burned_fc, viable, exclude,
                        args.baseline_pixels, args.start_date, args.end_date, use_scale)
    if df.empty:
        print("Warning: no monthly records retrieved.")
        return

    cols = ["date", "year", "month", "mvg", "n_scenes", "valid_pixels", "median_ndvi"]
    out = outdir / args.out_csv
    df[cols].to_csv(out, index=False, float_format="%.6f")
    print(f"\nBaseline cache: {out.resolve()}")
    print(f"Census:         {census_out.resolve()}")
    print(f"Rows: {len(df)}  |  classes fetched: {sorted(df['mvg'].unique().tolist())}")


if __name__ == "__main__":
    main()
