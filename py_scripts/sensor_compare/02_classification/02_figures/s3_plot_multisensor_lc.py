#!/usr/bin/env python3
"""
Multi-sensor learning curve comparison figure.

Reads one results CSV per sensor and produces a 2x2 facet figure 
comparing mean +- SD kappa (or OA / macro-F1) learning curves 
across all four sensors against a common multispectral reference.

Inputs
------
  --prisma / --desis / --emit / --wyvern : per-sensor results csv
  --out         : output directory
  --metric      : kappa | oa | f1 (default: kappa)
  --min_classes : exclude scenes with fewer than N MVG classes (default 3)
  --ref_all_only: omit VNIR-only reference baseline

Usage
-----
  python s3_plot_multisensor_lc.py \\
      --prisma  ./lc_prisma_s2_results.csv \\
      --desis   ./lc_desis_s2_results.csv \\
      --emit    ./lc_emit_s2_results.csv \\
      --wyvern  ./lc_wyvern_s2_results.csv \\
      --out     ./figures_multisensor \\
      --metric  kappa \\
      --min_classes 3
"""

import argparse
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
from pathlib import Path

# ─── colours ─────────────────────────────────────────────────────────────────
C_HYP      = '#5B8C5A'
C_REF_ALL  = '#2F4F6F'
C_REF_VNIR = '#CD853F'

SENSOR_ORDER = ['PRISMA', 'DESIS', 'EMIT', 'WYVERN']

SENSOR_META = {
    'PRISMA': {'title': 'PRISMA',               'note': 'VNIR+SWIR ~234b'},
    'DESIS':  {'title': 'DESIS',                'note': 'VNIR only ~235b'},
    'EMIT':   {'title': 'EMIT',                  'note': 'VNIR+SWIR ~244b continuum'},
    'WYVERN': {'title': 'Wyvern Dragonette-004', 'note': 'VNIR only 31b'},
}

# Fixed AULC reference range — must match s1_learning_curve AULC_X_MIN/MAX
AULC_X_MIN = 5
AULC_X_MAX = 100


# =============================================================================
# DATA LOADING + FILTERING
# =============================================================================

def _load(csv_path: str, sensor_key: str,
          min_classes: int) -> pd.DataFrame | None:
    p = Path(csv_path)
    if not p.exists():
        raise FileNotFoundError(f'CSV not found: {csv_path}')
    df = pd.read_csv(csv_path)

    required = ['n_samples_per_class', 'iteration', 'scene_name', 'n_classes']
    missing  = [c for c in required if c not in df.columns]
    if missing:
        raise ValueError(f'{sensor_key}: missing columns {missing}')

    df['_sensor'] = sensor_key

    v5_cols = ['ref_window_extended', 'hyp_f1', 'ref_all_f1', 'ref_vnir_f1']
    present = [c for c in v5_cols if c in df.columns]
    if present:
        print(f'[INFO] {sensor_key}: optional columns found: {present}')

    n_before = df['scene_name'].nunique()
    df = df[df['n_classes'] >= min_classes].copy()
    n_after  = df['scene_name'].nunique()
    dropped  = n_before - n_after
    flag = f'  ({dropped} degenerate scenes filtered)' if dropped else ''
    print(f'[OK]  {sensor_key}: {n_after} scenes after min_classes={min_classes}{flag}')

    if df.empty:
        print(f'[WARN] {sensor_key}: no data after filter — skipping')
        return None
    return df


# =============================================================================
# HELPERS
# =============================================================================

def _agg(df: pd.DataFrame, col: str) -> pd.DataFrame:
    return (df.groupby('n_samples_per_class')
              .agg(mean=(col, 'mean'),
                   sd=(col, 'std'),
                   n_scenes=('scene_name', 'nunique'))
              .reset_index()
              .sort_values('n_samples_per_class'))


def _ref_labels(df: pd.DataFrame):
    ref    = str(df['reference'].iloc[0]).lower() if 'reference' in df.columns else 's2'
    prefix = 'S2' if ref == 's2' else 'Landsat'
    n_all  = int(df['n_ref_all_bands'].median())  if 'n_ref_all_bands'  in df.columns else '?'
    n_vnir = int(df['n_ref_vnir_bands'].median()) if 'n_ref_vnir_bands' in df.columns else '?'
    return f'{prefix} all ({n_all}b)', f'{prefix} VNIR ({n_vnir}b)'


def _compute_aulc(df: pd.DataFrame, metric_col: str,
                  x_min: int = AULC_X_MIN,
                  x_max: int = AULC_X_MAX) -> float:
    """
    Mean AULC across all scene-iterations, x normalised to [0,1].
    Clips to fixed reference range; iterations not reaching x_max are excluded.
    """
    vals = []
    for (_, _it), sub in df.groupby(['scene_name', 'iteration']):
        sub = sub[(sub['n_samples_per_class'] >= x_min) &
                  (sub['n_samples_per_class'] <= x_max)
                  ].sort_values('n_samples_per_class')
        if len(sub) < 2 or sub['n_samples_per_class'].max() < x_max:
            continue
        x  = sub['n_samples_per_class'].values
        y  = sub[metric_col].values
        xn = (x - x_min) / (x_max - x_min)
        vals.append(float(np.trapz(y, xn)))
    return float(np.mean(vals)) if vals else np.nan


def _common_xlim(sensor_dfs: dict, metric: str) -> tuple[int, int]:
    all_n = set()
    col   = f'hyp_{metric}'
    for df in sensor_dfs.values():
        if df is not None and col in df.columns:
            all_n.update(df['n_samples_per_class'].unique())
    if not all_n:
        return 0, 210
    return int(min(all_n)) - 1, int(max(all_n)) + 2


def _has_extended_window(df: pd.DataFrame) -> bool:
    """True if any scene in this sensor's data used the extended window."""
    if 'ref_window_extended' not in df.columns:
        return False
    return bool(df['ref_window_extended'].any())


# =============================================================================
# SINGLE SUBPLOT
# =============================================================================

def _draw_subplot(ax, df: pd.DataFrame, sensor_key: str,
                  metric: str, show_vnir: bool,
                  xlim: tuple[int, int]):
    hyp_col  = f'hyp_{metric}'
    all_col  = f'ref_all_{metric}'
    vnir_col = f'ref_vnir_{metric}'

    if hyp_col not in df.columns:
        ax.text(0.5, 0.5,
                f'No {metric} data\n(requires v5 CSV for F1)',
                transform=ax.transAxes,
                ha='center', va='center', fontsize=11, color='grey')
        ax.set_title(SENSOR_META[sensor_key]['title'],
                     fontsize=12, fontweight='bold')
        return

    meta          = SENSOR_META[sensor_key]
    all_lbl, vnir_lbl = _ref_labels(df)
    scale         = 100 if metric == 'oa' else 1
    y_label_unit  = '%' if metric == 'oa' else 'κ' if metric == 'kappa' else 'F1'

    agg_h = _agg(df, hyp_col)
    agg_a = _agg(df, all_col)
    agg_v = _agg(df, vnir_col) if vnir_col in df.columns else None

    x = agg_h['n_samples_per_class']

    ax.errorbar(x, agg_h['mean']*scale, yerr=agg_h['sd']*scale,
                marker='o', label=meta['title'],
                color=C_HYP, capsize=4, linewidth=2.4,
                markersize=7, capthick=1.2, zorder=4)

    ax.errorbar(agg_a['n_samples_per_class'],
                agg_a['mean']*scale, yerr=agg_a['sd']*scale,
                marker='s', label=all_lbl,
                color=C_REF_ALL, capsize=4, linewidth=2.0,
                markersize=6, capthick=1.2, zorder=3)

    if show_vnir and agg_v is not None:
        ax.errorbar(agg_v['n_samples_per_class'],
                    agg_v['mean']*scale, yerr=agg_v['sd']*scale,
                    marker='^', label=vnir_lbl,
                    color=C_REF_VNIR, capsize=4, linewidth=1.8,
                    markersize=6, capthick=1.0, linestyle='--', zorder=3)


    ax.set_xlim(*xlim)
    all_x_ticks = sorted(df['n_samples_per_class'].unique())
    ax.set_xticks(all_x_ticks)
    ax.tick_params(axis='x', labelsize=9)
    ax.tick_params(axis='y', labelsize=9)
    ax.grid(True, alpha=0.25, linestyle='--')

    if metric == 'oa':
        ax.set_ylim(20, 75)
        ax.yaxis.set_major_formatter(
            plt.FuncFormatter(lambda y, _: f'{int(y)}%'))
    else:
        ax.set_ylim(0, 0.65)
        if metric == 'kappa':
            ax.axhline(0, color='grey', linestyle=':', lw=0.8)

    # Info box
    n_sc    = df['scene_name'].nunique()
    aulc    = _compute_aulc(df, hyp_col)
    nc_rng  = f"{int(df['n_classes'].min())}–{int(df['n_classes'].max())}"
    ext_tag = '\n* extended window used' if _has_extended_window(df) else ''

    aulc_lbl = 'AULC(κ)' if metric == 'kappa' else f'AULC({metric})'
    aulc_str = f'{aulc:.3f}' if not np.isnan(aulc) else 'n/a'

    info = (f'n={n_sc} scenes\n'
            f'{meta["note"]}\n'
            f'classes: {nc_rng}\n'
            f'{aulc_lbl}: {aulc_str}'
            f'{ext_tag}')
    ax.text(0.03, 0.97, info,
            transform=ax.transAxes, fontsize=7.5, color='#444444',
            va='top', ha='left',
            bbox=dict(boxstyle='round,pad=0.3', fc='white',
                      ec='#cccccc', alpha=0.85))

    ax.set_title(meta['title'], fontsize=12, fontweight='bold', pad=6)


# =============================================================================
# MAIN FIGURE
# =============================================================================

def build_figure(sensor_dfs: dict, metric: str, show_vnir: bool,
                 out_path: Path, min_classes: int):
    if metric == 'f1':
        for key, df in sensor_dfs.items():
            if 'hyp_f1' not in df.columns:
                print(f'[WARN] {key}: hyp_f1 missing — F1 metric unavailable.')

    fig = plt.figure(figsize=(13, 10))
    fig.patch.set_facecolor('white')

    gs = gridspec.GridSpec(2, 2, figure=fig,
                           hspace=0.42, wspace=0.28,
                           left=0.07, right=0.97,
                           top=0.88, bottom=0.14)
    axes = [fig.add_subplot(gs[r, c])
            for r, c in [(0,0),(0,1),(1,0),(1,1)]]

    present = [s for s in SENSOR_ORDER if s in sensor_dfs]
    xlim    = _common_xlim(sensor_dfs, metric)

    for ax, sensor_key in zip(axes, present):
        _draw_subplot(ax, sensor_dfs[sensor_key], sensor_key,
                      metric, show_vnir, xlim)
    for ax in axes[len(present):]:
        ax.set_visible(False)

    # Shared axis labels
    fig.text(0.5, 0.075, 'Training samples per class',
             ha='center', va='bottom', fontsize=13)
    y_lbl = {'oa': 'Overall accuracy (%)',
              'kappa': "Cohen's κ",
              'f1': 'Macro F1'}.get(metric, metric)
    fig.text(0.01, 0.5, y_lbl,
             ha='left', va='center', fontsize=13, rotation='vertical')

    # Legend
    first_df   = sensor_dfs[present[0]]
    all_lbl, vnir_lbl = _ref_labels(first_df)

    legend_handles = [
        Line2D([0],[0], color=C_HYP,    marker='o', linewidth=2.2,
               markersize=7,  label='Hyperspectral sensor'),
        Line2D([0],[0], color=C_REF_ALL, marker='s', linewidth=2.0,
               markersize=6,  label=all_lbl),
    ]
    if show_vnir:
        legend_handles.append(
            Line2D([0],[0], color=C_REF_VNIR, marker='^', linewidth=1.8,
                   markersize=6, linestyle='--', label=vnir_lbl))

    fig.legend(handles=legend_handles,
               loc='lower center', ncol=len(legend_handles),
               fontsize=10, framealpha=0.9,
               bbox_to_anchor=(0.5, 0.01),
               bbox_transform=fig.transFigure)

    # Title
    metric_lbl = {'oa': 'Overall Accuracy',
                  'kappa': "Cohen's κ",
                  'f1': 'Macro F1'}.get(metric, metric)
    ref_label  = str(first_df['reference'].iloc[0]).upper() \
                 if 'reference' in first_df.columns else 'S2'
    fig.suptitle(
        f'Hyperspectral vs {ref_label}: {metric_lbl} Learning Curves\n'
        f'NVIS MVG vegetation classification for Kangaroo Island, Australia',
        fontsize=14, fontweight='semibold', y=0.96)

    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=300, bbox_inches='tight', facecolor='white')
    plt.close()
    print(f'[SAVED] {out_path}')


# =============================================================================
# ARGS + MAIN
# =============================================================================

def main():
    ap = argparse.ArgumentParser(
        description='Multi-sensor learning curve comparison')
    ap.add_argument('--prisma',  default=None)
    ap.add_argument('--desis',   default=None)
    ap.add_argument('--emit',    default=None)
    ap.add_argument('--wyvern',  default=None)
    ap.add_argument('--out',         default='.')
    ap.add_argument('--metric',      default='kappa',
                    choices=['oa', 'kappa', 'f1'],
                    help='Primary metric (default: kappa)')
    ap.add_argument('--min_classes', type=int, default=3,
                    help='Exclude scenes with n_classes < N (default 3)')
    ap.add_argument('--ref_all_only', action='store_true',
                    help='Omit VNIR-only baseline')
    args = ap.parse_args()

    csv_map = {'PRISMA': args.prisma, 'DESIS': args.desis,
               'EMIT': args.emit,    'WYVERN': args.wyvern}

    sensor_dfs = {}
    for key, path in csv_map.items():
        if path is None:
            print(f'[SKIP] {key} — no CSV provided')
            continue
        try:
            df = _load(path, key, args.min_classes)
            if df is not None:
                sensor_dfs[key] = df
        except Exception as e:
            print(f'[WARN] {key} failed: {e}')

    if not sensor_dfs:
        raise RuntimeError(
            'No valid sensor csv loaded after filtering. '
            'Try lowering --min_classes or supply more csv.')

    out_dir = Path(args.out)

    for metric in ['kappa', 'oa']:
        fname = f'multisensor_lc_{metric}.png'
        build_figure(sensor_dfs, metric,
                     show_vnir=not args.ref_all_only,
                     out_path=out_dir / fname,
                     min_classes=args.min_classes)


if __name__ == '__main__':
    main()
