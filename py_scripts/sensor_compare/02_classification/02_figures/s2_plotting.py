#!/usr/bin/env python3
"""
Per-sensor learning-curve plotter.

Reads the results csv produced by s1_learning_curve.py and generates eight
figures (facet by scene, kappa gap, violin, AULC bar, mean kappa curve, 
mean Overall Accuracy curve, chance-corrected OA, optional macro-F1 curve).

Inputs
------
  --csv         : results CSV from s1_learning_curve
  --out         : output directory for figures
  --min_classes : exclude scenes with fewer than N MVG classes (default 3)
  --no_vnir     : omit VNIR-only reference baseline line
  --show_f1     : produce Fig 8 (macro F1 curve)
  --min_year    : sensitivity analysis, exclude scenes before this year

Usage
-----
  python s2_plotting.py \\
      --csv  ./lc_prisma_s2_results.csv \\
      --out  ./figures_prisma_s2 \\
      --min_classes 3 \\
      --show_f1
"""

import argparse
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import seaborn as sns
from pathlib import Path
from matplotlib.patches import Rectangle, Patch

sns.set_style("whitegrid")
plt.rcParams.update({'font.size': 10, 'axes.titlesize': 11})

# colour palette ──────────────────────────────────────────────────────────
C_HYP  = '#5B8C5A'   # muted green   - hyperspectral
C_ALL  = '#2F4F6F'   # dark slate    - reference all-bands
C_VNIR = '#CD853F'   # peru          - reference VNIR-only

# Fixed AULC reference range - must match s1_learning_curve AULC_X_MIN/MAX
AULC_X_MIN = 5
AULC_X_MAX = 100


# =============================================================================
#  DATA LOADING + FILTERING
# =============================================================================

def load_and_filter(csv_path: str, min_classes: int,
                    min_year: int | None = None) -> pd.DataFrame:
    df = pd.read_csv(csv_path)

    required = ['hyp_oa', 'ref_all_oa', 'ref_vnir_oa', 'hyp_kappa',
                'ref_all_kappa', 'ref_vnir_kappa',
                'scene_name', 'iteration', 'n_samples_per_class',
                'sensor', 'reference', 'n_classes']
    missing = [c for c in required if c not in df.columns]
    if missing:
        raise ValueError(
            f'CSV missing columns: {missing}\n'
            f'Requires s1_learning_curve output format.')

    # Report optional columns
    optional_v5 = ['hyp_f1', 'ref_all_f1', 'ref_vnir_f1',
                   'ref_window_extended']
    present_opt = [c for c in optional_v5 if c in df.columns]
    absent_opt  = [c for c in optional_v5 if c not in df.columns]
    if present_opt:
        print(f'  [INFO] Optional columns present  : {present_opt}')
    if absent_opt:
        print(f'  [INFO] Optional columns absent   : {absent_opt} (older CSV)')

    n_before = len(df)
    sc_before = df['scene_name'].nunique()

    df = df[df['n_classes'] >= min_classes].copy()

    sc_after = df['scene_name'].nunique()
    dropped_sc = sc_before - sc_after
    print(f'  [FILTER] min_classes={min_classes}: '
          f'{sc_before} → {sc_after} scenes  '
          f'({dropped_sc} dropped, {n_before - len(df)} rows removed)')
    if dropped_sc > 0:
        print(f'  [FILTER] Degenerate scenes (<{min_classes} MVG classes) removed.')

    if df.empty:
        raise ValueError(
            f'No data remaining after min_classes={min_classes} filter.'
            f'Try a lower threshold.')

    # ── Year filter (sensitivity analysis) ───────────────────────────────────
    if min_year is not None:
        if 'scene_date' not in df.columns:
            raise ValueError(
                '--min_year requires a scene_date column in the CSV.')
        sc_before_yr = df['scene_name'].nunique()
        df['_scene_year'] = pd.to_datetime(df['scene_date']).dt.year
        df = df[df['_scene_year'] >= min_year].drop(columns='_scene_year')
        sc_after_yr = df['scene_name'].nunique()
        dropped_yr  = sc_before_yr - sc_after_yr
        print(f'  [FILTER] min_year={min_year}: '
              f'{sc_before_yr} → {sc_after_yr} scenes '
              f'({dropped_yr} dropped - SENSITIVITY ANALYSIS MODE)')
        if dropped_yr > 0:
            print(f'  [WARN]  {dropped_yr} scene(s) excluded. '
                  f'This is a sensitivity analysis; do NOT use as primary result.')
        if df.empty:
            raise ValueError(
                f'No scenes remain after min_year={min_year} filter.')

    return df


# =============================================================================
# HELPERS
# =============================================================================

def _infer_labels(df: pd.DataFrame):
    sensor_label = str(df['sensor'].iloc[0]).upper()
    reference    = str(df['reference'].iloc[0]).lower()

    n_hyp  = int(df['n_hyp_bands'].median())     if 'n_hyp_bands'      in df.columns else '?'
    n_all  = int(df['n_ref_all_bands'].median())  if 'n_ref_all_bands'  in df.columns else '?'
    n_vnir = int(df['n_ref_vnir_bands'].median()) if 'n_ref_vnir_bands' in df.columns else '?'

    ref_prefix     = 'S2' if reference == 's2' else 'Landsat'
    hyp_label      = f'{sensor_label} ({n_hyp}b)'
    ref_all_label  = f'{ref_prefix} all ({n_all}b)'
    ref_vnir_label = f'{ref_prefix} VNIR ({n_vnir}b)'

    palette = {
        hyp_label:      C_HYP,
        ref_all_label:  C_ALL,
        ref_vnir_label: C_VNIR,
    }
    return hyp_label, ref_all_label, ref_vnir_label, palette


def _agg(df: pd.DataFrame, col: str) -> pd.DataFrame:
    return (df.groupby('n_samples_per_class')
              .agg(mean=(col, 'mean'),
                   std=(col, 'std'),
                   n_scenes=('scene_name', 'nunique'))
              .reset_index()
              .sort_values('n_samples_per_class'))


def _x_ticks(df: pd.DataFrame):
    return sorted(df['n_samples_per_class'].unique())


def _annotate_n_scenes(ax, agg: pd.DataFrame, metric='oa'):
    y_bot = ax.get_ylim()[0]
    dy    = (ax.get_ylim()[1] - y_bot) * 0.04
    for _, row in agg.iterrows():
        ax.text(row['n_samples_per_class'], y_bot + dy,
                f'n={int(row["n_scenes"])}',
                ha='center', va='bottom', fontsize=7, color='#888888')


def _chance_corrected_oa(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    chance = 1.0 / df['n_classes']
    for prefix in ['hyp', 'ref_all', 'ref_vnir']:
        raw = df[f'{prefix}_oa']
        df[f'{prefix}_norm_oa'] = (raw - chance) / (1.0 - chance)
    return df


def _aulc_fixed(sub: pd.DataFrame, col: str,
                x_min: int = AULC_X_MIN,
                x_max: int = AULC_X_MAX) -> float:
    """AULC clipped to [x_min, x_max]; NaN if iteration does not reach x_max."""
    sub = sub[(sub['n_samples_per_class'] >= x_min) &
              (sub['n_samples_per_class'] <= x_max)
              ].sort_values('n_samples_per_class')
    if sub['n_samples_per_class'].max() < x_max:
        return np.nan
    x = sub['n_samples_per_class'].values
    y = sub[col].values
    xn = (x - x_min) / (x_max - x_min)
    return float(np.trapz(y, xn))


def _is_extended(df: pd.DataFrame, scene: str) -> bool:
    """True if this scene was accepted via the extended temporal window."""
    if 'ref_window_extended' not in df.columns:
        return False
    sub = df[df['scene_name'] == scene]
    return bool(sub['ref_window_extended'].any())


# =============================================================================
# FIG 1 - Facet by scene (kappa)
# =============================================================================

def plot_facet_by_scene(df, hyp_label, ref_all_label, ref_vnir_label,
                        palette, out_dir, show_vnir=True, sens_tag=''):
    scenes = sorted(df['scene_name'].unique())
    n_sc   = len(scenes)
    ncols  = min(3, n_sc)
    nrows  = max(1, int(np.ceil(n_sc / ncols)))

    fig, axes = plt.subplots(nrows, ncols,
                             figsize=(ncols * 3.8, nrows * 3.2),
                             sharex=False, sharey=True)
    axes = np.array(axes).flatten()

    col_map = {
        hyp_label:      'hyp_kappa',
        ref_all_label:  'ref_all_kappa',
        ref_vnir_label: 'ref_vnir_kappa',
    }
    if not show_vnir:
        col_map.pop(ref_vnir_label)

    scene_date_map = {}
    if 'scene_date' in df.columns:
        scene_date_map = (df.drop_duplicates('scene_name')
                           .set_index('scene_name')['scene_date']
                           .to_dict())

    for ax, scene in zip(axes, scenes):
        sub = df[df['scene_name'] == scene]
        nc  = int(sub['n_classes'].iloc[0])
        ext = _is_extended(df, scene)
        for lbl, col in col_map.items():
            agg = _agg(sub, col)
            ax.errorbar(agg['n_samples_per_class'], agg['mean'],
                        yerr=agg['std'], marker='o', label=lbl,
                        color=palette[lbl], capsize=3, linewidth=1.8)
        ext_tag = ' *' if ext else ''
        scene_label = scene_date_map.get(scene, scene[-22:])
        ax.set_title(f'{scene_label}{ext_tag}\n({nc} classes)', fontsize=7.5)
        ax.set_ylim(-0.05, 1.05)
        ax.axhline(0, color='grey', linestyle=':', linewidth=0.8)
        ax.grid(True, alpha=0.3)
        ax.set_xticks(_x_ticks(sub))
        ax.tick_params(axis='x', labelsize=8)

    for ax in axes[n_sc:]:
        ax.set_visible(False)

    handles, labels = axes[0].get_legend_handles_labels()
    fig.legend(handles, labels, loc='lower center', ncol=3,
               fontsize=9, bbox_to_anchor=(0.5, -0.02))
    fig.supxlabel('Training samples per class', fontsize=12)
    fig.supylabel("Cohen's κ", fontsize=12)

    if any(_is_extended(df, s) for s in scenes):
        fig.text(0.5, -0.04,
                 '* Scene accepted via extended temporal search window',
                 ha='center', fontsize=8, color='#666666', style='italic')

    fig.suptitle(f"Cohen's κ Learning Curve by Scene{sens_tag}", fontsize=13,
                 fontweight='bold', y=1.02)
    plt.tight_layout()
    fig.savefig(out_dir / 'fig1_facet_by_scene.png', dpi=300,
                bbox_inches='tight')
    plt.close()
    print('  Saved fig1_facet_by_scene.png')


# =============================================================================
# FIG 2 - Kappa gap (hyperspec advantage) plot
# =============================================================================

def plot_gap(df, hyp_label, ref_all_label, ref_vnir_label,
             palette, out_dir, show_vnir=True):
    df = df.copy()
    df['gap_all']  = df['hyp_kappa'] - df['ref_all_kappa']
    df['gap_vnir'] = df['hyp_kappa'] - df['ref_vnir_kappa']

    fig, ax = plt.subplots(figsize=(7, 5))
    pairs = [('gap_all', f'{hyp_label} - {ref_all_label}', C_ALL)]
    if show_vnir:
        pairs.append(('gap_vnir', f'{hyp_label} - {ref_vnir_label}', C_VNIR))

    for gap_col, lbl, col in pairs:
        agg = _agg(df, gap_col)
        ax.errorbar(agg['n_samples_per_class'], agg['mean'],
                    yerr=agg['std'], marker='s', label=lbl,
                    color=col, capsize=5, linewidth=2.2,
                    markersize=8, capthick=1.5)
        _annotate_n_scenes(ax, agg)

    ax.axhline(0, color='black', linestyle='--', linewidth=1.2, alpha=0.5)
    ax.set_xlabel('Training samples per class', fontsize=12)
    ax.set_ylabel("κ advantage (hyperspectral - reference)", fontsize=12)
    ax.set_title("Hyperspectral κ Advantage Over Reference\n"
                 "Positive = hyperspectral wins; Negative = reference wins",
                 fontsize=12, fontweight='bold')
    ax.legend(fontsize=9)
    ax.grid(True, alpha=0.3)
    ax.set_xticks(_x_ticks(df))

    summary_lines = []
    for n, grp in df.groupby('n_samples_per_class'):
        nw = (grp['gap_all'] > 0).sum()
        summary_lines.append(f'n={n}: hyp>{ref_all_label.split()[0]} '
                              f'{nw}/{len(grp)} iter')
    ax.text(0.02, 0.97, '\n'.join(summary_lines),
            transform=ax.transAxes, fontsize=7.5, color='grey',
            va='top', ha='left',
            bbox=dict(boxstyle='round,pad=0.3', fc='white',
                      ec='#cccccc', alpha=0.85))

    plt.tight_layout()
    fig.savefig(out_dir / 'fig2_gap_kappa.png', dpi=300, bbox_inches='tight')
    plt.close()
    print('  Saved fig2_gap_kappa.png')


# =============================================================================
# FIG 3 - Violin & boxplot
# =============================================================================

def plot_violin(df, hyp_label, ref_all_label, ref_vnir_label,
                palette, out_dir, show_vnir=True):
    metric = 'kappa'
    col_map = {'__HYP__': hyp_label, '__REF_ALL__': ref_all_label,
               '__REF_VNIR__': ref_vnir_label}
    hue_order = [hyp_label, ref_all_label]
    if show_vnir:
        hue_order.append(ref_vnir_label)

    id_vars = [c for c in ['n_samples_per_class', 'iteration',
                            'scene_name', 'ref_date', 'ref_days_diff']
               if c in df.columns]
    cols = {f'hyp_{metric}': '__HYP__',
            f'ref_all_{metric}': '__REF_ALL__',
            f'ref_vnir_{metric}': '__REF_VNIR__'}
    if not show_vnir:
        cols.pop(f'ref_vnir_{metric}')

    df_long = pd.melt(df, id_vars=id_vars,
                      value_vars=list(cols.keys()),
                      var_name='feature_set', value_name=metric)
    df_long['sensor'] = df_long['feature_set'].map(cols).map(col_map)
    df_long = df_long[df_long['sensor'].isin(hue_order)]

    x_order  = sorted(df_long['n_samples_per_class'].unique())
    n_hues   = len(hue_order)
    width    = 0.8

    fig, ax = plt.subplots(figsize=(11, 6))
    sns.violinplot(data=df_long, x='n_samples_per_class', y=metric,
                   hue='sensor', hue_order=hue_order, order=x_order,
                   palette=palette, inner=None, cut=0, linewidth=1.5,
                   ax=ax, dodge=True, width=width, saturation=0.9,
                   alpha=0.85, zorder=2)
    sns.stripplot(data=df_long, x='n_samples_per_class', y=metric,
                  hue='sensor', hue_order=hue_order, order=x_order,
                  palette={h: '#1a1a1a' for h in hue_order},
                  dodge=True, jitter=0.18, size=3.0, alpha=0.30,
                  edgecolor='none', ax=ax, legend=False, zorder=3)

    bw = 0.22
    for i, x_val in enumerate(x_order):
        for j, hue_val in enumerate(hue_order):
            sub = df_long[(df_long['n_samples_per_class'] == x_val) &
                          (df_long['sensor'] == hue_val)][metric].dropna()
            if len(sub) < 2:
                continue
            q25, q50, q75 = np.percentile(sub, [25, 50, 75])
            iqr   = q75 - q25
            lo    = max(q25 - 1.5*iqr, sub.min())
            hi    = min(q75 + 1.5*iqr, sub.max())
            off   = (j - (n_hues-1)/2) * (width/n_hues)
            xp    = i + off
            bw_px = (width/n_hues) * bw
            ax.add_patch(Rectangle((xp-bw_px/2, q25), bw_px, iqr,
                                   fc='none', ec='black', lw=1.2,
                                   zorder=5, clip_on=False))
            ax.plot([xp,xp], [lo,q25], color='black', lw=1.0,
                    solid_capstyle='butt', zorder=5)
            ax.plot([xp,xp], [q75,hi], color='black', lw=1.0,
                    solid_capstyle='butt', zorder=5)
            ax.scatter(xp, q50, color='white', ec='black',
                       s=40, zorder=6, lw=0.9)

    ax.axhline(0, color='grey', linestyle=':', lw=0.9)
    legend_elements = [Patch(fc=palette[h], ec='black', label=h)
                       for h in hue_order]
    ax.legend(handles=legend_elements, loc='lower right', fontsize=9)
    ax.set_ylim(-0.15, 1.05)
    ax.set_xlabel('Training samples per class', fontsize=12)
    ax.set_ylabel("Cohen's κ", fontsize=12)
    ax.set_title("Distribution of κ Across All Scene-Iterations",
                 fontsize=12, fontweight='semibold')
    ax.grid(True, alpha=0.3, axis='y')
    plt.tight_layout()
    fig.savefig(out_dir / 'fig3_violin_kappa.png', dpi=300,
                bbox_inches='tight')
    plt.close()
    print('  Saved fig3_violin_kappa.png')


# =============================================================================
# FIG 4 - AULC bar chart by scene
# =============================================================================

def plot_aulc(df, hyp_label, ref_all_label, ref_vnir_label,
              palette, out_dir, show_vnir=True):
    records = []
    for (scene, it), sub in df.groupby(['scene_name', 'iteration']):
        sub = sub.sort_values('n_samples_per_class')
        for col_key, lbl in [('hyp_kappa',      hyp_label),
                              ('ref_all_kappa',  ref_all_label),
                              ('ref_vnir_kappa', ref_vnir_label)]:
            if not show_vnir and lbl == ref_vnir_label:
                continue
            area = _aulc_fixed(sub, col_key, AULC_X_MIN, AULC_X_MAX)
            records.append({'scene': scene, 'iteration': it,
                            'feature_set': lbl, 'aulc': area})

    if not records:
        print('[SKIP] fig4 - insufficient data for AULC')
        return
    aulc_df = pd.DataFrame(records)
    n_nan = aulc_df['aulc'].isna().sum()
    if n_nan:
        print(f'[AULC] {n_nan} iterations excluded (did not reach n={AULC_X_MAX})')
    aulc_df = aulc_df.dropna(subset=['aulc'])

    if aulc_df.empty:
        print('  [SKIP] fig4 - no iterations reached full AULC range [5,100]')
        return

    scenes  = sorted(aulc_df['scene'].unique())
    x       = np.arange(len(scenes))
    labels  = [hyp_label, ref_all_label] + ([ref_vnir_label] if show_vnir else [])
    bar_w   = 0.8 / len(labels)

    fig, ax = plt.subplots(figsize=(max(7, len(scenes)*1.8), 5))
    for i, lbl in enumerate(labels):
        sub   = aulc_df[aulc_df['feature_set'] == lbl]
        means = sub.groupby('scene')['aulc'].mean().reindex(scenes)
        stds  = sub.groupby('scene')['aulc'].std().reindex(scenes)
        ax.bar(x + i*bar_w, means, bar_w, yerr=stds,
               label=lbl, color=palette[lbl],
               capsize=3, edgecolor='black', linewidth=0.5)

    ax.set_xticks(x + bar_w*(len(labels)-1)/2)
    ax.set_xticklabels([s[-22:] for s in scenes],
                       rotation=45, ha='right', fontsize=8)
    ax.set_ylabel(f"AULC (κ, x normalised to [{AULC_X_MIN},{AULC_X_MAX}])",
                  fontsize=12)
    ax.set_title("AULC per Scene (higher = faster & better learning)",
                 fontsize=12, fontweight='semibold')
    ax.legend(fontsize=9)
    ax.grid(True, alpha=0.3, axis='y')
    plt.tight_layout()
    fig.savefig(out_dir / 'fig4_aulc_by_scene.png', dpi=300,
                bbox_inches='tight')
    plt.close()

# =============================================================================
# FIG 5 - Mean learning curve (kappa - primary)
# =============================================================================

def plot_mean_curve_kappa(df, hyp_label, ref_all_label, ref_vnir_label,
                          palette, out_dir, show_vnir=True, sens_tag=''):
    _plot_mean_curve(df, hyp_label, ref_all_label, ref_vnir_label,
                     palette, 'kappa', "Cohen's κ",
                     'fig5_mean_curve_kappa.png', out_dir, show_vnir,
                     sens_tag=sens_tag)

# =============================================================================
# FIG 6 - Mean learning curve (OA - secondary)
# =============================================================================

def plot_mean_curve_oa(df, hyp_label, ref_all_label, ref_vnir_label,
                       palette, out_dir, show_vnir=True, sens_tag=''):
    _plot_mean_curve(df, hyp_label, ref_all_label, ref_vnir_label,
                     palette, 'oa', 'Overall accuracy (%)',
                     'fig6_mean_curve_oa.png', out_dir, show_vnir,
                     sens_tag=sens_tag)


def _plot_mean_curve(df, hyp_label, ref_all_label, ref_vnir_label,
                     palette, metric, y_label, fname, out_dir,
                     show_vnir=True, sens_tag=''):
    hyp_col  = f'hyp_{metric}'
    all_col  = f'ref_all_{metric}'
    vnir_col = f'ref_vnir_{metric}'
    scale    = 100 if metric == 'oa' else 1

    agg_h = _agg(df, hyp_col)
    agg_a = _agg(df, all_col)
    agg_v = _agg(df, vnir_col)

    fig, ax = plt.subplots(figsize=(8, 5.5))
    x = agg_h['n_samples_per_class']

    ax.errorbar(x, agg_h['mean']*scale, yerr=agg_h['std']*scale,
                marker='o', label=hyp_label, color=palette[hyp_label],
                capsize=5, linewidth=2.2, markersize=8, capthick=1.0, zorder=3)
    ax.errorbar(agg_a['n_samples_per_class'], agg_a['mean']*scale,
                yerr=agg_a['std']*scale,
                marker='s', label=ref_all_label, color=palette[ref_all_label],
                capsize=5, linewidth=2.2, markersize=8, capthick=1.0, zorder=3)
    if show_vnir:
        ax.errorbar(agg_v['n_samples_per_class'], agg_v['mean']*scale,
                    yerr=agg_v['std']*scale,
                    marker='^', label=ref_vnir_label,
                    color=palette[ref_vnir_label],
                    capsize=5, linewidth=2.2, markersize=8, capthick=1.0,
                    linestyle='--', zorder=3)

    _annotate_n_scenes(ax, agg_h, metric)

    if metric == 'kappa':
        ax.axhline(0, color='grey', linestyle=':', lw=0.9)

    ax.set_xlabel('Training samples per class', fontsize=12)
    ax.set_ylabel(y_label, fontsize=12)
    sensor_short = hyp_label.split('(')[0].strip()
    ax.set_title(f'{sensor_short} Hyperspectral vs Reference\n'
                 f'NVIS MVG classification, mean +- SD across scenes{sens_tag}',
                 fontsize=12, fontweight='semibold')
    ax.legend(fontsize=9, loc='lower right', framealpha=0.9)
    ax.grid(True, alpha=0.3)
    ax.set_xticks(_x_ticks(df))

    table_lines = []
    for n, grp in df.groupby('n_samples_per_class'):
        nw       = (grp[hyp_col] > grp[all_col]).sum()
        n_scenes = grp['scene_name'].nunique()
        n_iters  = grp['iteration'].nunique() if 'iteration' in grp.columns else '?'
        table_lines.append(
            f'n={n}: {nw}/{len(grp)} scene×iter hyp>ref'
            f'  ({n_scenes} scenes × {n_iters} iter)'
        )
    ax.text(0.02, 0.97, '\n'.join(table_lines),
            transform=ax.transAxes, fontsize=7.5, color='grey',
            va='top', ha='left',
            bbox=dict(boxstyle='round,pad=0.3', fc='white',
                      ec='#cccccc', alpha=0.85))

    plt.tight_layout()
    fig.savefig(out_dir / fname, dpi=300, bbox_inches='tight')
    plt.close()
    print(f' Saved {fname}')


# =============================================================================
# FIG 7 - Chance-corrected OA
# =============================================================================

def plot_norm_oa(df, hyp_label, ref_all_label, ref_vnir_label,
                 palette, out_dir, show_vnir=True):
    df = _chance_corrected_oa(df)

    agg_h = _agg(df, 'hyp_norm_oa')
    agg_a = _agg(df, 'ref_all_norm_oa')
    agg_v = _agg(df, 'ref_vnir_norm_oa')

    fig, ax = plt.subplots(figsize=(8, 5.5))
    x = agg_h['n_samples_per_class']

    ax.errorbar(x, agg_h['mean'], yerr=agg_h['std'],
                marker='o', label=hyp_label, color=palette[hyp_label],
                capsize=5, linewidth=2.2, markersize=8, capthick=1.0, zorder=3)
    ax.errorbar(agg_a['n_samples_per_class'], agg_a['mean'],
                yerr=agg_a['std'],
                marker='s', label=ref_all_label, color=palette[ref_all_label],
                capsize=5, linewidth=2.2, markersize=8, capthick=1.0, zorder=3)
    if show_vnir:
        ax.errorbar(agg_v['n_samples_per_class'], agg_v['mean'],
                    yerr=agg_v['std'],
                    marker='^', label=ref_vnir_label,
                    color=palette[ref_vnir_label],
                    capsize=5, linewidth=2.2, markersize=8, capthick=1.0,
                    linestyle='--', zorder=3)

    _annotate_n_scenes(ax, agg_h)
    ax.axhline(0, color='grey', linestyle=':', lw=0.9,
               label='Chance level (0)')

    ax.set_xlabel('Training samples per class', fontsize=12)
    ax.set_ylabel('Chance-corrected OA  [(OA - 1/K) / (1 - 1/K)]',
                  fontsize=11)
    sensor_short = hyp_label.split('(')[0].strip()
    ax.set_title(f'{sensor_short}: Chance-Corrected OA\n'
                 f'Controls for varying n_classes across scenes',
                 fontsize=12, fontweight='semibold')
    ax.legend(fontsize=9, loc='lower right', framealpha=0.9)
    ax.grid(True, alpha=0.3)
    ax.set_xticks(_x_ticks(df))

    plt.tight_layout()
    fig.savefig(out_dir / 'fig7_norm_oa.png', dpi=300, bbox_inches='tight')
    plt.close()
    print('  Saved fig7_norm_oa.png')


# =============================================================================
# FIG 8 - Macro F1 mean learning curve
# =============================================================================

def plot_mean_curve_f1(df, hyp_label, ref_all_label, ref_vnir_label,
                       palette, out_dir, show_vnir=True):
    """
    Mean ± SD macro F1 learning curve.
    Requires output columns: hyp_f1, ref_all_f1, ref_vnir_f1.
    Skipped gracefully if absent.
    """
    required = ['hyp_f1', 'ref_all_f1', 'ref_vnir_f1']
    missing  = [c for c in required if c not in df.columns]
    if missing:
        print(f'  [SKIP] fig8_f1 - columns missing ({missing}).')
        return

    _plot_mean_curve(df, hyp_label, ref_all_label, ref_vnir_label,
                     palette, 'f1', 'Macro F1',
                     'fig8_mean_curve_f1.png', out_dir, show_vnir)


# =============================================================================
# MAIN
# =============================================================================

def main(csv_path: str, out_dir: str, min_classes: int,
         show_vnir: bool, show_f1: bool, min_year: int | None = None):
    out = Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)

    df = load_and_filter(csv_path, min_classes, min_year=min_year)

    hyp_label, ref_all_label, ref_vnir_label, palette = _infer_labels(df)

    print(f'[INFO] Sensor      : {hyp_label}')
    print(f'[INFO] Reference   : {ref_all_label}  /  {ref_vnir_label}')
    print(f'[INFO] Scenes      : {df["scene_name"].nunique()}')
    print(f'[INFO] n_classes   : {df["n_classes"].min()}–{df["n_classes"].max()}'
          f' (mean {df["n_classes"].mean():.1f})')
    print(f'[INFO] n_samples   : {sorted(df["n_samples_per_class"].unique())}')
    print(f'[INFO] Rows        : {len(df)}')
    print(f'[INFO] AULC range  : [{AULC_X_MIN}, {AULC_X_MAX}] (fixed)')
    if min_year is not None:
        print(f'[INFO] * SENSITIVITY ANALYSIS: scenes from {min_year} onwards only *')
    print(f'[INFO] Output      : {out}')
    print('[INFO] Generating figures...')

    sens_tag = f'  [sensitivity: ≥{min_year}]' if min_year is not None else ''

    plot_facet_by_scene(df, hyp_label, ref_all_label, ref_vnir_label,
                        palette, out, show_vnir, sens_tag=sens_tag)
    plot_gap(df, hyp_label, ref_all_label, ref_vnir_label,
             palette, out, show_vnir)
    plot_violin(df, hyp_label, ref_all_label, ref_vnir_label,
                palette, out, show_vnir)
    plot_aulc(df, hyp_label, ref_all_label, ref_vnir_label,
              palette, out, show_vnir)
    plot_mean_curve_kappa(df, hyp_label, ref_all_label, ref_vnir_label,
                          palette, out, show_vnir, sens_tag=sens_tag)
    plot_mean_curve_oa(df, hyp_label, ref_all_label, ref_vnir_label,
                       palette, out, show_vnir, sens_tag=sens_tag)
    plot_norm_oa(df, hyp_label, ref_all_label, ref_vnir_label,
                 palette, out, show_vnir)
    if show_f1:
        plot_mean_curve_f1(df, hyp_label, ref_all_label, ref_vnir_label,
                           palette, out, show_vnir)

    print(f'[DONE] Figures saved to {out}')


if __name__ == '__main__':
    ap = argparse.ArgumentParser(
        description='Per-sensor learning-curve plotter')
    ap.add_argument('--csv',         required=True,
                    help='Input CSV from s1_learning_curve')
    ap.add_argument('--out',         default='.',
                    help='Output directory')
    ap.add_argument('--min_classes', type=int, default=3,
                    help='Exclude scenes with n_classes < N (default 3)')
    ap.add_argument('--no_vnir',     action='store_true',
                    help='Omit VNIR-only reference baseline')
    ap.add_argument('--show_f1',     action='store_true',
                    help='Produce Fig 8 (macro F1 curve)')
    ap.add_argument('--min_year',     type=int, default=None,
                    help='Sensitivity analysis: exclude scenes before this year '
                         '(e.g. --min_year 2021). Do NOT use as primary result.')
    args = ap.parse_args()
    main(args.csv, args.out, args.min_classes,
         show_vnir=not args.no_vnir, show_f1=args.show_f1,
         min_year=args.min_year)
