"""
Regime Timeline plots in the style of the Sharpe Two Forward Notes:
daily 30DTE (or 9DTE) implied vol scatter, colored by model regime.

Usage:
  python plot_timeline.py                     # replication panels vs the PDFs
  python plot_timeline.py TICKER [tenor]      # single ticker, last ~12 months
"""
import os
import sys
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

HERE = os.path.dirname(os.path.abspath(__file__))
RESULTS = os.path.join(HERE, "..", "results")
FIGURES = os.path.join(HERE, "..", "figures")

# regime -> color, ordered from quietest to most stressed (legend order).
# Hues chosen to match the Sharpe Two legend semantics (validated palette).
COLORS = {
    "Low RV": "#008300",
    "Positive VRP": "#4a3aa7",
    "Calm": "#2a78d6",
    "Post Stress": "#eda100",
    "Stress": "#eb6834",
    "High Stress": "#9c5f2e",
    "Extreme Stress": "#e34948",
    "Vol Crush": "#e87ba4",
}

INK, MUTED, GRID, AXIS = "#0b0b0b", "#898781", "#e1e0d9", "#c3c2b7"


def load(tenor="30dte"):
    d = pd.read_parquet(os.path.join(RESULTS, f"regimes_{tenor}.pq"))
    d["tradeDate"] = pd.to_datetime(d["tradeDate"])
    return d


def plot_timeline(ax, d, ticker, t0=None, t1=None):
    g = d[d.ticker == ticker]
    if t0:
        g = g[g.tradeDate >= t0]
    if t1:
        g = g[g.tradeDate <= t1]
    for reg, col in COLORS.items():
        gg = g[g.regime == reg]
        if len(gg):
            ax.scatter(gg.tradeDate, gg.iv, s=16, color=col, label=reg,
                       edgecolors="none", zorder=3)
    ax.set_title(ticker, fontsize=11, color=INK, pad=10,
                 bbox=dict(facecolor="#f0efec", edgecolor="none",
                           boxstyle="round,pad=0.35"))
    ax.set_ylabel("IMPLIED VOLATILITY", color=MUTED, fontsize=8)
    ax.set_ylim(bottom=0)
    ax.yaxis.set_major_locator(plt.MaxNLocator(4))
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y-%m-%d"))
    ax.grid(axis="y", ls="--", color=GRID, zorder=0)
    for s in ["top", "right", "left"]:
        ax.spines[s].set_visible(False)
    ax.spines["bottom"].set_color(AXIS)
    ax.tick_params(colors=MUTED, labelsize=8)


def finish(fig, axes, fname):
    present = set()
    for ax in np.ravel(axes):
        present |= {h.get_label() for h in ax.collections}
    handles = [plt.Line2D([], [], marker="o", ls="", color=c, label=r)
               for r, c in COLORS.items() if r in present]
    fig.legend(handles=handles, loc="lower center", ncol=len(COLORS),
               frameon=False, fontsize=8, labelcolor="#52514e",
               columnspacing=1.2, handletextpad=0.2)
    fig.tight_layout(rect=[0, 0.05, 1, 1])
    os.makedirs(FIGURES, exist_ok=True)
    path = os.path.join(FIGURES, fname)
    fig.savefig(path, dpi=120, facecolor=fig.get_facecolor(),
                bbox_inches="tight")
    print("saved", path)


def replication_panels():
    """Same ticker/window pairs as the Forward Note PDFs, for eyeballing."""
    d = load("30dte")
    fig, axes = plt.subplots(2, 1, figsize=(9, 8), facecolor="#fcfcfb")
    plot_timeline(axes[0], d, "QQQ", "2025-06-19", "2026-06-26")   # FN 20260628
    plot_timeline(axes[1], d, "USO", "2025-07-25", "2026-03-13")   # FN 20260315
    finish(fig, axes, "replication_qqq_uso.png")


def single(ticker, tenor="30dte"):
    d = load(tenor)
    t1 = d.tradeDate.max()
    t0 = t1 - pd.Timedelta(days=365)
    fig, ax = plt.subplots(figsize=(9, 4.5), facecolor="#fcfcfb")
    plot_timeline(ax, d, ticker, t0, t1)
    finish(fig, [ax], f"timeline_{ticker}_{tenor}.png")


if __name__ == "__main__":
    if len(sys.argv) > 1:
        single(sys.argv[1].upper(), *sys.argv[2:])
    else:
        replication_panels()
