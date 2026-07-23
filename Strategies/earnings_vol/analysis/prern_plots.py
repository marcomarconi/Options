"""
Plots for the LONG pre-earnings ATM straddle study (companion to
prern_analysis.py). Reads cache/trades_prern.parquet, writes PNGs to
figures/.  Run from analysis/:  python3 prern_plots.py

GROSS ONLY (mid marks) — costs ignored per house rule 2026-07-11.
Gate = PAST-ONLY expanding impErnMv rank (gate.py); the first-pass
full-sample rank had lookahead (audit_prern.py).

Panels
1. cumulative gross pnl (%/spot summed per trade, by exit date):
   ungated lag-15 vs AMC+cheap-1/3 gate
2. mean %/spot by entry lag (with t-stats)
3. lag-15 mean %/spot by year
4. gate monotonicity: mean %/spot by impErnMv past-only rank quintile
5. gamma decomposition: mean %/spot by |spot move| bucket
6. lag-15 return distribution (clipped tail), AMC vs BMO overlay
"""
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from gate import add_mv_rank

R = "ret_spot_mid"
OUT = "../figures"


def tstat(r):
    return r.mean() / r.std() * np.sqrt(len(r))


def bar_with_t(ax, labels, means, ts, title, color="#4878cf"):
    x = np.arange(len(labels))
    ax.bar(x, means, color=color)
    for xi, m, t in zip(x, means, ts):
        ax.text(xi, m, f"t={t:.1f}", ha="center",
                va="bottom" if m >= 0 else "top", fontsize=8)
    ax.set_xticks(x, labels)
    ax.axhline(0, color="k", lw=0.7)
    ax.set_ylabel("mean %/spot (gross mid)")
    ax.set_title(title)


def main():
    os.makedirs(OUT, exist_ok=True)
    tr = pd.read_parquet("../cache/trades_prern.parquet")
    tr["year"] = tr.ernDate.dt.year
    t15 = tr[tr.lag == 15].copy()
    g = add_mv_rank(t15).dropna(subset=["mv_rk"])
    gate = g[(g.when == "AMC") & (g.mv_rk < 0.33)]

    # 1. cumulative pnl curves
    fig, ax = plt.subplots(figsize=(9, 5))
    for sub, lab, c in [(t15, f"lag 15 ungated (n={len(t15)})", "#888888"),
                        (gate, f"AMC + cheap 1/3, past-only rank "
                               f"(n={len(gate)})", "#4878cf")]:
        s = sub.sort_values("exit")
        ax.plot(s.exit, s[R].cumsum() * 100, label=lab, color=c)
    ax.axhline(0, color="k", lw=0.7)
    ax.set_ylabel("cumulative %/spot (sum over trades)")
    ax.set_title("Long pre-earnings straddle: cumulative gross pnl")
    ax.legend()
    fig.tight_layout()
    fig.savefig(f"{OUT}/1_cum_pnl.png", dpi=110)

    # 2. by entry lag
    fig, ax = plt.subplots(figsize=(6.5, 4.2))
    rows = [(f"lag {lag}", s[R].mean() * 100, tstat(s[R]))
            for lag, s in tr.groupby("lag")]
    bar_with_t(ax, *zip(*rows),
               title="mean gross return by entry lag (all trades)")
    fig.tight_layout()
    fig.savefig(f"{OUT}/2_by_lag.png", dpi=110)

    # 3. lag 15 by year
    fig, ax = plt.subplots(figsize=(9, 4.2))
    yr = t15.groupby("year")[R].agg(["mean", "size"])
    ax.bar(yr.index, yr["mean"] * 100,
           color=["#4878cf" if m >= 0 else "#d1615d" for m in yr["mean"]])
    ax.axhline(0, color="k", lw=0.7)
    ax.set_ylabel("mean %/spot (gross mid)")
    ax.set_title("lag 15: mean gross return by year (positive 10/12)")
    fig.tight_layout()
    fig.savefig(f"{OUT}/3_by_year.png", dpi=110)

    # 4. gate monotonicity, quintiles of past-only impErnMv rank
    fig, ax = plt.subplots(figsize=(6.5, 4.2))
    g["q"] = pd.cut(g.mv_rk, np.linspace(0, 1, 6), labels=False,
                    include_lowest=True)
    rows = [(f"Q{int(q)+1}", s[R].mean() * 100, tstat(s[R]))
            for q, s in g.groupby("q")]
    bar_with_t(ax, *zip(*rows),
               title="lag 15: gross return by impErnMv PAST-ONLY rank "
                     "quintile\n(Q1 = implied move CHEAP vs own prior events)")
    fig.tight_layout()
    fig.savefig(f"{OUT}/4_gate_monotone.png", dpi=110)

    # 5. gamma decomposition by |spot move| bucket
    fig, ax = plt.subplots(figsize=(6.5, 4.2))
    t15["absmove"] = t15.spot_ret.abs()
    qs = t15.absmove.quantile([0, 0.25, 0.5, 0.75, 0.9, 1.0]).values
    t15["mb"] = pd.cut(t15.absmove, qs, labels=False, include_lowest=True)
    labs = ["<p25", "p25-50", "p50-75", "p75-90", ">p90"]
    rows = [(labs[int(b)], s[R].mean() * 100, tstat(s[R]))
            for b, s in t15.groupby("mb")]
    bar_with_t(ax, *zip(*rows),
               title="lag 15: gross return by |spot move| over hold\n"
                     "(pnl is pre-event gamma: movers pay, quiet bleeds)")
    fig.tight_layout()
    fig.savefig(f"{OUT}/5_gamma_decomp.png", dpi=110)

    # 6. return distribution, AMC vs BMO
    fig, ax = plt.subplots(figsize=(7.5, 4.2))
    bins = np.linspace(-4, 8, 61)
    for w, c in [("AMC", "#4878cf"), ("BMO", "#d1615d")]:
        r = t15.loc[t15.when == w, R].clip(-0.04, 0.08) * 100
        ax.hist(r, bins=bins, alpha=0.55, label=f"{w} (n={len(r)})",
                color=c, density=True)
    ax.axvline(0, color="k", lw=0.7)
    ax.set_xlabel("%/spot per trade (clipped at -4/+8)")
    ax.set_title("lag 15: return distribution — win ~41%, positive skew")
    ax.legend()
    fig.tight_layout()
    fig.savefig(f"{OUT}/6_distribution.png", dpi=110)

    print(f"wrote 6 plots to {OUT}/")


if __name__ == "__main__":
    main()
