"""
Plots for the Oakquants 4-signal test (companion to oq_signals.py).
Writes figs 10-11 to figures/.  Run from analysis/:  python3 oq_signals_plot.py

Fig 10:  quintile bars per signal (Q1 = implied move CHEAP vs own history),
         lag-15 book on top, lag-2 book below — monotone at 15, dead/U at 2.
Fig 11b: the SIMPLE single-signal gate (AMC + s4 low-half) vs the old gate —
         legacy/robustness view; the video-faithful 4-signal walk-forward
         model (fig 11) lives in oq_model.py and supersedes this as the
         headline sleeve.

GROSS mid only; all signals past-only (expanding, >=4 prior events).
"""
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from gate import add_mv_rank
from oq_signals import build_event_table, add_signals, SIGS

R = "ret_spot_mid"
OUT = "../figures"
BLUE, RED, GRAY, GREEN = "#4878cf", "#d1615d", "#888888", "#1baf7a"

SIG_LAB = {
    "s1_vs_last_imp": "s1: imp / last imp",
    "s2_vs_last_rlz": "s2: imp − last rlz",
    "s3_vs_avg_imp": "s3: imp / avg imp",
    "s4_vs_avg_rlz": "s4: imp − avg rlz",
}


def tstat(r):
    return r.mean() / r.std() * np.sqrt(len(r))


def qrows(d, sig):
    s = d.dropna(subset=[sig, R])
    s = s[np.isfinite(s[sig])]
    q = pd.qcut(s[sig].rank(method="first"), 5, labels=False)
    return [(s.loc[q == k, R].mean() * 100, tstat(s.loc[q == k, R]))
            for k in range(5)]


def main():
    os.makedirs(OUT, exist_ok=True)
    et = build_event_table()
    tr = pd.read_parquet("../cache/trades_prern.parquet")
    ls = pd.read_parquet("../cache/trades_prern_lagscan.parquet")
    t15 = add_signals(add_mv_rank(tr[tr.lag == 15]), et)
    t2 = add_signals(ls[ls.lag == 2].dropna(subset=["impErnMv"]).copy(), et)

    # ---- fig 10: quintile panels, lag 15 vs lag 2 --------------------------
    fig, axes = plt.subplots(2, 4, figsize=(13, 6.2), sharey="row")
    for row, (d, lab) in enumerate([(t15, "lag 15"), (t2, "lag 2")]):
        for col, sig in enumerate(SIGS):
            ax = axes[row, col]
            rows = qrows(d, sig)
            means = [m for m, _ in rows]
            ax.bar(np.arange(5), means,
                   color=[BLUE if m >= 0 else RED for m in means])
            for xi, (m, t) in enumerate(rows):
                ax.text(xi, m, f"{t:.1f}", ha="center",
                        va="bottom" if m >= 0 else "top", fontsize=7,
                        color="#52514e")
            ax.set_xticks(np.arange(5), [f"Q{k+1}" for k in range(5)],
                          fontsize=8)
            ax.axhline(0, color="k", lw=0.7)
            if row == 0:
                ax.set_title(SIG_LAB[sig], fontsize=10)
            if col == 0:
                ax.set_ylabel(f"{lab}\nmean %/spot (gross mid)")
    fig.suptitle("Oakquants signals: gross return by quintile "
                 "(Q1 = implied move cheap vs own history; numbers = t)\n"
                 "all four monotone at lag 15 — weak / U-shaped at lag 2",
                 fontsize=11)
    fig.tight_layout(rect=[0, 0, 1, 0.93])
    fig.savefig(f"{OUT}/10_oq_quintiles.png", dpi=110)

    # ---- fig 11: upgraded gate ---------------------------------------------
    # same universe as oq_signals.py's combined-gate run: >=4 prior events
    # for BOTH signals (mv_rk non-null), median split on that universe
    s4 = "s4_vs_avg_rlz"
    u = t15.dropna(subset=["mv_rk"])
    med = u[s4].median()
    old = u[(u.when == "AMC") & (u.mv_rk < 0.33)]
    new = u[(u.when == "AMC") & (u[s4] <= med)]

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12.5, 4.8),
                                   gridspec_kw={"width_ratios": [1.7, 1]})
    for sub, lab, c, lw in [
            (u, f"lag 15 ungated (n={len(u)}, "
                f"{u[R].mean()*100:+.2f}%)", GRAY, 1.2),
            (old, f"old gate: AMC + rank-cheap 1/3 (n={len(old)}, "
                  f"{old[R].mean()*100:+.2f}%)", BLUE, 1.4),
            (new, f"NEW gate: AMC + s4 low-half (n={len(new)}, "
                  f"{new[R].mean()*100:+.2f}%)", GREEN, 1.8)]:
        s = sub.sort_values("exit")
        ax1.plot(s.exit, s[R].cumsum() * 100, label=lab, color=c, lw=lw)
    ax1.axhline(0, color="k", lw=0.7)
    ax1.set_ylabel("cumulative %/spot (sum over trades)")
    ax1.set_title("upgraded gate: more trades AND more edge")
    ax1.legend(fontsize=8, loc="upper left")

    h = u.dropna(subset=[s4])
    m2 = h[s4].median()
    cells = [("rank cheap\n+ s4 low", h[(h.mv_rk < 0.5) & (h[s4] <= m2)]),
             ("rank cheap\n+ s4 high", h[(h.mv_rk < 0.5) & (h[s4] > m2)]),
             ("rank rich\n+ s4 low", h[(h.mv_rk >= 0.5) & (h[s4] <= m2)]),
             ("rank rich\n+ s4 high", h[(h.mv_rk >= 0.5) & (h[s4] > m2)])]
    means = [s[R].mean() * 100 for _, s in cells]
    ax2.bar(np.arange(4), means,
            color=[GREEN, BLUE, BLUE, RED])
    for xi, (lab_, s) in enumerate(cells):
        ax2.text(xi, means[xi], f"t={tstat(s[R]):.1f}", ha="center",
                 va="bottom" if means[xi] >= 0 else "top", fontsize=8,
                 color="#52514e")
    ax2.set_xticks(np.arange(4), [lab_ for lab_, _ in cells], fontsize=8)
    ax2.axhline(0, color="k", lw=0.7)
    ax2.set_ylabel("mean %/spot (gross mid)")
    ax2.set_title("double-sort (lag 15): s4 adds on top of\n"
                  "the old implied-rank gate", fontsize=10)
    fig.tight_layout()
    fig.savefig(f"{OUT}/11b_s4_gate.png", dpi=110)

    print(f"old gate n={len(old)} mean {old[R].mean()*100:+.3f} "
          f"t={tstat(old[R]):+.2f}")
    print(f"new gate n={len(new)} mean {new[R].mean()*100:+.3f} "
          f"t={tstat(new[R]):+.2f}")
    print(f"wrote 10_oq_quintiles.png, 11b_s4_gate.png to {OUT}/")


if __name__ == "__main__":
    main()
