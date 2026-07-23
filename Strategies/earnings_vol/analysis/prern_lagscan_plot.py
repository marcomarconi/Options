"""
Entry-distance profile of the LONG pre-earnings straddle.

Reads cache/trades_prern_lagscan.parquet (lag grid 2..30 step 2, fixed exit
at the pre-announcement close). Since exit is fixed, mean return vs lag maps
how the position's pnl accrues over its life; the marginal pnl of holding the
window [-b, -a] is the same-event difference ret(lag b) - ret(lag a),
EXCLUDING pairs where both lags map to the same entry date (weekends dilute
those bars toward zero — audit finding 2026-07-11).

GROSS ONLY (mid marks); gate = PAST-ONLY impErnMv rank (gate.py).
Writes figures/7_lag_profile.png and prints the table.
Run from analysis/:  python3 prern_lagscan_plot.py
"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from gate import add_mv_rank

R = "ret_spot_mid"


def prof(tr):
    rows = []
    for lag, s in tr.groupby("lag"):
        r = s[R]
        rows.append(dict(lag=lag, n=len(r), mean=r.mean() * 100,
                         t=r.mean() / r.std() * np.sqrt(len(r))))
    return pd.DataFrame(rows).sort_values("lag")


def main():
    tr = pd.read_parquet("../cache/trades_prern_lagscan.parquet")
    g = add_mv_rank(tr, by=("ticker", "lag")).dropna(subset=["mv_rk"])
    gate = g[(g.when == "AMC") & (g.mv_rk < 0.33)]

    p_all, p_gate = prof(tr), prof(gate)
    print("== ungated ==");  print(p_all.round(3).to_string(index=False))
    print("== AMC + cheap 1/3 (past-only rank) ==")
    print(p_gate.round(3).to_string(index=False))

    # marginal pnl of each extra 2-day window: pair adjacent lags on the same
    # event; drop pairs whose entries coincide (no window actually added)
    key = ["ticker", "ernDate"]
    w = tr.pivot_table(index=key, columns="lag", values=R)
    we = tr.pivot_table(index=key, columns="lag", values="entry",
                        aggfunc="first")
    lags = sorted(w.columns)
    marg = []
    for a, b in zip(lags, lags[1:]):
        d = (w[b] - w[a])[we[b] != we[a]].dropna()
        marg.append((f"{b}-{a}", d.mean() * 100,
                     d.mean() / d.std() * np.sqrt(len(d))))

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(11.5, 4.4))
    ax1.plot(p_all.lag, p_all["mean"], "o-", color="#888888",
             label="ungated")
    ax1.plot(p_gate.lag, p_gate["mean"], "o-", color="#4878cf",
             label="AMC + cheap 1/3 (past-only rank)")
    ax1.axhline(0, color="k", lw=0.7)
    ax1.set_xlabel("entry lag (calendar days before event)")
    ax1.set_ylabel("mean %/spot (gross mid)")
    ax1.set_title("mean return vs entry distance (exit fixed pre-event)")
    ax1.legend(fontsize=8)
    ax1.invert_xaxis()

    labs, m, ts = zip(*marg)
    x = np.arange(len(labs))
    ax2.bar(x, m, color=["#4878cf" if v >= 0 else "#d1615d" for v in m])
    for xi, v, t in zip(x, m, ts):
        ax2.text(xi, v, f"{t:.1f}", ha="center",
                 va="bottom" if v >= 0 else "top", fontsize=7)
    ax2.set_xticks(x, labs, rotation=45, fontsize=7)
    ax2.axhline(0, color="k", lw=0.7)
    ax2.set_xlabel("lag pair (longer minus shorter, same event, "
                   "distinct entries)")
    ax2.set_ylabel("marginal %/spot of the extra window")
    ax2.set_title("where in the pre-event window the pnl accrues (t on bars)")
    ax2.invert_xaxis()
    fig.tight_layout()
    fig.savefig("../figures/7_lag_profile.png", dpi=110)
    print("wrote ../figures/7_lag_profile.png")


if __name__ == "__main__":
    main()
