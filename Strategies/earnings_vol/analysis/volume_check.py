"""
Profitability by option trading volume / open interest (GROSS at mid).

Joins entry-day liquidity of the traded straddle (cVolu+pVolu contracts,
cOi+pOi, and dollar volume = contracts x mid x 100) onto the lag-15 trades,
then sorts gross return by liquidity bucket:
  - quintiles of straddle contract volume (pooled)
  - quintiles of straddle $ volume (pooled)
  - quintiles of OI (pooled)
  - within-ticker past-only-style check: volume rank vs the name's own trades
    (separates "liquid names vs illiquid names" from "busy day vs quiet day")
Also crosses the AMC+cheap gate with volume halves.

Writes cache/trades_prern_vol.parquet (lag-15 trades + liquidity columns)
and figures/8_by_volume.png.  Run from analysis/:  python3 volume_check.py
"""
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from gate import add_mv_rank

SMV = os.path.expanduser(
    "~/trading/Systems/Options/Strategies/ff_calendar_v2/cache/smv_single")
R = "ret_spot_mid"


def t(r):
    return r.mean() / r.std() * np.sqrt(len(r))


def block(s, label):
    if len(s) < 30:
        return None
    r = s[R]
    return dict(sel=label, n=len(s), mean_pct=r.mean() * 100, t=t(r),
                win_pct=(r > 0).mean() * 100)


def show(rows, title):
    df = pd.DataFrame([b for b in rows if b])
    print(f"\n== {title} ==")
    print(df.round(3).to_string(index=False))


def attach_liquidity(t15):
    out = []
    for tkr, sub in t15.groupby("ticker"):
        path = f"{SMV}/{tkr}/part-0.parquet"
        if not os.path.exists(path):
            continue
        smv = pd.read_parquet(path, columns=["tdate", "expir", "strike",
                                             "cVolu", "pVolu", "cOi", "pOi"])
        smv["tdate"] = pd.to_datetime(smv.tdate)
        smv["expir"] = pd.to_datetime(smv.expir)
        m = sub.merge(smv, left_on=["entry", "expir", "strike"],
                      right_on=["tdate", "expir", "strike"], how="left")
        out.append(m.drop(columns=["tdate"]))
    d = pd.concat(out, ignore_index=True)
    d["volu"] = d.cVolu.fillna(0) + d.pVolu.fillna(0)      # contracts
    d["oi"] = d.cOi.fillna(0) + d.pOi.fillna(0)
    d["dvolu"] = d.volu * d.debit_mid * 100                # $ traded that day
    return d


def main():
    tr = pd.read_parquet("../cache/trades_prern.parquet")
    t15 = tr[tr.lag == 15].copy()
    d = attach_liquidity(t15)
    d.to_parquet("../cache/trades_prern_vol.parquet")
    print(f"{len(d)} lag-15 trades, liquidity attached "
          f"(volume missing on {(d.volu == 0).mean()*100:.1f}%)")
    print("straddle entry-day volume: median "
          f"{d.volu.median():.0f} contracts, p10 {d.volu.quantile(.1):.0f}, "
          f"p90 {d.volu.quantile(.9):.0f}; OI median {d.oi.median():.0f}")

    labels = ["Q1 lowest", "Q2", "Q3", "Q4", "Q5 highest"]
    for col, name in [("volu", "straddle contract volume"),
                      ("dvolu", "straddle $ volume"),
                      ("oi", "open interest")]:
        d["q"] = pd.qcut(d[col].rank(method="first"), 5, labels=False)
        show([block(s, labels[int(q)]) for q, s in d.groupby("q")],
             f"lag 15 by entry-day {name} quintile (pooled)")

    # within-ticker: busy vs quiet days for the SAME name
    d["vrk"] = d.groupby("ticker").volu.rank(pct=True)
    d["q"] = pd.cut(d.vrk, np.linspace(0, 1, 6), labels=False,
                    include_lowest=True)
    show([block(s, labels[int(q)]) for q, s in d.groupby("q")],
         "lag 15 by volume rank WITHIN ticker (own-name busy vs quiet days)")

    # gate x volume halves
    g = add_mv_rank(d).dropna(subset=["mv_rk"])
    gate = g[(g.when == "AMC") & (g.mv_rk < 0.33)]
    med = d.volu.median()
    show([block(gate[gate.volu <= med], "gate, volume <= pooled median"),
          block(gate[gate.volu > med], "gate, volume > pooled median")],
         "AMC + cheap gate split by straddle volume")

    # figure 8
    fig, axes = plt.subplots(1, 2, figsize=(11.5, 4.4))
    for ax, col, name in [(axes[0], "dvolu", "straddle $ volume (pooled)"),
                          (axes[1], "vrk", "volume rank within ticker")]:
        if col == "dvolu":
            d["q"] = pd.qcut(d[col].rank(method="first"), 5, labels=False)
        else:
            d["q"] = pd.cut(d[col], np.linspace(0, 1, 6), labels=False,
                            include_lowest=True)
        rows = [(labels[int(q)], s[R].mean() * 100, t(s[R]))
                for q, s in d.groupby("q")]
        labs, means, ts = zip(*rows)
        x = np.arange(len(labs))
        ax.bar(x, means, color="#4878cf")
        for xi, m_, t_ in zip(x, means, ts):
            ax.text(xi, m_, f"t={t_:.1f}", ha="center",
                    va="bottom" if m_ >= 0 else "top", fontsize=8)
        ax.set_xticks(x, labs, fontsize=8)
        ax.axhline(0, color="k", lw=0.7)
        ax.set_ylabel("mean %/spot (gross mid)")
        ax.set_title(f"lag 15: gross return by {name}")
    fig.tight_layout()
    fig.savefig("../figures/8_by_volume.png", dpi=110)
    print("\nwrote ../figures/8_by_volume.png")


if __name__ == "__main__":
    main()
