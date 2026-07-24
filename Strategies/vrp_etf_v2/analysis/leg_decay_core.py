"""
EXTENSION of leg_decay.py past the strike archive, to 2026-06, via the Vasquez
CORE straddle label (proven == NBBO mid to 8 dp; see vasquez_ts/README).

Question: the VRP screener's backwardation leg uses the 30/60 short-end slope.
Vasquez's slope is the 30d-vs-1yr long-end slope, and it went dead 2024+. Do BOTH
tenors of the same term-structure factor die on the same schedule?

We compute, on the vasquez core panel (151,951 straddle cycles, 2007-2026):
  - VRP short-end signal  f_ffr = flat/std 30-60d fwd ratio  (high = contango)
  - Vasquez long-end slope = log(iv1yr/iv30d)                (high = contango)
and, per month, the cross-sectional L/S of the straddle return y_prem =
mean(top-Q contango) - mean(bottom-Q backwardation). Positive = "contango
straddles beat backwardated ones" = the effect is alive (and equivalently, the
VRP short-backwardation leg has an edge). Sharpe annotated. GROSS (y_prem is mid).

Run from analysis/:  python3 leg_decay_core.py
"""
import numpy as np, pandas as pd
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

CORE = "../../vasquez_ts/cache/panel_core.parquet"
INK, MUTED, GRID, BG = "#1c1c1c", "#5c5c5c", "#dcdcdc", "#fcfcfb"


def f_ffr(v1, v2, t1=30.0, t2=60.0):
    """VRP's 30/60 fwd-vol ratio, exactly as vrp_etf_v2/analysis/common.py."""
    fstd = np.sqrt(np.maximum((v2 ** 2 * t2 - v1 ** 2 * t1) / (t2 - t1), 1e-6))
    fflat = (v2 * np.sqrt(t2) - v1 * np.sqrt(t1)) / (np.sqrt(t2) - np.sqrt(t1))
    return np.clip(fflat / fstd, -10, 10)


def monthly_ls(d, sigcol):
    """mean y_prem in top-Q(contango) minus bottom-Q(backwardation), per month."""
    def spread(g):
        if len(g) < 25:
            return np.nan
        q = g[sigcol].rank(pct=True)
        return g.y_prem[q >= .8].mean() - g.y_prem[q <= .2].mean()
    return d.groupby("ym").apply(spread).dropna()


def sharpe(m):
    return m.mean() / m.std() * np.sqrt(12) if m.std() > 0 else np.nan


def main():
    d = pd.read_parquet(CORE, columns=["ticker", "iv30d", "iv60d", "iv1yr",
                                        "y_prem", "ym", "avgOptVolu20d", "pxAtmIv"])
    d = d.dropna(subset=["iv30d", "iv60d", "y_prem"])
    d = d[(d.avgOptVolu20d >= 500) & (d.pxAtmIv >= 5)]          # match VRP liquidity floor
    d["ffr"] = f_ffr(d.iv30d.values, d.iv60d.values)
    iv1 = d.iv1yr.replace(0, np.nan)                            # 0 = ORATS sentinel, not real
    d["slope_1yr"] = np.log(iv1 / d.iv30d)
    d["ym"] = pd.to_datetime(d.ym.astype(str))
    print(f"core panel: {len(d):,} cycles, {d.ym.min().date()}..{d.ym.max().date()}\n")

    ffr = monthly_ls(d, "ffr")
    slp = monthly_ls(d.dropna(subset=["slope_1yr"]), "slope_1yr")

    print("Cross-sectional straddle-return L/S (contango − backwardation), %/mo by year:")
    print(f"{'year':6}{'VRP 30/60':>12}{'Vasquez 30d/1yr':>18}")
    for y in range(2007, 2027):
        a = ffr[[i.year == y for i in ffr.index]]
        b = slp[[i.year == y for i in slp.index]]
        sa = f"{a.mean()*100:+.2f}%" if len(a) else "—"
        sb = f"{b.mean()*100:+.2f}%" if len(b) else "—"
        print(f"{y:<6}{sa:>12}{sb:>18}")

    print("\nRegime split (Vasquez break = 2024-01):")
    for lab, m in [("VRP 30/60 backwardation", ffr), ("Vasquez 30d/1yr slope", slp)]:
        for tag, sub in [("2007-2023", m[[i.year < 2024 for i in m.index]]),
                         ("2024-2026", m[[i.year >= 2024 for i in m.index]])]:
            print(f"  {lab:26s} {tag}: {sub.mean()*100:+.2f}%/mo  Sh {sharpe(sub):+.2f}  (n={len(sub)}mo)")

    # ---- plot ----
    plt.rcParams.update({"font.size": 9, "figure.facecolor": BG})
    fig, ax = plt.subplots(figsize=(12.5, 6.2))
    for m, color, lab in [(ffr, "#c1272d", "VRP  30/60 backwardation leg"),
                          (slp, "#4a3aa7", "Vasquez  30d/1yr slope")]:
        m = m.sort_index()
        ax.plot(m.index, (m.cumsum() * 100).values, color=color, lw=2.1,
                label=f"{lab}   Sh {sharpe(m):+.2f}   Σ {m.sum()*100:+.0f}%")
    ax.axvline(pd.Timestamp("2024-01-01"), color=MUTED, lw=1.1, ls=":")
    ax.text(pd.Timestamp("2024-02-01"), ax.get_ylim()[1]*0.02, " 2024 break", color=MUTED, fontsize=8)
    ax.axhline(0, color=INK, lw=.9)
    ax.set_facecolor(BG); ax.grid(color=GRID, lw=.7); ax.set_axisbelow(True)
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    ax.set_title("Both tenors of the term-structure factor die together: cross-sectional straddle-return "
                 "L/S (contango − backwardation), core label 2007-2026",
                 loc="left", color=INK, fontsize=10.3)
    ax.set_ylabel("cumulative L/S straddle return (sum of monthly %)", color=MUTED, fontsize=9)
    ax.tick_params(colors=MUTED, labelsize=8)
    ax.legend(frameon=False, fontsize=9, loc="upper left")
    fig.tight_layout()
    p = "../out/plots/45_leg_decay_core.png"
    fig.savefig(p, dpi=150, facecolor=BG)
    print("\nwrote", p)


if __name__ == "__main__":
    main()
