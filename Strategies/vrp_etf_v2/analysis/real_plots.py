#!/usr/bin/env python3
"""
The VRP retest, on real quotes. Two figures, deliberately split:

  42_vrp_gross.png  -- GROSS only. Is the trade real, do the signals sort it, and how much
                       of the old study's conclusion was an artefact of its proxy target?
  43_vrp_cost.png   -- the cost reckoning, read second.

Gross and cost are separated because they are proportional here: the names with the fattest
edge also have the widest spreads, and judging on net alone makes a real effect look like
nothing. Every panel carries a Sharpe.

Run: /home/marco/trading/.venv_orats/bin/python real_plots.py
"""
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from common import BASE

# validated categorical palette, carried over from ff_calendar_v2/analysis/gross_wide.py
C = {"a": "#2a78d6", "b": "#eb6834", "c": "#1baf7a", "d": "#eda100"}
RAMP = ["#7db3e6", "#4a8ad2", "#2a68b4", "#1f4a80", "#152f52"]     # ordinal, 5 steps
INK, MUTED, GRID = "#1c1c1c", "#5c5c5c", "#dcdcdc"
BG = "#fcfcfb"
IDX = pd.period_range("2013-01", "2024-11", freq="M").to_timestamp()
LIQ = 2.5e5          # "genuinely liquid ETF" -- see panel A of the cost figure


def style(ax, title, ylab=None, xlab=None):
    ax.set_title(title, fontsize=10.5, color=INK, loc="left", pad=8)
    if ylab:
        ax.set_ylabel(ylab, fontsize=8.5, color=MUTED)
    if xlab:
        ax.set_xlabel(xlab, fontsize=8.5, color=MUTED)
    ax.grid(color=GRID, lw=0.7, zorder=0)
    ax.set_axisbelow(True)
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    for s in ("left", "bottom"):
        ax.spines[s].set_color(GRID)
    ax.tick_params(colors=MUTED, labelsize=8)


def prep():
    t = pd.read_parquet(f"{BASE}/cache/real_trades_etf.parquet")
    t["ym"] = t.tradeDate.values.astype("datetime64[M]")
    t["cost"] = t.r_mid - t.r_taker
    t["q_ivrv"] = t.groupby("ym").f_ivrv.rank(pct=True)
    t["q_vov"] = t.groupby("ym").f_vov.rank(pct=True)
    t["q_ffr"] = 1 - t.groupby("ym").f_ffr.rank(pct=True)      # low ratio = backwardation
    t["q_blend"] = (t.q_ivrv + t.q_vov + t.q_ffr) / 3
    return t


def curve(sel, col):
    g = sel.groupby("ym")[col].mean().reindex(IDX, fill_value=0.0)
    return g, (1 + g).cumprod()


def sharpe(g):
    return g.mean() / g.std() * np.sqrt(12) if g.std() > 0 else np.nan


# ===================================================================== figure 1: GROSS
def gross_fig(t):
    fig, axes = plt.subplots(2, 2, figsize=(14.5, 9.6))
    fig.patch.set_facecolor(BG)
    for ax in axes.ravel():
        ax.set_facecolor(BG)

    # ---- A. the proxy vs the trade it stood in for
    ax = axes[0, 0]
    s = t.sample(4000, random_state=0)
    ax.scatter(100 * s.y_margin, 100 * s.r_mid, s=9, color=C["a"], alpha=.35,
               edgecolor="none", zorder=3)
    lim = (-25, 25)
    ax.plot(lim, lim, color=MUTED, lw=1.0, ls="--", zorder=4)
    r = t.r_mid.corr(t.y_margin)
    ax.set_xlim(*lim)
    ax.set_ylim(-25, 30)
    ax.axhline(0, color=INK, lw=1.0, zorder=2)
    ax.axvline(0, color=INK, lw=1.0, zorder=2)
    ax.annotate(f"corr {r:+.2f}   R² {r**2:.2f}\n"
                f"proxy mean {100*t.y_margin.mean():+.2f}%/trade\n"
                f"REAL  mean {100*t.r_mid.mean():+.2f}%/trade",
                (0.03, 0.97), xycoords="axes fraction", va="top", fontsize=9.5,
                color=INK, fontweight="bold")
    style(ax, "A · The old study scored a formula, not the trade",
          "REAL delta-hedged straddle, % of margin",
          "the proxy   1 − (RV/IV)²   on margin  (what the old study measured)")

    # ---- B. do the signals sort the real P&L?
    ax = axes[0, 1]
    sigs = [("q_ivrv", "1  IV/RV"), ("q_vov", "+  volOfVol"),
            ("q_ivpct_", "2  ivPctile1y"), ("q_ffr", "3  backwardation\n   (signal 3 inverted)")]
    t = t.copy()
    t["q_ivpct_"] = t.groupby("ym").f_ivpct.rank(pct=True)
    w = 0.2
    x = np.arange(5)
    for i, (col, name) in enumerate(sigs):
        q = pd.qcut(t[col].rank(method="first"), 5, labels=False)
        m = t.groupby(q).r_mid.agg(["mean", "std", "size"])
        sh = m["mean"] / m["std"] * np.sqrt(12)
        ax.bar(x + (i - 1.5) * w, 100 * m["mean"], w * 0.9,
               color=list(C.values())[i], edgecolor="white", lw=0.8, zorder=3,
               label=f"{name}   Q5−Q1 {100*(m['mean'].iloc[-1]-m['mean'].iloc[0]):+.2f}pp")
        for xi, v, s_ in zip(x, m["mean"].values, sh.values):
            ax.annotate(f"{s_:.2f}", (xi + (i - 1.5) * w, 100 * v),
                        textcoords="offset points", xytext=(0, 3), ha="center",
                        rotation=90, fontsize=6.6, color=MUTED)
    ax.set_xticks(x)
    ax.set_xticklabels(["Q1 (low)", "Q2", "Q3", "Q4", "Q5 (high)"], fontsize=8.4)
    ax.axhline(0, color=INK, lw=1.0, zorder=4)
    ax.legend(fontsize=7.6, frameon=False, ncol=2, loc="upper left")
    ax.set_ylim(0, 3.1)
    style(ax, "B · All four signals sort the REAL trade  (number over each bar = annualised Sharpe)",
          "gross, % of margin per ~30d trade")

    # ---- C. gross equity
    ax = axes[1, 0]
    books = [(t, "blind: sell every ETF", C["b"], 1.6),
             (t[t.q_ivrv >= .9], "IV/RV top 10%", C["a"], 1.6),
             (t[t.q_ffr >= .9], "backwardation top 10%", C["d"], 1.6),
             (t[t.q_blend >= .8], "3-signal blend top 20%", C["c"], 2.4)]
    for sel, lbl, col, lw in books:
        g, eq = curve(sel, "r_mid")
        ax.plot(eq.index, eq.values, color=col, lw=lw, zorder=3,
                label=f"{lbl}   Sh {sharpe(g):.2f}   {eq.iloc[-1]:.1f}×")
    ax.set_yscale("log")
    ax.set_yticks([1, 2, 5, 10, 20, 50])
    ax.get_yaxis().set_major_formatter(matplotlib.ticker.ScalarFormatter())
    ax.legend(fontsize=8.2, frameon=False, loc="upper left")
    style(ax, "C · Gross equity — monthly, equal weight, idle months flat",
          "growth of 1 (log scale)")

    # ---- D. by year
    ax = axes[1, 1]
    y = t.groupby(t.tradeDate.dt.year).r_mid.agg(["mean", "std", "size"])
    ysh = y["mean"] / y["std"] * np.sqrt(12)
    cols = [C["a"] if v > 0 else C["b"] for v in y["mean"]]
    ax.bar(y.index, 100 * y["mean"], 0.68, color=cols, edgecolor="white", lw=1.0, zorder=3)
    for xi, v, s_ in zip(y.index, y["mean"].values, ysh.values):
        ax.annotate(f"{s_:.2f}", (xi, 100 * v), textcoords="offset points",
                    xytext=(0, 4), ha="center", fontsize=7.2, color=MUTED)
    ax.axhline(0, color=INK, lw=1.0, zorder=4)
    ax.set_ylim(0, 2.9)
    style(ax, f"D · Gross positive in {int((y['mean']>0).sum())}/{len(y)} years  "
              "(number over each bar = that year's Sharpe)",
          "gross, % of margin per trade")

    fig.suptitle("Option-Quants VRP retest — the trade is real; the old study's target was not",
                 fontsize=13.5, color=INK, x=0.006, ha="left", y=0.985)
    fig.text(0.006, 0.952,
             f"{len(t):,} REAL delta-hedged short ATM straddles on {t.ticker.nunique()} clean ETFs, "
             f"2013-01..2024-11 · NBBO quotes from _data/strikes, daily rehedge at the archive's "
             f"own delta, settled at |S−K|",
             fontsize=8.8, color=MUTED, ha="left")
    fig.text(0.006, 0.929,
             "GROSS throughout — mid fills, no cost anywhere. The cost reckoning is figure 43, "
             "and it is where this changes.",
             fontsize=8.8, color=MUTED, ha="left")
    fig.tight_layout(rect=[0, 0.005, 1, 0.918])
    p = f"{BASE}/out/plots/42_vrp_gross.png"
    fig.savefig(p, dpi=150, facecolor=BG)
    print("wrote", p)


# ===================================================================== figure 2: COST
def cost_fig(t):
    fig, axes = plt.subplots(1, 3, figsize=(16.5, 5.4))
    fig.patch.set_facecolor(BG)
    for ax in axes:
        ax.set_facecolor(BG)

    # ---- A. f* by liquidity
    ax = axes[0]
    edges = [2500, 1e4, 5e4, 2.5e5, 1e6, np.inf]
    lbl = ["2.5–10k", "10–50k", "50–250k", "250k–1M", "1M+"]
    t = t.copy()
    t["vb"] = pd.cut(t.avgOptVolu20d, edges, labels=lbl)
    g = t.groupby("vb", observed=True).agg(n=("r_mid", "size"), gross=("r_mid", "mean"),
                                           cost=("cost", "mean"), sd=("r_mid", "std"))
    g["f"] = g.gross / g.cost
    x = np.arange(len(g))
    ax.bar(x, g.f.values, 0.62, color=RAMP, edgecolor="white", lw=1.0, zorder=3)
    ax.axhline(1.0, color=C["b"], lw=1.6, ls="--", zorder=4)
    # sits at 1.25, not just above the line: the n=/Sh= label over the 50-250k bar reaches ~1.01
    ax.annotate("above this line you can pay the FULL spread\nand still profit",
                (-0.42, 1.20), ha="left", fontsize=7.8, color=C["b"])
    for xi, (f, sh, n) in enumerate(zip(g.f.values, (g.gross / g.sd * np.sqrt(12)).values, g.n.values)):
        ax.annotate(f"f* {f:.2f}\nSh {sh:.2f}\nn={n:,}", (xi, f), textcoords="offset points",
                    xytext=(0, 5), ha="center", fontsize=7.6, color=MUTED)
    ax.set_xticks(x)
    ax.set_xticklabels(lbl, fontsize=8.4)
    ax.set_ylim(0, 3.1)
    style(ax, "A · Break-even fill fraction rises with liquidity",
          "f* = gross ÷ round-trip cost", "avgOptVolu20d at signal date")

    # ---- B. gross vs net on the liquid set
    ax = axes[1]
    L = t[t.avgOptVolu20d >= LIQ].copy()
    L["q"] = (L.groupby("ym").f_ivrv.rank(pct=True)
              + L.groupby("ym").f_vov.rank(pct=True)
              + (1 - L.groupby("ym").f_ffr.rank(pct=True))) / 3
    for sel, col, cc, lw, lbl_ in [(L, "r_mid", C["a"], 2.2, "gross, all liquid ETFs"),
                                   (L, "r_taker", C["b"], 2.2, "NET, all liquid ETFs"),
                                   (L[L.q >= .5], "r_taker", C["c"], 2.2, "NET, blend top 50%")]:
        g_, eq = curve(sel, col)
        ax.plot(eq.index, eq.values, color=cc, lw=lw, zorder=3,
                label=f"{lbl_}   Sh {sharpe(g_):.2f}   {eq.iloc[-1]:.2f}×")
    ax.axhline(1.0, color=INK, lw=1.0, zorder=2)
    ax.legend(fontsize=8.2, frameon=False, loc="upper left")
    style(ax, f"B · The {L.ticker.nunique()} genuinely liquid ETFs survive the spread",
          "growth of 1")

    # ---- C. per-ticker gross vs cost
    ax = axes[2]
    p = t.groupby("ticker").agg(n=("r_mid", "size"), gross=("r_mid", "mean"),
                                cost=("cost", "mean"), volu=("avgOptVolu20d", "median"))
    p = p[p.n >= 12]
    sc = ax.scatter(100 * p.cost, 100 * p.gross, s=np.clip(p.n / 2.2, 8, 130),
                    c=np.log10(p.volu), cmap=matplotlib.colors.LinearSegmentedColormap.from_list(
                        "r", RAMP), alpha=.85, edgecolor="white", lw=.7, zorder=3)
    m = max(100 * p.cost.max(), 100 * p.gross.max()) * 1.02
    ax.plot([0, m], [0, m], color=C["b"], lw=1.6, ls="--", zorder=4)
    ax.annotate("break-even (gross = cost)", (m * .62, m * .68), fontsize=8,
                color=C["b"], rotation=38)
    # the big-name cluster sits in one corner, so each label is pushed out on its own
    # bearing with a leader line rather than nudged -- nudging alone still collided
    for tk, off in [("SPY", (-30, -16)), ("QQQ", (-34, 2)), ("IWM", (-30, 18)),
                    ("EEM", (16, 20)), ("HYG", (20, 6)), ("USO", (26, -6))]:
        if tk in p.index:
            ax.annotate(tk, (100 * p.cost[tk], 100 * p.gross[tk]),
                        textcoords="offset points", xytext=off, fontsize=7.6, color=INK,
                        arrowprops=dict(arrowstyle="-", color=MUTED, lw=0.6,
                                        shrinkA=0, shrinkB=3))
    cb = fig.colorbar(sc, ax=ax, pad=0.02)
    cb.set_label("log₁₀ median option volume", fontsize=8, color=MUTED)
    cb.ax.tick_params(colors=MUTED, labelsize=7.5)
    ax.set_xlim(0, m)
    ax.set_ylim(0, m)
    style(ax, "C · Above the line = tradeable after costs",
          "gross edge, % of margin", "round-trip cost, % of margin")

    fig.suptitle("VRP retest — the cost reckoning: thin ETFs give it all back, liquid ones don't",
                 fontsize=13, color=INK, x=0.005, ha="left", y=0.985)
    fig.text(0.005, 0.925,
             "cost = sell both legs at the BID instead of mid, plus 2bp per share rehedge. "
             "No exit spread — the straddle is held to expiry and settles at |S−K|. "
             "Bubble area = trade count.",
             fontsize=8.6, color=MUTED, ha="left")
    fig.tight_layout(rect=[0, 0.005, 1, 0.905])
    p2 = f"{BASE}/out/plots/43_vrp_cost.png"
    fig.savefig(p2, dpi=150, facecolor=BG)
    print("wrote", p2)


if __name__ == "__main__":
    t = prep()
    gross_fig(t)
    cost_fig(t)
