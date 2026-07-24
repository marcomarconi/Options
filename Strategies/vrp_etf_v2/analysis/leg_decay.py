"""
Does the VRP screener's BACKWARDATION leg decay on the same 2024+ schedule that
killed the Vasquez term-structure straddle sort?

Both books trade the SAME factor: the IV term-structure slope.
  - Vasquez : log(iv1yr/iv30d), cross-sectional straddle L/S.  Short-backwardation
              leg went flat-to-negative for 30 months (2024-2026).
  - VRP     : q_ffr = 1 - rank(30/60 fwd ratio) = backwardation short-signal, one of
              three blended ranks.  f_ffr is the raw ratio (high = contango).

Here we form, per signal, a monthly cross-sectional L/S spread on the REAL-quote
short-vol return r_mid (GROSS/mid, per standing preference), top-quintile minus
bottom-quintile, and track each leg over time.  Sharpe annotated on the plot.

Real quotes end 2024-11 (strike archive), so this covers the FIRST 11 months of the
Vasquez break; the 2025-26 tail needs the core label (see note in output).

Run from analysis/:  python3 leg_decay.py
"""
import numpy as np, pandas as pd
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

R = "r_mid"
INK, MUTED, GRID, BG = "#1c1c1c", "#5c5c5c", "#dcdcdc", "#fcfcfb"
# short-oriented legs: (column, ascending? high-rank = attractive-to-short, color, label)
LEGS = [
    ("ivrv",  lambda d: d.f_ivrv,   "#1baf7a", "IV/RV  log(iv30/hv20)"),
    ("vov",   lambda d: d.f_vov,    "#e08a1e", "vol-of-vol"),
    ("back",  lambda d: -d.f_ffr,   "#c1272d", "BACKWARDATION  (−fwd ratio)"),
    ("ivlvl", lambda d: d.f_ivlvl,  "#8a8a8a", "IV level  (control)"),
]


def monthly_ls(d, sig):
    """top-quintile minus bottom-quintile mean r_mid, per calendar month."""
    d = d.assign(s=sig(d)).dropna(subset=["s", R]).copy()
    d["ym"] = d.tradeDate.values.astype("datetime64[M]")
    def spread(g):
        if len(g) < 20:
            return np.nan
        q = g.s.rank(pct=True)
        return g[R][q >= .8].mean() - g[R][q <= .2].mean()
    return d.groupby("ym").apply(spread).dropna()


def sharpe(m):
    return m.mean() / m.std() * np.sqrt(12) if m.std() > 0 else np.nan


def blend_ls(d):
    d = d.assign(bl=(d.f_ivrv.rank(pct=True) + d.f_vov.rank(pct=True)
                     + (1 - d.f_ffr.rank(pct=True)))).dropna(subset=["bl", R]).copy()
    # rebuild blend PER MONTH so ranks are cross-sectional within the month
    d["ym"] = d.tradeDate.values.astype("datetime64[M]")
    def spread(g):
        if len(g) < 20:
            return np.nan
        bl = (g.f_ivrv.rank(pct=True) + g.f_vov.rank(pct=True)
              + (1 - g.f_ffr.rank(pct=True)))
        q = bl.rank(pct=True)
        return g[R][q >= .8].mean() - g[R][q <= .2].mean()
    return d.groupby("ym").apply(spread).dropna()


def main():
    d = pd.read_parquet("../cache/real_trades_stock.parquet")
    print(f"stock book: {len(d):,} trades, {d.tradeDate.min().date()}..{d.tradeDate.max().date()}\n")

    series = {name: monthly_ls(d, fn) for name, _, _, name_ in
              [(n, None, None, l) for n, fn, c, l in LEGS]
              for fn in [dict((n, f) for n, f, c, l in LEGS)[name]]}
    blend = blend_ls(d)

    # ---- per-year table, each leg ----
    print("Monthly L/S spread (top-Q minus bottom-Q, r_mid gross), by year:")
    hdr = "year   " + "".join(f"{l.split()[0]:>9}" for _, _, _, l in LEGS) + f"{'BLEND':>9}"
    print(hdr); print("-" * len(hdr))
    allyears = sorted({ym.year for ym in blend.index})
    def yr(m, y): return m[[i.year == y for i in m.index]]
    for y in allyears:
        row = f"{y}  "
        for name, _, _, _ in LEGS:
            v = yr(series[name], y)
            row += f"{v.mean()*100:>+8.2f}%" if len(v) else f"{'—':>9}"
        vb = yr(blend, y)
        row += f"{vb.mean()*100:>+8.2f}%" if len(vb) else f"{'—':>9}"
        print(row)

    # ---- pre/post 2024 break (Vasquez split date) ----
    print("\nPre/post 2024-01 (Vasquez break date), monthly-mean L/S and Sharpe:")
    for name, _, _, lab in LEGS + [("blend", None, None, "BLEND")]:
        m = blend if name == "blend" else series[name]
        pre = m[[i.year < 2024 for i in m.index]]
        post = m[[i.year >= 2024 for i in m.index]]
        print(f"  {lab:28s} pre {pre.mean()*100:+.2f}%/mo Sh {sharpe(pre):+.2f}   "
              f"| 2024 {post.mean()*100:+.2f}%/mo Sh {sharpe(post):+.2f}  (n={len(post)}mo)")

    # ---- plot: cumulative L/S per leg + blend ----
    plt.rcParams.update({"font.size": 9, "figure.facecolor": BG})
    fig, ax = plt.subplots(figsize=(12.5, 6.2))
    for name, _, color, lab in LEGS:
        m = series[name].sort_index()
        ax.plot(m.index, (m.cumsum() * 100).values, color=color, lw=2.0 if name == "back" else 1.4,
                label=f"{lab}   Sh {sharpe(m):+.2f}   Σ {m.sum()*100:+.0f}%",
                zorder=5 if name == "back" else 3)
    mb = blend.sort_index()
    ax.plot(mb.index, (mb.cumsum() * 100).values, color=INK, lw=2.4, ls=(0, (4, 1.5)),
            label=f"BLEND (traded)   Sh {sharpe(mb):+.2f}   Σ {mb.sum()*100:+.0f}%", zorder=6)
    ax.axvline(pd.Timestamp("2024-01-01"), color=MUTED, lw=1.1, ls=":", zorder=1)
    ax.text(pd.Timestamp("2024-01-15"), ax.get_ylim()[0], " Vasquez break", color=MUTED,
            fontsize=8, va="bottom")
    ax.axhline(0, color=INK, lw=.9)
    ax.set_facecolor(BG); ax.grid(color=GRID, lw=.7); ax.set_axisbelow(True)
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    ax.set_title("VRP legs decomposed: cumulative monthly L/S spread on real-quote short-vol return "
                 "(gross/mid, single names)", loc="left", color=INK, fontsize=10.5)
    ax.set_ylabel("cumulative top-Q − bottom-Q return (sum of monthly %)", color=MUTED, fontsize=9)
    ax.tick_params(colors=MUTED, labelsize=8)
    ax.legend(frameon=False, fontsize=8.5, loc="upper left")
    fig.tight_layout()
    p = "../out/plots/44_leg_decay.png"
    fig.savefig(p, dpi=150, facecolor=BG)
    print("\nwrote", p)


if __name__ == "__main__":
    main()
