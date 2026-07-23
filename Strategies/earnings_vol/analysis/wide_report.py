"""
Wide pre-earnings straddle retest — the reckoning across the full universe.

Gross first (mid), then cost by a fill-fraction model (f of the entry+exit spread;
f=0 mid, f=0.5 realistic, f=1 full taker), split by liquidity — the lesson from
skew_verticals being that an apparent edge can be an illiquid-quote artifact.
Returns are per event, in % of SPOT (house convention; comparable to the baseline).

Run from analysis/:  python3 wide_report.py
"""
import os
import numpy as np, pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

R = "/home/marco/trading/Systems/Options/Strategies/earnings_vol"
EDGES = [0, 1e3, 1e4, 5e4, 2.5e5, 1e12]
BLAB = ["<1k", "1-10k", "10-50k", "50-250k", "250k+"]
RAMP = ["#d98a3d", "#c25b3a", "#2a68b4", "#1f8a5c", "#154a8a"]
INK, MUTED, GRID, BG = "#1c1c1c", "#5c5c5c", "#dcdcdc", "#fcfcfb"


def ret_at(t, f):
    """return in % of spot at fill fraction f of the entry+exit spread."""
    pay = t.debit_mid + f * (t.debit_ask - t.debit_mid)          # entry toward ask
    recv = t.exit_mid - f * (t.exit_mid - t.exit_bid)            # exit toward bid
    return (recv - pay) / t.spot


def tstat(r):
    return r.mean() / r.std() * np.sqrt(len(r)) if len(r) > 2 and r.std() > 0 else np.nan


def main():
    t = pd.read_parquet(f"{R}/cache/trades_prern_wide.parquet")
    t["entry"] = pd.to_datetime(t.entry); t["ernDate"] = pd.to_datetime(t.ernDate)
    t["yr"] = t.ernDate.dt.year
    t["vb"] = pd.cut(t.avgOptVolu20d, EDGES, labels=BLAB)
    t["r_gross"] = ret_at(t, 0.0); t["r_real"] = ret_at(t, 0.5); t["r_taker"] = ret_at(t, 1.0)
    print(f"WIDE: {len(t):,} trades, {t.ticker.nunique():,} tickers, "
          f"{t.yr.min()}..{t.yr.max()}\n")

    print("gross (mid) by entry lag — % of spot:")
    for lag, s in t.groupby("lag"):
        print(f"  lag {lag:2d}: n={len(s):6d}  gross {s.r_gross.mean()*100:+.3f}%  "
              f"t={tstat(s.r_gross):+.2f}  win {(s.r_gross>0).mean()*100:.0f}%  "
              f"real(f.5) {s.r_real.mean()*100:+.3f}%  taker {s.r_taker.mean()*100:+.3f}%")

    best = 5              # wide-universe optimum (edge concentrates near the event)
    b = t[t.lag == best]
    print(f"\nliquidity split at lag {best} (avgOptVolu20d) — % of spot:")
    print(f"{'bucket':9}{'n':>7}{'gross%':>9}{'t':>7}{'real%':>8}{'taker%':>8}{'win%':>6}")
    for bl in BLAB:
        x = b[b.vb == bl]
        if len(x) < 30: continue
        print(f"{bl:9}{len(x):>7}{x.r_gross.mean()*100:>9.3f}{tstat(x.r_gross):>7.2f}"
              f"{x.r_real.mean()*100:>8.3f}{x.r_taker.mean()*100:>8.3f}{(x.r_gross>0).mean()*100:>6.0f}")

    print(f"\ngross by year (lag {best}, % of spot):")
    g = b.groupby("yr").r_gross.mean() * 100
    print("  " + " ".join(f"{y}:{v:+.2f}" for y, v in g.items()))

    # ---- figure: gross/real/taker by liquidity + by year ----
    plt.rcParams.update({"font.size": 9, "figure.facecolor": BG})
    fig, ax = plt.subplots(1, 3, figsize=(16.5, 4.8))

    def style(a, title, ylab, xlab=None):
        a.set_facecolor(BG); a.grid(color=GRID, lw=.7); a.set_axisbelow(True)
        for s in ("top", "right"): a.spines[s].set_visible(False)
        a.set_title(title, loc="left", color=INK, fontsize=10.5)
        a.set_ylabel(ylab, color=MUTED, fontsize=9)
        if xlab: a.set_xlabel(xlab, color=MUTED, fontsize=9)
        a.tick_params(colors=MUTED, labelsize=8)

    # A. gross by lag: wide vs baseline
    base = pd.read_parquet(f"{R}/cache/trades_prern.parquet")
    lags = sorted(t.lag.unique())
    gw = [t[t.lag == l].r_gross.mean()*100 for l in lags]
    gb = [base[base.lag == l].ret_spot_mid.mean()*100 for l in lags]
    ax[0].plot(lags, gw, "o-", color="#2a78d6", lw=2, label=f"WIDE ({t.ticker.nunique()} names)")
    ax[0].plot(lags, gb, "s--", color=MUTED, lw=1.8, label=f"baseline (191 names)")
    ax[0].axhline(0, color=INK, lw=.9)
    style(ax[0], "A · gross IV run-up by entry lag", "gross, % of spot", "entry lag (days before event)")
    ax[0].legend(frameon=False, fontsize=8)

    # B. gross/real/taker by liquidity (lag 15)
    xs = np.arange(len(BLAB)); w = 0.26
    gg = [b[b.vb == bl].r_gross.mean()*100 for bl in BLAB]
    rr = [b[b.vb == bl].r_real.mean()*100 for bl in BLAB]
    tt = [b[b.vb == bl].r_taker.mean()*100 for bl in BLAB]
    ax[1].bar(xs - w, gg, w, color="#2a78d6", label="gross (mid)")
    ax[1].bar(xs, rr, w, color="#1baf7a", label="realistic (f=0.5)")
    ax[1].bar(xs + w, tt, w, color="#eb6834", label="full taker")
    ax[1].axhline(0, color=INK, lw=.9); ax[1].set_xticks(xs); ax[1].set_xticklabels(BLAB, fontsize=8)
    style(ax[1], f"B · lag {best} by liquidity — net clears only in the 50-250k 'Goldilocks' bucket", "% of spot", "avgOptVolu20d")
    ax[1].legend(frameon=False, fontsize=8)

    # C. gross by year (lag 15)
    cols = ["#2a78d6" if v > 0 else "#eb6834" for v in g.values]
    ax[2].bar(g.index, g.values, 0.7, color=cols)
    ax[2].axhline(0, color=INK, lw=.9)
    style(ax[2], f"C · gross by year (lag {best}) — {int((g>0).sum())}/{len(g)} positive", "gross, % of spot")

    fig.suptitle("Pre-earnings straddle retest — wide universe, real _data/strikes quotes",
                 fontsize=12.5, color=INK, x=0.006, ha="left")
    fig.tight_layout(rect=[0, 0, 1, 0.95])
    os.makedirs(f"{R}/figures", exist_ok=True)
    p = f"{R}/figures/wide_earnings.png"
    fig.savefig(p, dpi=150, facecolor=BG)
    print(f"\nwrote {p}")


if __name__ == "__main__":
    main()
