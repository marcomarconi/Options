"""
Market profile of the two pre-earnings straddle books: correlation / beta to
SPY, and behaviour vs VIX (both are long-gamma-into-the-event, so we expect
low SPY beta and POSITIVE co-movement with VIX spikes).

Monthly units: each book's monthly return is the sum of per-event %/spot with
exit in that month (the same additive unit book_equity.py uses — events cluster
in earnings season, so compounding is meaningless). SPY = monthly total return
from adj close; VIX = monthly change in the close (points).

Gross (mid) leads; realistic (f=0.5) appears only in the closing table and its
own figure, never mixed into the gross panels.

Data: Data/SPY.csv, Data/VIX.csv end 2024-02-05, so the study window is
2013-01 .. 2024-01 (~133 months). Stated on the figure.

Run from analysis/:  python3 market_beta.py
"""
import numpy as np, pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from oq_model import build_model_book, load_lag15

R = "/home/marco/trading/Systems/Options/Strategies/earnings_vol"
DATA = "/home/marco/trading/Systems/Options/Data"
INK, MUTED, GRID, BG = "#1c1c1c", "#5c5c5c", "#dcdcdc", "#fcfcfb"
SPYC, VIXC, STRATC = "#eb6834", "#8a5cd6", "#2a78d6"


def ret_at(t, f):
    pay = t.debit_mid + f * (t.debit_ask - t.debit_mid)
    recv = t.exit_mid - f * (t.exit_mid - t.exit_bid)
    return (recv - pay) / t.spot


def lag2_book():
    ls = pd.read_parquet(f"{R}/cache/trades_prern_lagscan.parquet")
    d = ls[ls.lag == 2].copy()
    d["exit"] = pd.to_datetime(d.exit)
    return d.sort_values("exit")


def monthly_strat(d, f):
    """Monthly summed %/spot for cost level f, as a month-end-indexed Series."""
    r = ret_at(d, f)
    s = pd.Series(r.values, index=pd.to_datetime(d.exit.values))
    return s.resample("ME").sum()


def load_market():
    spy = pd.read_csv(f"{DATA}/SPY.csv", parse_dates=["Date"]).set_index("Date")
    vix = pd.read_csv(f"{DATA}/VIX.csv", parse_dates=["Date"]).set_index("Date")
    spy_m = spy["Adj Close"].resample("ME").last()
    spy_ret = spy_m.pct_change() * 100                 # % monthly total return
    vix_m = vix["Close"].resample("ME").last()
    vix_chg = vix_m.diff()                             # points
    return spy_ret, vix_m, vix_chg


def sharpe(m):
    return m.mean() / m.std() * np.sqrt(12) if m.std() > 0 else np.nan


def align(strat, other):
    df = pd.concat({"s": strat, "o": other}, axis=1).dropna()
    return df.s, df.o


def stats_row(name, strat, spy_ret, vix_chg):
    s, sp = align(strat, spy_ret)
    beta = np.polyfit(sp, s, 1)[0]
    r_spy = np.corrcoef(s, sp)[0, 1]
    s2, vx = align(strat, vix_chg)
    r_vix = np.corrcoef(s2, vx)[0, 1]
    down = sp < 0
    return dict(book=name, n=len(s), sharpe=sharpe(strat),
                corr_spy=r_spy, beta_spy=beta, corr_vix=r_vix,
                up_mkt=s[~down].mean(), down_mkt=s[down].mean())


# ---------------------------------------------------------------- figures
def scatter_spy(ax, strat, spy_ret, title):
    s, sp = align(strat, spy_ret)
    beta, a = np.polyfit(sp, s, 1)
    r = np.corrcoef(s, sp)[0, 1]
    ax.scatter(sp, s, s=16, color=STRATC, alpha=.55, edgecolor="none")
    xs = np.array([sp.min(), sp.max()])
    ax.plot(xs, a + beta * xs, color=INK, lw=1.4)
    ax.axhline(0, color=MUTED, lw=.6); ax.axvline(0, color=MUTED, lw=.6)
    ax.set_facecolor(BG); ax.grid(color=GRID, lw=.6); ax.set_axisbelow(True)
    for sp_ in ("top", "right"):
        ax.spines[sp_].set_visible(False)
    ax.set_title(title, loc="left", color=INK, fontsize=10)
    ax.set_xlabel("SPY monthly return %", color=MUTED, fontsize=8.5)
    ax.set_ylabel("book monthly %/spot", color=MUTED, fontsize=8.5)
    ax.tick_params(colors=MUTED, labelsize=8)
    ax.text(.03, .97, f"corr {r:+.2f}\nbeta {beta:+.2f}\nSharpe {sharpe(strat):+.2f}",
            transform=ax.transAxes, va="top", ha="left", fontsize=8.5,
            color=INK, bbox=dict(boxstyle="round", fc="white", ec=GRID))


def overlay_vix(ax, strat, vix_m, vix_chg, title):
    cum = strat.cumsum() * 1.0
    ax.plot(cum.index, cum.values, color=STRATC, lw=1.9, label="book cum %/spot (gross)")
    ax.axhline(0, color=INK, lw=.8)
    ax.set_facecolor(BG); ax.grid(color=GRID, lw=.6); ax.set_axisbelow(True)
    for sp_ in ("top", "right"):
        ax.spines[sp_].set_visible(False)
    ax.set_ylabel("cumulative %/spot (sum)", color=STRATC, fontsize=8.5)
    ax.tick_params(axis="y", colors=STRATC, labelsize=8)
    ax.tick_params(axis="x", colors=MUTED, labelsize=8)
    ax2 = ax.twinx()
    # reindex WITHOUT ffill so the VIX line ends at its last real print
    # (Feb-2024) rather than flatlining under the later strategy equity
    vx = vix_m.reindex(cum.index)
    ax2.plot(vx.index, vx.values, color=VIXC, lw=1.2, alpha=.8, label="VIX level")
    ax2.set_ylabel("VIX", color=VIXC, fontsize=8.5)
    ax2.tick_params(axis="y", colors=VIXC, labelsize=8)
    ax2.spines["top"].set_visible(False)
    s2, vc = align(strat, vix_chg)
    r_vix = np.corrcoef(s2, vc)[0, 1]
    ax.set_title(f"{title}   corr(Δreturn, ΔVIX) {r_vix:+.2f}",
                 loc="left", color=INK, fontsize=10)


def rolling_corr_panel(ax, strat, spy_ret, vix_chg, title, win=12):
    """12-month rolling correlation of the book's monthly return vs SPY return
    and vs VIX change, with full-sample means as dashed references."""
    s, sp = align(strat, spy_ret)
    _, vx = align(strat, vix_chg)
    rc_spy = s.rolling(win).corr(sp)
    rc_vix = s.rolling(win).corr(vx)
    ax.plot(rc_spy.index, rc_spy.values, color=SPYC, lw=1.7, label="vs SPY return")
    ax.plot(rc_vix.index, rc_vix.values, color=VIXC, lw=1.7, label="vs VIX change")
    ax.axhline(np.corrcoef(s, sp)[0, 1], color=SPYC, lw=.9, ls="--", alpha=.7)
    ax.axhline(np.corrcoef(s, vx)[0, 1], color=VIXC, lw=.9, ls="--", alpha=.7)
    ax.axhline(0, color=INK, lw=.8)
    ax.set_ylim(-1.05, 1.05)
    ax.set_facecolor(BG); ax.grid(color=GRID, lw=.6); ax.set_axisbelow(True)
    for sp_ in ("top", "right"):
        ax.spines[sp_].set_visible(False)
    ax.set_title(f"{title}   ({win}m rolling)", loc="left", color=INK, fontsize=10)
    ax.set_ylabel("rolling correlation", color=MUTED, fontsize=8.5)
    ax.tick_params(colors=MUTED, labelsize=8)
    ax.legend(frameon=False, fontsize=8, loc="upper left", ncol=2)


def main():
    plt.rcParams.update({"font.size": 9, "figure.facecolor": BG})
    spy_ret, vix_m, vix_chg = load_market()

    d2 = lag2_book()
    book15, _ = build_model_book(load_lag15())
    book15 = book15.copy(); book15["exit"] = pd.to_datetime(book15.exit)

    books = [("lag2 (ungated, 2d pre-event)", d2),
             ("lag15-s4 (4-signal model, OOS)", book15)]

    # ---- printed table: gross AND realistic ------------------------------
    rows = []
    for name, d in books:
        for f, tag in [(0.0, "gross"), (0.5, "realistic")]:
            rows.append({**stats_row(f"{name} [{tag}]",
                                     monthly_strat(d, f), spy_ret, vix_chg)})
    tab = pd.DataFrame(rows)
    pd.set_option("display.width", 160)
    print("\n== monthly market profile (2013-01 .. 2024-01, SPY/VIX truncated) ==")
    print(tab.round(3).to_string(index=False))
    print("\nup_mkt/down_mkt = mean book %/spot in SPY-up vs SPY-down months")

    # ---- GROSS figure ----------------------------------------------------
    fig, ax = plt.subplots(2, 2, figsize=(13.5, 8.5))
    for i, (name, d) in enumerate(books):
        m = monthly_strat(d, 0.0)
        scatter_spy(ax[i, 0], m, spy_ret, f"{name} vs SPY")
        overlay_vix(ax[i, 1], m, vix_m, vix_chg, name)
    fig.suptitle("Pre-earnings straddle — market profile (GROSS, mid): SPY beta & VIX co-movement",
                 fontsize=12.5, color=INK, x=0.006, ha="left")
    fig.tight_layout(rect=[0, 0, 1, 0.96])
    p = f"{R}/figures/23_market_beta.png"
    fig.savefig(p, dpi=115); print(f"\nwrote {p}")

    # ---- COST figure (realistic net), kept separate ----------------------
    fig2, ax2 = plt.subplots(1, 2, figsize=(13.5, 4.6))
    for i, (name, d) in enumerate(books):
        scatter_spy(ax2[i], monthly_strat(d, 0.5), spy_ret,
                    f"{name} vs SPY  [realistic net f=0.5]")
    fig2.suptitle("Same, AFTER costs (realistic f=0.5) — correlation is a return-stream property, "
                  "shown separately",
                  fontsize=11.5, color=INK, x=0.006, ha="left")
    fig2.tight_layout(rect=[0, 0, 1, 0.94])
    p2 = f"{R}/figures/24_market_beta_net.png"
    fig2.savefig(p2, dpi=115); print(f"wrote {p2}")

    # ---- ROLLING correlation figure (GROSS) ------------------------------
    fig3, ax3 = plt.subplots(2, 1, figsize=(13.5, 8), sharex=True)
    for i, (name, d) in enumerate(books):
        rolling_corr_panel(ax3[i], monthly_strat(d, 0.0), spy_ret, vix_chg, name)
    fig3.suptitle("Pre-earnings straddle — 12-month rolling correlation (GROSS): "
                  "vs SPY and vs VIX  (dashed = full-sample mean)",
                  fontsize=12, color=INK, x=0.006, ha="left")
    fig3.tight_layout(rect=[0, 0, 1, 0.96])
    p3 = f"{R}/figures/25_rolling_corr.png"
    fig3.savefig(p3, dpi=115); print(f"wrote {p3}")


if __name__ == "__main__":
    main()
