"""
Test of the four Oakquants relative-value signals on our pre-earnings
straddle trades (GROSS mid, past-only construction throughout).

Signals (all: LOWER = implied move cheap vs own history = better):
  s1 imp_vs_last_imp   = impErnMv(entry) / implied move at PREVIOUS event
  s2 imp_vs_last_rlz   = impErnMv(entry) - |realized move| at PREVIOUS event
  s3 imp_vs_avg_imp    = impErnMv(entry) / expanding mean of past event
                         implied moves (>=4 prior)
  s4 imp_vs_avg_rlz    = impErnMv(entry) - expanding mean of past |realized|
                         (>=4 prior)

Per-event inputs:
  implied  = impErnMv at the last close before the announcement reaction
  realized = |close-to-close move| on the reaction day (AMC -> first tdate
             AFTER ernDate; BMO/UNK -> first tdate >= ernDate), from the
             CORRECTED close series (pxCls is prior close -> shift back).

Outputs: quintile tables + Spearman IC per signal (lag 15 and lag 2),
double-sort vs the existing mv_rk gate, signal correlations.
Run from analysis/:  python3 oq_signals.py
"""
import numpy as np
import pandas as pd
from scipy import stats

from gate import add_mv_rank

R = "ret_spot_mid"


def t(r):
    return r.mean() / r.std() * np.sqrt(len(r))


def build_event_table():
    ev = pd.read_parquet("../cache/events.parquet")
    panel = pd.read_parquet("../cache/ern_panel.parquet",
                            columns=["ticker", "tradeDate", "impErnMv", "pxCls"])
    panel = panel.sort_values(["ticker", "tradeDate"])
    panel["close"] = panel.groupby("ticker").pxCls.shift(-1)
    panel["ret"] = panel.groupby("ticker").close.pct_change(fill_method=None)

    rows = []
    for tkr, sub in panel.groupby("ticker"):
        sub = sub.reset_index(drop=True)
        dates = sub.tradeDate.values
        for e in ev[ev.ticker == tkr].itertuples():
            pos = dates.searchsorted(np.datetime64(e.ernDate))
            react = pos + 1 if e.when == "AMC" else pos   # reaction day index
            if react < 1 or react >= len(sub):
                continue
            rlz = abs(sub.ret.iloc[react]) * 100          # % move
            imp = sub.impErnMv.iloc[react - 1]            # last pre-event close
            rows.append(dict(ticker=tkr, ernDate=e.ernDate,
                             ev_imp=imp, ev_rlz=rlz))
    et = pd.DataFrame(rows).sort_values(["ticker", "ernDate"])
    g = et.groupby("ticker")
    et["last_imp"] = g.ev_imp.shift(1)
    et["last_rlz"] = g.ev_rlz.shift(1)
    et["avg_imp"] = g.ev_imp.transform(
        lambda s: s.expanding(min_periods=4).mean().shift(1))
    et["avg_rlz"] = g.ev_rlz.transform(
        lambda s: s.expanding(min_periods=4).mean().shift(1))
    return et


SIGS = ["s1_vs_last_imp", "s2_vs_last_rlz", "s3_vs_avg_imp", "s4_vs_avg_rlz"]


def add_signals(tr, et):
    d = tr.merge(et, on=["ticker", "ernDate"], how="left")
    m = d.impErnMv  # implied move known AT ENTRY (point-in-time)
    d["s1_vs_last_imp"] = m / d.last_imp.replace(0, np.nan)
    d["s2_vs_last_rlz"] = m - d.last_rlz
    d["s3_vs_avg_imp"] = m / d.avg_imp.replace(0, np.nan)
    d["s4_vs_avg_rlz"] = m - d.avg_rlz
    return d


def quintiles(d, sig):
    s = d.dropna(subset=[sig, R])
    s = s[np.isfinite(s[sig])]
    q = pd.qcut(s[sig].rank(method="first"), 5, labels=False)
    rows = []
    for k in range(5):
        r = s.loc[q == k, R]
        rows.append(f"Q{k+1} {r.mean()*100:+.3f} (t={t(r):+.1f})")
    ic = stats.spearmanr(s[sig], s[R])[0]
    return f"{sig:16s} n={len(s):5d}  IC={ic:+.4f}  " + "  ".join(rows)


def main():
    et = build_event_table()
    print(f"event table: {len(et)} events with implied+realized; "
          f"median ev_imp {et.ev_imp.median():.1f}%, "
          f"median ev_rlz {et.ev_rlz.median():.1f}%  "
          f"(imp/rlz median ratio "
          f"{(et.ev_imp/et.ev_rlz.replace(0,np.nan)).median():.2f})")

    tr = pd.read_parquet("../cache/trades_prern.parquet")
    ls = pd.read_parquet("../cache/trades_prern_lagscan.parquet")
    for lab, book in [("LAG 15", tr[tr.lag == 15]),
                      ("LAG 2", ls[ls.lag == 2])]:
        d = add_signals(book.dropna(subset=["impErnMv"]).copy(), et)
        print(f"\n== {lab}: quintiles (Q1 = cheapest vs history) ==")
        for sig in SIGS:
            print(quintiles(d, sig))

    # does anything add to the existing mv_rk gate? (lag 15)
    d = add_signals(add_mv_rank(tr[tr.lag == 15]).dropna(subset=["mv_rk"]), et)
    print("\n== lag 15: signal correlations (spearman, incl. existing mv_rk) ==")
    cols = ["mv_rk"] + SIGS
    print(d[cols].corr(method="spearman").round(2).to_string())

    print("\n== lag 15: double-sort — within mv_rk cheap-half, split by s4 ==")
    h = d[d.mv_rk < 0.5].dropna(subset=["s4_vs_avg_rlz"])
    med = h.s4_vs_avg_rlz.median()
    for lab, s in [("cheap-half & s4 low (cheap vs realized)",
                    h[h.s4_vs_avg_rlz <= med]),
                   ("cheap-half & s4 high", h[h.s4_vs_avg_rlz > med])]:
        r = s[R]
        print(f"{lab:38s} n={len(s):4d}  {r.mean()*100:+.3f}%  t={t(r):+.2f}")
    print("\n== and the reverse: within s4 cheap-half, does mv_rk still split? ==")
    h = d.dropna(subset=["s4_vs_avg_rlz"])
    h = h[h.s4_vs_avg_rlz <= h.s4_vs_avg_rlz.median()]
    for lab, s in [("s4-cheap & mv_rk<0.5", h[h.mv_rk < 0.5]),
                   ("s4-cheap & mv_rk>=0.5", h[h.mv_rk >= 0.5])]:
        r = s[R]
        print(f"{lab:38s} n={len(s):4d}  {r.mean()*100:+.3f}%  t={t(r):+.2f}")


if __name__ == "__main__":
    main()
