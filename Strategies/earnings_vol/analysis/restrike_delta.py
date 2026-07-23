"""
Vol-normalized restrike triggers for the lag-15 book (follow-up to
restrike.py, which found the fixed |spot/K-1| >= 5% rule beats holding).

Trigger tested here: straddle NET DELTA |2*Phi(d1)-1| >= D, with the
remaining sig*sqrt(T) backed out of the CURRENT ATM straddle mid
(sig*sqrt(T) ~ atm_mid / (0.8*spot)) — no IV solver needed, and the
threshold self-adjusts for each name's vol and the shrinking calendar.
A 5% fixed-move rerun is included as the baseline. Bank-excess sizing only
(the winner in restrike.py). Also prints trigger rate by entry-vol quintile
to show what the normalization changes.
Run from analysis/:  python3 restrike_delta.py
"""
import os

import numpy as np
import pandas as pd
from scipy.stats import norm

SMV = os.path.expanduser(
    "~/trading/Systems/Options/Strategies/ff_calendar_v2/cache/smv_single")
DELTAS = [0.20, 0.30, 0.40, 0.50]
PCT_BASE = 0.05


def marks(ch, strike):
    r = ch[ch.strike == strike]
    if len(r) != 1:
        return None
    r = r.iloc[0]
    if not (r.cAsk > 0 and r.pAsk > 0 and r.cBid >= 0 and r.pBid >= 0
            and r.cAsk >= r.cBid and r.pAsk >= r.pBid):
        return None
    return (r.cBid + r.cAsk) / 2 + (r.pBid + r.pAsk) / 2


def net_delta(spot, K, atm_mid):
    sv = atm_mid / (0.8 * spot)          # sig*sqrt(T) of remaining life
    if not sv > 1e-4:
        return 1.0
    d1 = (np.log(spot / K) + sv * sv / 2) / sv
    return 2 * norm.cdf(d1) - 1


def simulate(trade, by_day, days, kind, thresh):
    K = trade.strike
    n, cash = 1.0 / trade.debit_mid, 0.0
    restrikes = 0
    last_val = None
    for d in days[:-1]:
        ch = by_day.get(d)
        if ch is None:
            continue
        ch = ch[ch.expir == trade.expir]
        if ch.empty:
            continue
        spot = ch.stk.iloc[0]
        k_atm = ch.strike.iloc[(ch.strike - spot).abs().values.argmin()]
        if kind == "pct":
            trig = abs(spot / K - 1) >= thresh
        else:
            m_atm = marks(ch, k_atm)
            if m_atm is None:
                continue
            trig = abs(net_delta(spot, K, m_atm)) >= thresh
        if not trig or k_atm == K:
            continue
        m_old, m_new = marks(ch, K), marks(ch, k_atm)
        if m_old is None or m_new is None or m_new <= 0:
            continue
        val = n * m_old
        spend = min(val, 1.0)
        cash += val - spend
        n = spend / m_new
        K = k_atm
        restrikes += 1
        last_val = val
    ch = by_day.get(days[-1])
    m_exit = None
    if ch is not None:
        m_exit = marks(ch[ch.expir == trade.expir], K)
    if m_exit is None:
        if restrikes == 0:
            m_exit = trade.exit_mid
        else:
            return dict(pnl=last_val + cash - 1.0, restrikes=restrikes)
    return dict(pnl=n * m_exit + cash - 1.0, restrikes=restrikes)


def main():
    tr = pd.read_parquet("../cache/trades_prern.parquet")
    tr = tr[tr.lag == 15].copy()
    tr["relvol"] = tr.debit_mid / tr.spot
    arms = [("pct", PCT_BASE)] + [("delta", d) for d in DELTAS]
    rows = []
    for i, (tkr, sub) in enumerate(tr.groupby("ticker")):
        path = f"{SMV}/{tkr}/part-0.parquet"
        if not os.path.exists(path):
            continue
        smv = pd.read_parquet(path)
        smv["tdate"] = pd.to_datetime(smv.tdate)
        smv["expir"] = pd.to_datetime(smv.expir)
        smv = smv[smv.expir.isin(sub.expir.unique())]
        by_day = dict(tuple(smv.groupby("tdate")))
        tdates = np.array(sorted(by_day))
        for t in sub.itertuples():
            days = tdates[(tdates > t.entry) & (tdates <= t.exit)]
            if not len(days):
                continue
            for kind, th in arms:
                r = simulate(t, by_day, days, kind, th)
                r.update(kind=kind, thresh=th, hold=t.ret_debit_mid,
                         exit=t.exit, relvol=t.relvol)
                rows.append(r)
        if i % 40 == 0:
            print(f"  {i:3d} {tkr}", flush=True)
    res = pd.DataFrame(rows)
    res.to_parquet("../cache/restrike_delta_results.parquet")

    d0 = res[(res.kind == "pct")]
    h = d0.hold
    m = (d0.set_index("exit").hold.resample("ME").sum()
           .reindex(pd.date_range("2013-01-31", "2024-12-31", freq="ME"),
                    fill_value=0.0))
    print(f"\nHOLD baseline: mean {h.mean()*100:+.2f}%  std {h.std()*100:.1f}%"
          f"  moSharpe {m.mean()/m.std()*np.sqrt(12):.2f}")
    for kind, th in arms:
        d = res[(res.kind == kind) & (res.thresh == th)]
        s = d.pnl
        mm = (d.set_index("exit").pnl.resample("ME").sum()
                .reindex(pd.date_range("2013-01-31", "2024-12-31", freq="ME"),
                         fill_value=0.0))
        trig = d[d.restrikes > 0]
        lab = f"{kind} {th:.2f}"
        print(f"{lab:12s} trig {len(trig)/len(d)*100:3.0f}% "
              f"(avg {trig.restrikes.mean():.2f})  mean {s.mean()*100:+.2f}%  "
              f"std {s.std()*100:.1f}%  t {s.mean()/s.std()*np.sqrt(len(s)):+.1f}"
              f"  moSharpe {mm.mean()/mm.std()*np.sqrt(12):.2f}")
    print("\ntrigger rate by entry-vol quintile (low->high relvol):")
    for kind, th in [("pct", PCT_BASE), ("delta", 0.30), ("delta", 0.40)]:
        d = res[(res.kind == kind) & (res.thresh == th)].copy()
        q = pd.qcut(d.relvol, 5, labels=False)
        rates = d.groupby(q).apply(lambda g: (g.restrikes > 0).mean())
        print(f"  {kind} {th:.2f}: " +
              "  ".join(f"{r*100:.0f}%" for r in rates))


if __name__ == "__main__":
    main()
