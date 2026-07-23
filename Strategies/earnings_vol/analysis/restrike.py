"""
Restrike test for the lag-15 book: when spot drifts >= theta from the current
strike mid-hold, is it better to (a) HOLD the now-ITM straddle to the normal
pre-event exit, (b) RESTRIKE — close at mid, reopen the nearest ATM straddle
on the SAME expiry (two sizings: reinvest all proceeds / bank the excess over
the original premium), or (c) CLOSE early and stop?

Marks: same smv mid conventions as pipeline/backtest_prern.py (gross, house
rule). Multiple restrikes allowed; if the chain can't mark the current strike
on the exit day, the position is valued at its last markable day (counted).
All pnl in units of the original debit (equal-premium accounting).
Run from analysis/:  python3 restrike.py
"""
import os

import numpy as np
import pandas as pd

SMV = os.path.expanduser(
    "~/trading/Systems/Options/Strategies/ff_calendar_v2/cache/smv_single")
THETAS = [0.05, 0.075, 0.10, 0.15]


def marks(ch, strike):
    r = ch[ch.strike == strike]
    if len(r) != 1:
        return None
    r = r.iloc[0]
    if not (r.cAsk > 0 and r.pAsk > 0 and r.cBid >= 0 and r.pBid >= 0
            and r.cAsk >= r.cBid and r.pAsk >= r.pBid):
        return None
    return (r.cBid + r.cAsk) / 2 + (r.pBid + r.pAsk) / 2


def simulate(trade, by_day, days, theta):
    """Returns dict of pnl per 1 unit original premium for each arm."""
    K = trade.strike
    n = 1.0 / trade.debit_mid          # contracts per unit premium (both arms)
    n_bank, cash_bank = n, 0.0
    closed_early = None                # pnl of arm (c), first trigger only
    restrikes, last_val_full, last_val_bank = 0, None, None
    for d in days[:-1]:
        ch = by_day.get(d)
        if ch is None:
            continue
        ch = ch[ch.expir == trade.expir]
        if ch.empty:
            continue
        spot = ch.stk.iloc[0]
        if abs(spot / K - 1) < theta:
            continue
        m_old = marks(ch, K)
        if m_old is None:
            continue
        if closed_early is None:
            closed_early = n * m_old - 1.0
        k2 = ch.strike.iloc[(ch.strike - spot).abs().values.argmin()]
        if k2 == K:
            continue
        m_new = marks(ch, k2)
        if m_new is None or m_new <= 0:
            continue
        val = n * m_old
        n = val / m_new
        val_b = n_bank * m_old
        spend = min(val_b, 1.0)        # bank profit above original premium
        cash_bank += val_b - spend
        n_bank = spend / m_new
        K = k2
        restrikes += 1
        last_val_full, last_val_bank = val, val_b
    # exit at the pre-event close
    ch = by_day.get(days[-1])
    m_exit = None
    if ch is not None:
        m_exit = marks(ch[ch.expir == trade.expir], K)
    if m_exit is None:                 # strike no longer marked at exit
        if restrikes == 0:
            m_exit = trade.exit_mid    # original strike always has exit marks
        else:                          # value at last markable point
            return dict(hold=trade.ret_debit_mid,
                        rs_full=last_val_full - 1.0,
                        rs_bank=last_val_bank + cash_bank - 1.0,
                        close=closed_early, restrikes=restrikes, unmarked=1)
    return dict(hold=trade.ret_debit_mid,
                rs_full=n * m_exit - 1.0,
                rs_bank=n_bank * m_exit + cash_bank - 1.0,
                close=closed_early if closed_early is not None
                else n * m_exit - 1.0,
                restrikes=restrikes, unmarked=0)


def monthly_sharpe(d, col):
    m = (d.set_index("exit")[col]
          .resample("ME").sum()
          .reindex(pd.date_range("2013-01-31", "2024-12-31", freq="ME"),
                   fill_value=0.0))
    return m.mean() / m.std() * np.sqrt(12)


def main():
    tr = pd.read_parquet("../cache/trades_prern.parquet")
    tr = pd.concat([tr[tr.lag == 15],
                    pd.read_parquet("../cache/trades_prern_lagscan.parquet")
                      .query("lag == 2")])
    print(f"{len(tr)} trades (lag 15 + lag 2)")
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
            for th in THETAS:
                r = simulate(t, by_day, days, th)
                r.update(ticker=tkr, exit=t.exit, lag=t.lag, theta=th)
                rows.append(r)
        if i % 40 == 0:
            print(f"  {i:3d} {tkr}", flush=True)
    res = pd.DataFrame(rows)
    res.to_parquet("../cache/restrike_results.parquet")

    for lag in (15, 2):
        for th in THETAS:
            d = res[(res.lag == lag) & (res.theta == th)]
            trig = d[d.restrikes > 0]
            print(f"\n=== lag {lag}  theta {th*100:.1f}%  triggered "
                  f"{len(trig)}/{len(d)} ({len(trig)/len(d)*100:.0f}%)  "
                  f"avg restrikes {trig.restrikes.mean():.2f}  "
                  f"unmarked exits {d.unmarked.sum()}")
            for col, lab in [("hold", "HOLD to exit"),
                             ("rs_bank", "RESTRIKE bank-excess"),
                             ("rs_full", "RESTRIKE reinvest-all"),
                             ("close", "CLOSE early, stop")]:
                for scope, s in [("all", d[col]), ("triggered", trig[col])]:
                    s = s.dropna()
                    if not len(s):
                        continue
                    t_ = s.mean() / s.std() * np.sqrt(len(s))
                    msh = (monthly_sharpe(d.dropna(subset=[col]), col)
                           if scope == "all" else np.nan)
                    print(f"  {lab:22s} [{scope:9s}] mean {s.mean()*100:+7.2f}%"
                          f"  std {s.std()*100:6.1f}%  t {t_:+5.1f}  "
                          f"win {(s>0).mean()*100:.0f}%"
                          + (f"  moSharpe {msh:.2f}" if scope == "all" else ""))


if __name__ == "__main__":
    main()
