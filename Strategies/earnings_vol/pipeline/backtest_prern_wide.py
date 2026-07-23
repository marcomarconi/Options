"""
WIDE pre-earnings ATM straddle backtest — permitted-data retest.

Identical trade logic to backtest_prern.py (buy ATM straddle on the first expiry
AFTER the report ~LAG days before the event; sell at the last close strictly
before the announcement; never hold the jump), but priced from the FULL
_data/strikes archive (4,427 names) instead of the ff_calendar_v2 top-200
smv_single cache. Parallel over tickers.

Marks are MID (gross); entry-ask / exit-bid recorded for the cost footnote.
Filters unchanged: spot >= $5, debit >= $0.30, ATM strike within 10% of spot,
both legs quoted at entry & exit.

Run from pipeline/ AFTER build_events_wide.py:  python3 backtest_prern_wide.py
Writes cache/trades_prern_wide.parquet (one row per event x LAG).
"""
import os
from multiprocessing import Pool

import numpy as np
import pandas as pd
import pyarrow.dataset as pds

STRIKES = os.path.expanduser("~/trading/Systems/Options/Strategies/_data/strikes")
CACHE = "../cache"
LAGS = [20, 15, 10, 5]
MIN_PX = 5.0
MIN_DEBIT = 0.30
MAX_ATM_DIST = 0.10
COLS = ["tdate", "expir", "strike", "stk", "cBid", "cAsk", "pBid", "pAsk"]


def leg_marks(day, expir, strike):
    r = day[(day.expir == expir) & (day.strike == strike)]
    if len(r) != 1:
        return None
    r = r.iloc[0]
    if not (r.cAsk > 0 and r.pAsk > 0 and r.cBid >= 0 and r.pBid >= 0
            and r.cAsk >= r.cBid and r.pAsk >= r.pBid):
        return None
    return dict(mid=(r.cBid + r.cAsk) / 2 + (r.pBid + r.pAsk) / 2,
                ask=r.cAsk + r.pAsk, bid=r.cBid + r.pBid, spot=r.stk)


def _one(args):
    tkr, ev = args
    try:
        smv = pds.dataset(f"{STRIKES}/ticker={tkr}", format="parquet").to_table(
            columns=COLS).to_pandas()
    except Exception:
        return []
    if smv.empty:
        return []
    smv["tdate"] = pd.to_datetime(smv.tdate)
    smv["expir"] = pd.to_datetime(smv.expir)
    tdates = np.sort(smv.tdate.unique())
    out = []
    for e in ev.itertuples():
        cutoff = e.ernDate + pd.Timedelta(days=1) if e.when == "AMC" else e.ernDate
        xs = tdates[tdates < cutoff]
        if not len(xs):
            continue
        exit_d = xs[-1]
        jump_span = e.ernDate if e.when == "BMO" else e.ernDate + pd.Timedelta(days=1)
        for lag in LAGS:
            ens = tdates[tdates <= e.ernDate - pd.Timedelta(days=lag)]
            if not len(ens):
                continue
            entry_d = ens[-1]
            if entry_d >= exit_d:
                continue
            day_in = smv[smv.tdate == entry_d]
            if day_in.empty:
                continue
            spot = day_in.stk.iloc[0]
            if not spot >= MIN_PX:
                continue
            exps = np.sort(day_in.expir[day_in.expir >= jump_span].unique())
            if not len(exps):
                continue
            expir = exps[0]
            ch = day_in[day_in.expir == expir]
            strike = ch.strike.iloc[(ch.strike - spot).abs().values.argmin()]
            if abs(strike - spot) / spot > MAX_ATM_DIST:
                continue
            m_in = leg_marks(day_in, expir, strike)
            m_out = leg_marks(smv[smv.tdate == exit_d], expir, strike)
            if m_in is None or m_out is None or m_in["mid"] < MIN_DEBIT:
                continue
            out.append(dict(
                ticker=tkr, ernDate=e.ernDate, when=e.when, lag=lag,
                entry=entry_d, exit=exit_d, expir=expir, strike=strike,
                spot=spot, held=int((exit_d - entry_d) / np.timedelta64(1, "D")),
                debit_mid=m_in["mid"], exit_mid=m_out["mid"],
                debit_ask=m_in["ask"], exit_bid=m_out["bid"],
                ret_spot_mid=(m_out["mid"] - m_in["mid"]) / spot,
                ret_debit_mid=(m_out["mid"] - m_in["mid"]) / m_in["mid"],
                ret_spot_taker=(m_out["bid"] - m_in["ask"]) / spot,
                spot_ret=(m_out["spot"] - spot) / spot))
    return out


def main():
    ev = pd.read_parquet(f"{CACHE}/events_wide.parquet")
    jobs = [(tkr, sub) for tkr, sub in ev.groupby("ticker")]
    print(f"{len(jobs)} tickers, {len(ev)} events -> pricing from _data/strikes", flush=True)
    rows = []
    with Pool(10) as p:
        for i, part in enumerate(p.imap_unordered(_one, jobs, chunksize=8)):
            rows += part
            if i % 200 == 0:
                print(f"  {i}/{len(jobs)} tickers, cum trades {len(rows)}", flush=True)
    tr = pd.DataFrame(rows)

    # point-in-time signals at entry: implied earnings move + liquidity (for gating)
    panel = pd.read_parquet(f"{CACHE}/ern_panel_wide.parquet",
                            columns=["ticker", "tradeDate", "impErnMv", "avgOptVolu20d"])
    tr = tr.merge(panel.rename(columns={"tradeDate": "entry"}),
                  on=["ticker", "entry"], how="left")
    tr.to_parquet(f"{CACHE}/trades_prern_wide.parquet")
    print(f"\n{len(tr):,} trades, {tr.ticker.nunique():,} tickers, "
          f"{tr.ernDate.dt.year.min()}..{tr.ernDate.dt.year.max()}")
    for lag, s in tr.groupby("lag"):
        r = s.ret_spot_mid
        print(f"lag {lag:2d}: n={len(s):6d}  gross {r.mean()*100:+.3f}%/spot  "
              f"t={r.mean()/r.std()*np.sqrt(len(r)):+.2f}  win {(r>0).mean()*100:.0f}%")


if __name__ == "__main__":
    main()
