"""
LONG pre-earnings ATM straddle backtest (the "IV run-up" harvest).

Trade: buy the ATM straddle on the first expiry AFTER the earnings report,
~LAG calendar days before the event; sell at the last close strictly BEFORE
the announcement (BMO/MID/UNK -> close of the day before the report date;
AMC -> close of the report date itself). Never holds through the jump.

Prices: real NBBO from ff_calendar_v2 smv_single strike cache (top-200 liquid
single names, 2013-2024, band +/-40% spot, DTE<=70). Marks are MID (gross,
house preference); entry-at-ask / exit-at-bid also recorded for the cost
footnote. Filters per house lessons: spot >= $5, debit >= $0.30, ATM strike
within 10% of spot, quotes present on both legs at entry & exit.

Run from pipeline/ AFTER build_events.py:  python3 backtest_prern.py
Writes cache/trades_prern.parquet (one row per event x LAG).
"""
import os

import numpy as np
import pandas as pd

SMV = os.path.expanduser(
    "~/trading/Systems/Options/Strategies/ff_calendar_v2/cache/smv_single")
CACHE = "../cache"
LAGS = [20, 15, 10, 5]          # calendar days before the event
MIN_PX = 5.0
MIN_DEBIT = 0.30
MAX_ATM_DIST = 0.10             # |strike-spot|/spot at entry


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


def run_ticker(tkr, ev):
    path = f"{SMV}/{tkr}/part-0.parquet"
    if not os.path.exists(path):
        return []
    smv = pd.read_parquet(path)
    smv["tdate"] = pd.to_datetime(smv.tdate)
    smv["expir"] = pd.to_datetime(smv.expir)
    tdates = np.sort(smv.tdate.unique())
    out = []
    for e in ev.itertuples():
        # last close strictly before the jump
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
            # first expiry spanning the jump
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
    ev = pd.read_parquet(f"{CACHE}/events.parquet")
    rows = []
    for i, (tkr, sub) in enumerate(ev.groupby("ticker")):
        rows += run_ticker(tkr, sub)
        if i % 25 == 0:
            print(f"{i:4d} {tkr:6s} cum trades {len(rows)}", flush=True)
    tr = pd.DataFrame(rows)
    # point-in-time implied earnings move at entry (rich/cheap gate signal);
    # merge, not per-row dict lookup — Timestamp/datetime64 keys don't hash equal
    panel = pd.read_parquet(f"{CACHE}/ern_panel.parquet",
                            columns=["ticker", "tradeDate", "impErnMv"])
    tr = tr.merge(panel.rename(columns={"tradeDate": "entry"}),
                  on=["ticker", "entry"], how="left")
    tr.to_parquet(f"{CACHE}/trades_prern.parquet")
    print(f"\n{len(tr)} trades, {tr.ticker.nunique()} tickers")
    for lag, s in tr.groupby("lag"):
        r = s.ret_spot_mid
        print(f"lag {lag:2d}: n={len(s):5d}  gross {r.mean()*100:+.3f}%/spot  "
              f"t={r.mean()/r.std()*np.sqrt(len(r)):+.2f}  win {(r>0).mean()*100:.0f}%")


if __name__ == "__main__":
    main()
