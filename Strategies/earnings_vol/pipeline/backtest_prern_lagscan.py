"""
Fine entry-lag scan for the LONG pre-earnings straddle: same trade logic as
backtest_prern.py but LAGS = 2..30 step 2, to map mean return vs entry
distance (exit is fixed at the pre-announcement close, so the lag profile IS
the cumulative pnl profile of the position over its life).

Run from pipeline/:  python3 backtest_prern_lagscan.py
Writes cache/trades_prern_lagscan.parquet.
"""
import numpy as np
import pandas as pd

import backtest_prern as bp

bp.LAGS = list(range(2, 31, 2))


def main():
    ev = pd.read_parquet(f"{bp.CACHE}/events.parquet")
    rows = []
    for i, (tkr, sub) in enumerate(ev.groupby("ticker")):
        rows += bp.run_ticker(tkr, sub)
        if i % 25 == 0:
            print(f"{i:4d} {tkr:6s} cum trades {len(rows)}", flush=True)
    tr = pd.DataFrame(rows)
    panel = pd.read_parquet(f"{bp.CACHE}/ern_panel.parquet",
                            columns=["ticker", "tradeDate", "impErnMv"])
    tr = tr.merge(panel.rename(columns={"tradeDate": "entry"}),
                  on=["ticker", "entry"], how="left")
    tr.to_parquet(f"{bp.CACHE}/trades_prern_lagscan.parquet")
    print(f"\n{len(tr)} trades, {tr.ticker.nunique()} tickers")
    for lag, s in tr.groupby("lag"):
        r = s.ret_spot_mid
        print(f"lag {lag:2d}: n={len(s):5d}  gross {r.mean()*100:+.3f}%/spot  "
              f"t={r.mean()/r.std()*np.sqrt(len(r)):+.2f}")


if __name__ == "__main__":
    main()
