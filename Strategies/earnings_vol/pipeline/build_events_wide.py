"""
WIDE earnings-event calendar — the permitted-data retest universe.

Identical event logic to build_events.py (reconstruct from lastErn / lastErnTod
in raw ORATS core), but the universe is the FULL _data/strikes archive (4,427
single names) instead of the ff_calendar_v2 top-200. Earnings studies are event-
count-bound, so widening the universe is the whole point of the retest (191 ->
thousands of names). Carries avgOptVolu20d so the backtest can gate on liquidity
rather than pre-selecting a top-200 list.

Writes cache/ern_panel_wide.parquet, cache/events_wide.parquet.
Run from pipeline/:  python3 build_events_wide.py
"""
import glob
import os
from multiprocessing import Pool

import numpy as np
import pandas as pd

CORE_DIR = os.path.expanduser("~/trading/HistoricalData/ORATS/core")
STRIKES = os.path.expanduser("~/trading/Systems/Options/Strategies/_data/strikes")
OUT = "../cache"
USECOLS = ["ticker", "tradeDate", "pxCls", "lastErn", "lastErnTod",
           "impErnMv", "impliedEarningsMove", "absAvgErnMv", "avgOptVolu20d"]
YEARS = range(2013, 2025)          # _data/strikes coverage (2013-01..2024-12)

# universe = every ticker with a strikes partition (only those are priceable)
TICKERS = {os.path.basename(p)[len("ticker="):]
           for p in glob.glob(f"{STRIKES}/ticker=*")}


def one_file(path):
    try:
        df = pd.read_csv(path, usecols=lambda c: c in USECOLS)
    except Exception as e:
        print(f"SKIP {os.path.basename(path)}: {e}")
        return None
    df = df[df.ticker.isin(TICKERS)]
    return df if len(df) else None


def main():
    if os.path.exists(f"{OUT}/ern_panel_wide.parquet"):
        print("reusing cached ern_panel_wide.parquet")
        panel = pd.read_parquet(f"{OUT}/ern_panel_wide.parquet")
    else:
        files = sorted(f for f in glob.glob(f"{CORE_DIR}/orats_core_*.csv.gz")
                       if int(os.path.basename(f)[11:15]) in YEARS)
        print(f"{len(files)} core files, {len(TICKERS)} strikes-archive tickers", flush=True)
        with Pool(10) as p:
            parts = [d for d in p.map(one_file, files, chunksize=20) if d is not None]
        panel = pd.concat(parts, ignore_index=True)
        panel["tradeDate"] = pd.to_datetime(panel.tradeDate)
        panel["lastErn"] = pd.to_datetime(panel.lastErn, errors="coerce")
        for c in ["pxCls", "lastErnTod", "impErnMv", "impliedEarningsMove",
                  "absAvgErnMv", "avgOptVolu20d"]:
            panel[c] = pd.to_numeric(panel[c], errors="coerce")
        panel = panel.sort_values(["ticker", "tradeDate"]).reset_index(drop=True)
        panel.to_parquet(f"{OUT}/ern_panel_wide.parquet")
    print(f"panel: {panel.shape}, {panel.tradeDate.min().date()}..{panel.tradeDate.max().date()}, "
          f"{panel.ticker.nunique()} tickers", flush=True)

    ev = (panel.dropna(subset=["lastErn"])
                .groupby(["ticker", "lastErn"], as_index=False)
                .agg(firstSeen=("tradeDate", "min"),
                     tod=("lastErnTod", "first")))
    ev = ev.rename(columns={"lastErn": "ernDate"})
    tod = pd.to_numeric(ev.tod, errors="coerce")        # CODE: 2=BMO, 3=AMC
    ev["when"] = np.select([tod == 2, tod == 3], ["BMO", "AMC"], default="UNK")
    ev = ev[ev.ernDate >= "2012-06-01"]
    ev.to_parquet(f"{OUT}/events_wide.parquet")
    print(f"events: {len(ev)} rows, {ev.ticker.nunique()} tickers")
    print(ev.when.value_counts().to_string())
    print(ev.groupby(ev.ernDate.dt.year).size().to_string())


if __name__ == "__main__":
    main()
