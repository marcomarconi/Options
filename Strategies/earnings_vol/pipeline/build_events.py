"""
Build the single-name earnings-event calendar from raw ORATS core day files.

nextErn is NEVER populated in historical cores (checked 2014/2019/2022/2024: 0%),
so the calendar is reconstructed from lastErn: every event shows up there on the
first trade date after it happens, with lastErnTod = announce time (HHMM; >=1600
or <0930 -> AMC/BMO). Point-in-time caveat: backtests entering N days before a
reconstructed date assume the schedule was known in advance (true in practice —
companies pre-announce reporting dates weeks ahead — but ORATS doesn't store the
forward date, so an unscheduled/moved report is a small lookahead risk).

Also keeps a daily point-in-time panel of impErnMv / impliedEarningsMove /
absAvgErnMv for the top-200 names (entry-day signal for rich/cheap gating).

Universe: ff_calendar_v2 top-200 single names (same smv_single strike cache).
Writes: cache/ern_panel.parquet (daily), cache/events.parquet (one row/event).
Run from pipeline/:  python3 build_events.py        (~5 min, 8 procs)
"""
import glob
import os
from multiprocessing import Pool

import numpy as np
import pandas as pd

CORE_DIR = os.path.expanduser("~/trading/HistoricalData/ORATS/core")
TOP200 = os.path.expanduser(
    "~/trading/Systems/Options/Strategies/ff_calendar_v2/out/top200_single_names_clean.csv")
OUT = "../cache"
USECOLS = ["ticker", "tradeDate", "pxCls", "lastErn", "lastErnTod",
           "impErnMv", "impliedEarningsMove", "absAvgErnMv"]
YEARS = range(2013, 2025)          # smv_single coverage

TICKERS = set(pd.read_csv(TOP200).ticker)


def one_file(path):
    try:
        df = pd.read_csv(path, usecols=lambda c: c in USECOLS)
    except Exception as e:
        print(f"SKIP {os.path.basename(path)}: {e}")
        return None
    df = df[df.ticker.isin(TICKERS)]
    return df if len(df) else None


def main():
    if os.path.exists(f"{OUT}/ern_panel.parquet"):
        print("reusing cached ern_panel.parquet")
        panel = pd.read_parquet(f"{OUT}/ern_panel.parquet")
    else:
        files = sorted(f for f in glob.glob(f"{CORE_DIR}/orats_core_*.csv.gz")
                       if int(os.path.basename(f)[11:15]) in YEARS)
        print(f"{len(files)} core files, {len(TICKERS)} tickers")
        with Pool(8) as p:
            parts = [d for d in p.map(one_file, files, chunksize=20) if d is not None]
        panel = pd.concat(parts, ignore_index=True)
        panel["tradeDate"] = pd.to_datetime(panel.tradeDate)
        panel["lastErn"] = pd.to_datetime(panel.lastErn, errors="coerce")
        for c in ["pxCls", "lastErnTod", "impErnMv", "impliedEarningsMove", "absAvgErnMv"]:
            panel[c] = pd.to_numeric(panel[c], errors="coerce")
        panel = panel.sort_values(["ticker", "tradeDate"]).reset_index(drop=True)
        panel.to_parquet(f"{OUT}/ern_panel.parquet")
    print(f"panel: {panel.shape}, {panel.tradeDate.min().date()}..{panel.tradeDate.max().date()}")

    # events: first sighting of each (ticker, lastErn)
    ev = (panel.dropna(subset=["lastErn"])
                .groupby(["ticker", "lastErn"], as_index=False)
                .agg(firstSeen=("tradeDate", "min"),
                     tod=("lastErnTod", "first")))
    ev = ev.rename(columns={"lastErn": "ernDate"})
    # ORATS ernTod is a CODE, not HHMM: 2=before open, 3=after close (verified:
    # JPM/WMT/KO/GS all 2, AAPL/MSFT/NFLX/AMZN all 3); 900/1/4 = unknown/odd.
    tod = pd.to_numeric(ev.tod, errors="coerce")
    ev["when"] = np.select([tod == 2, tod == 3], ["BMO", "AMC"], default="UNK")
    ev = ev[ev.ernDate >= "2012-06-01"]     # drop pre-sample tail seen in early files
    ev.to_parquet(f"{OUT}/events.parquet")
    print(f"events: {len(ev)} rows, {ev.ticker.nunique()} tickers")
    print(ev.when.value_counts().to_string())
    print(ev.groupby(ev.ernDate.dt.year).size().to_string())


if __name__ == "__main__":
    main()
