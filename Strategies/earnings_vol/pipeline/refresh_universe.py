"""
Refresh the live screening universe from recent local ORATS core files.

The historical backtest universe (ff_calendar_v2 top200_single_names_clean.csv,
ranked 2013-2024) has decayed: 38 of its 200 tickers no longer exist (renames,
acquisitions, bankruptcies), so the live screener was down to 162 names.
This script rebuilds a CURRENT top-200:

 1. scan the last SCAN_DAYS local core day files (one row per ticker);
    per ticker: median daily option contract volume (cVolu+pVolu), median
    close, coverage;
 2. keep single names that actually REPORT EARNINGS — lastErn within
    ERN_MAX_AGE days of the scan end (this alone excludes every ETF, index
    and non-reporting product, incl. single-stock ETFs like NVDL/TSLL);
    plus cover >= MIN_COVER and med_px >= MIN_PX (screener's price floor);
 3. rank by med_vol, take the top 200 -> out/top200_earnings_live.csv
    (the historical csv is left untouched — the backtest record stays as-is);
 4. backfill cache/ern_panel_live.parquet with FULL history (2013->today)
    for tickers new to the universe, so the four signals get their
    expanding past-only aggregates from day one (>=4 prior events needed
    for the lag-15 model book).

Run from pipeline/:  python3 refresh_universe.py
Rerun quarterly-ish; screener.py reads the csv this script writes.
"""
import glob
import os
from multiprocessing import Pool

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
BASE = os.path.dirname(HERE)
CACHE = f"{BASE}/cache"
CORE_DIR = os.path.expanduser("~/trading/HistoricalData/ORATS/core")
OLD_CSV = os.path.expanduser(
    "~/trading/Systems/Options/Strategies/ff_calendar_v2/out/"
    "top200_single_names_clean.csv")
NEW_CSV = f"{BASE}/out/top200_earnings_live.csv"
PANEL_COLS = ["ticker", "tradeDate", "pxCls", "lastErn", "lastErnTod",
              "impErnMv", "impliedEarningsMove", "absAvgErnMv"]

SCAN_DAYS = 126        # ~6 months of trading days
ERN_MAX_AGE = 200      # lastErn must be within this many days of scan end
MIN_COVER = 0.90
MIN_PX = 5.0
TOP_N = 200
BACKFILL_FROM = "2013-01-01"


def _scan_one(path):
    try:
        d = pd.read_csv(path, usecols=["ticker", "cVolu", "pVolu",
                                       "pxCls", "lastErn"])
    except Exception as e:
        print(f"SKIP {os.path.basename(path)}: {e}")
        return None
    d["vol"] = d.cVolu.fillna(0) + d.pVolu.fillna(0)
    d["date"] = os.path.basename(path)[11:19]
    return d[["ticker", "vol", "pxCls", "lastErn", "date"]]


def _panel_one(path):
    try:
        d = pd.read_csv(path, usecols=lambda c: c in PANEL_COLS)
    except Exception as e:
        print(f"SKIP {os.path.basename(path)}: {e}")
        return None
    d = d[d.ticker.isin(_panel_one.tickers)]
    return d if len(d) else None


def main():
    files = sorted(glob.glob(f"{CORE_DIR}/orats_core_*.csv.gz"))
    scan = files[-SCAN_DAYS:]
    end = pd.Timestamp(os.path.basename(scan[-1])[11:19])
    print(f"scanning {len(scan)} core files "
          f"({os.path.basename(scan[0])[11:19]} .. {end.date()})")

    with Pool(8) as p:
        parts = [d for d in p.map(_scan_one, scan, chunksize=8)
                 if d is not None]
    allg = pd.concat(parts, ignore_index=True)
    ndays = allg.date.nunique()

    agg = allg.groupby("ticker").agg(
        med_vol=("vol", "median"), med_px=("pxCls", "median"),
        days=("date", "nunique"), last_ern=("lastErn", "max"))
    agg["cover"] = agg.days / ndays
    agg["ern_age"] = (end - pd.to_datetime(agg.last_ern,
                                           errors="coerce")).dt.days

    ok = agg[(agg.ern_age <= ERN_MAX_AGE)
             & (agg.cover >= MIN_COVER)
             & (agg.med_px >= MIN_PX)].copy()
    print(f"{len(agg)} tickers scanned -> {len(ok)} reporting single names "
          f"(lastErn <= {ERN_MAX_AGE}d, cover >= {MIN_COVER}, "
          f"px >= ${MIN_PX:.0f})")

    top = (ok.sort_values("med_vol", ascending=False).head(TOP_N)
             .reset_index())
    top.insert(0, "rank", np.arange(1, len(top) + 1))
    top["asof"] = end.date()
    top[["rank", "ticker", "med_vol", "med_px", "cover", "asof"]].to_csv(
        NEW_CSV, index=False)
    print(f"wrote {NEW_CSV} ({len(top)} names, "
          f"#​{TOP_N} cutoff med_vol {top.med_vol.iloc[-1]:,.0f})")

    old = set(pd.read_csv(OLD_CSV).ticker)
    new = set(top.ticker)
    print(f"\noverlap with old universe: {len(old & new)}")
    print(f"dropped ({len(old - new)}): "
          f"{', '.join(sorted(old - new))}")
    print(f"added ({len(new - old)}): {', '.join(sorted(new - old))}")

    # ---- backfill signal history for tickers new to the panel -------------
    have = set(pd.read_parquet(f"{CACHE}/ern_panel.parquet",
                               columns=["ticker"]).ticker.unique())
    live_path = f"{CACHE}/ern_panel_live.parquet"
    live = pd.read_parquet(live_path) if os.path.exists(live_path) else None
    if live is not None:
        have |= set(live.ticker.unique())
    need = sorted(new - have)
    if not need:
        print("\nno panel backfill needed")
        return
    bf_files = [f for f in files
                if os.path.basename(f)[11:19] >= BACKFILL_FROM.replace("-", "")]
    print(f"\nbackfilling {len(need)} new tickers over {len(bf_files)} "
          f"core files: {', '.join(need)}")
    _panel_one.tickers = set(need)
    with Pool(8) as p:
        parts = [d for d in p.map(_panel_one, bf_files, chunksize=20)
                 if d is not None]
    add = pd.concat(parts, ignore_index=True)
    add["tradeDate"] = pd.to_datetime(add.tradeDate)
    add["lastErn"] = pd.to_datetime(add.lastErn, errors="coerce")
    for c in PANEL_COLS[2:]:
        if c != "lastErn" and c in add.columns:
            add[c] = pd.to_numeric(add[c], errors="coerce")
    live = add if live is None else pd.concat([live, add])
    live = (live.drop_duplicates(subset=["ticker", "tradeDate"], keep="last")
                .sort_values(["ticker", "tradeDate"]))
    live.to_parquet(live_path)
    print(f"panel backfill done: +{len(add)} rows "
          f"({add.tradeDate.min().date()} .. {add.tradeDate.max().date()}); "
          f"live panel now {live.ticker.nunique()} tickers")


if __name__ == "__main__":
    main()
