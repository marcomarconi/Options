#!/usr/bin/env python3
"""
Rebuild the VRP signal panel from ORATS core day-files ONLY.

Why this exists: the whole vrp_etf_model study was built on `sharpe_two/data/s2_dataset.pq`,
which is Marco's R rebuild and is not a permitted input (see _data/README.md, the
`straRetM1` trap). Every feature that study used -- iv30d, iv60d, clsHv20d, ivPctile1y,
avgOptVolu20d, confidence, pxCls, straPxM1, volOfVol, volOfIvol -- is a RAW core column,
so the panel is fully reconstructible. Only the target had to be rebuilt (see below).

THE TARGET. The old study's target came from a column called `fwd_rv` whose derivation is
in Marco's R and cannot be audited. Here forward realized vol is taken straight from ORATS:

    fwd_rv(t) = clsHv20d(t + 21 trading days)

i.e. the trailing 20-day close-to-close HV *observed 21 sessions later* is, by definition,
the realized vol over the 21 sessions that follow t. No price series is reconstructed, so
no split handling is needed and no cleaning rule can quietly delete crashes -- which is
exactly what went wrong in `straRetM1`. Rows whose forward window is not fully present
(gaps, delistings, the end of the sample) are dropped, never filled.

Mismatch worth stating: iv30d is a 30-CALENDAR-day implied vol (~21 sessions) and clsHv20d
spans 20 sessions. One session of slack, applied uniformly, so it cannot create a sign.

Output: cache/panel.parquet
Run:    /home/marco/trading/.venv_orats/bin/python build_panel.py
"""
import glob
import os
import re
from concurrent.futures import ProcessPoolExecutor

import numpy as np
import pandas as pd

CORE = "/home/marco/trading/HistoricalData/ORATS/core"
OUT = "/home/marco/trading/Systems/Options/Strategies/vrp_etf_v2/cache/panel.parquet"

COLS = ["ticker", "tradeDate", "assetType", "pxCls", "avgOptVolu20d", "confidence",
        "iv30d", "iv60d", "clsHv20d", "orHv20d", "ivPctile1y", "ivStdv1y",
        "volOfVol", "volOfIvol", "slope", "straPxM1"]

MIN_VOLU = 500.0        # deliberately loose -- the study's own filter is 2500, applied later
FWD = 21                # trading sessions; the horizon clsHv20d(t+21) measures


def one(f):
    try:
        d = pd.read_csv(f, usecols=COLS)
    except Exception:
        return None
    d = d[pd.to_numeric(d.avgOptVolu20d, errors="coerce") >= MIN_VOLU]
    if d.empty:
        return None
    for c in COLS:
        if c not in ("ticker", "tradeDate"):
            d[c] = pd.to_numeric(d[c], errors="coerce").astype("float32")
    d["tradeDate"] = pd.to_datetime(d.tradeDate, errors="coerce")
    return d.dropna(subset=["tradeDate"])


def main():
    files = sorted(glob.glob(f"{CORE}/orats_core_*.csv.gz"))
    print(f"{len(files):,} core day-files")
    parts = []
    with ProcessPoolExecutor(10) as ex:
        for i, d in enumerate(ex.map(one, files, chunksize=20)):
            if d is not None:
                parts.append(d)
            if (i + 1) % 500 == 0:
                print(f"  {i+1:,}/{len(files):,}")
    df = pd.concat(parts, ignore_index=True)
    print(f"raw panel {len(df):,} rows, {df.ticker.nunique():,} tickers, "
          f"{df.tradeDate.min().date()} .. {df.tradeDate.max().date()}")

    # ---- forward realized vol: the SAME column, read 21 sessions later.
    df = df.sort_values(["ticker", "tradeDate"], kind="mergesort").reset_index(drop=True)
    g = df.groupby("ticker", sort=False)
    df["fwd_rv"] = g.clsHv20d.shift(-FWD)
    df["fwd_date"] = g.tradeDate.shift(-FWD)
    # only trust the shift if the row 21 ahead really is ~21 sessions ahead: a delisting or
    # a data gap would otherwise splice an unrelated future onto the signal.
    gap = (df.fwd_date - df.tradeDate).dt.days
    bad = gap.isna() | (gap < 25) | (gap > 45)
    print(f"dropping {bad.sum():,} rows ({100*bad.mean():.1f}%) whose +{FWD}-session window "
          f"is missing or spliced across a gap")
    df = df[~bad].drop(columns=["fwd_date"])

    df.to_parquet(OUT, index=False, compression="zstd")
    print(f"\nwrote {OUT}: {len(df):,} rows, {df.ticker.nunique():,} tickers")
    print(df.groupby(df.tradeDate.dt.year).size().to_string())


if __name__ == "__main__":
    main()
