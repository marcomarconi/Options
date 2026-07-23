"""
Point-in-time (past-only) rich/cheap gate on impErnMv.

The first-pass analysis ranked impErnMv against each ticker's FULL-sample
history — lookahead (caught by audit_prern.py, 2026-07-11). Use this
expanding rank instead: each entry is ranked only against the same ticker's
PRIOR events (>=4 required, else NaN). Honest numbers are ~25-30% smaller
than the lookahead ones but the gate survives.
"""
import numpy as np
import pandas as pd


def past_rank(s):
    v = s.values
    out = np.full(len(v), np.nan)
    for i in range(len(v)):
        if i >= 4:
            out[i] = (v[:i] < v[i]).mean() + 0.5 * (v[:i] == v[i]).mean()
    return pd.Series(out, index=s.index)


def add_mv_rank(df, by=("ticker",)):
    """Adds mv_rk (past-only expanding pct rank of impErnMv within `by`).
    Pass by=("ticker","lag") for the lag-scan set so each event appears once
    per rank group."""
    by = list(by)
    df = (df.dropna(subset=["impErnMv"])
            .sort_values(by + ["ernDate"]).copy())
    df["mv_rk"] = df.groupby(by).impErnMv.transform(past_rank)
    return df
