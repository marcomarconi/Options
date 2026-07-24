"""
Shared book selection for the restrike study.

The restrike sweep was originally run BLIND (every eligible name, every month) to isolate
the delta-management effect on maximum sample. But the band has to be chosen on the book
that is actually traded, so this applies the study's own screener on top: the blend of
mean pct-rank of {log(IV/RV), volOfVol, inverted 30/60 fwd-ratio}, ranked within entry
month, exactly as `screener.py` ranks candidates.

Both the restrike frame and the share-hedged frame are filtered to the SAME entries, so
'naked', every band and the hedge ceiling always price an identical set of trades.

books:  all    every eligible name, every month (blind)
        top20  blend top 20%      -- the gross book of README table 42
        top10  blend top 10%
        net    ETF only: >=250k contracts/day, then blend top 50% within that set
               -- the only book with a positive net-of-cost verdict
"""
import numpy as np
import pandas as pd
from scipy import stats as sps

BOOKS = ["all", "top20", "top10", "net"]
LABEL = {"all": "blind (every name)", "top20": "blend top 20%",
         "top10": "blend top 10%", "net": "NET book: liquid + blend top 50%"}

_pct = lambda x: sps.rankdata(x) / (len(x) + 1.0)


def _blend_rank(d):
    """Rank the blend itself -> uniform, so a top-X% cut is exact. The raw blend is a mean
    of three ranks and concentrates near 0.5; cutting it at 0.80 is NOT the top 20%."""
    d = d.sort_values(["tradeDate", "ticker"]).copy()
    q = []
    for _, g in d.groupby(d.tradeDate.values.astype("datetime64[M]"), sort=True):
        b = (_pct(g.f_ivrv) + _pct(g.f_vov) + (1.0 - _pct(g.f_ffr))) / 3.0
        q.append(_pct(b))
    d["q"] = np.concatenate(q)
    return d


def load(kind, book="all", base=".."):
    """-> (restrike frame, hedged frame) on a common, book-filtered entry set."""
    rs = pd.read_parquet(f"{base}/cache/restrike_{kind}.parquet")
    hd = pd.read_parquet(f"{base}/cache/real_trades_{kind}.parquet")

    if book == "net":
        if kind != "etf":
            raise SystemExit("book 'net' is ETF-only (the >=250k liquidity tier)")
        hd = _blend_rank(hd[hd.avgOptVolu20d >= 250_000])
        hd = hd[hd.q >= 0.50]
    elif book != "all":
        cut = {"top20": 0.80, "top10": 0.90}[book]
        hd = _blend_rank(hd)
        hd = hd[hd.q >= cut]

    key = hd[["ticker", "tradeDate"]].drop_duplicates()
    return rs.merge(key, on=["ticker", "tradeDate"], how="inner"), hd, key
