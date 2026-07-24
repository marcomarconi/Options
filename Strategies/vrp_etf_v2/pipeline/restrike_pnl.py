#!/usr/bin/env python3
"""
UNHEDGED short straddle with DELTA-BAND RESTRIKING — the "poor-man's delta hedge".

Instead of trading shares to flatten delta (real_pnl.py), we hold no stock and, whenever
the live straddle's position delta |2*delta - 1| breaches a band D, we buy back the drifted
straddle and re-sell a fresh ATM straddle on the SAME expiry. Re-centering the strike resets
delta to ~0, so restriking continuously (D -> 0) is akin to delta-hedging (Marco's point) --
GROSS. The cost structure differs sharply though: a restrike crosses the OPTION spread, a
share-hedge crosses the cheap share spread, so cost is left for a later footnote.

Sweep D in {naked, 0.6, 0.5, 0.4, 0.3, 0.2, 0.15, 0.1}. D=naked = never restrike
(reproduces the unhedged book). One chain-parse per entry prices every band.

Accounting (GROSS, mid): each segment is a short straddle sold at mid; on a restrike it is
bought back at mid and a new one sold at mid; the last segment settles to |S_T - K| intrinsic.
  pnl = (open0 + Σ new_opens) - Σ buybacks - final_intrinsic
Return normalized by the SAME margin convention as real_pnl (0.20*S0 + open0) for comparability.

Run: /home/marco/trading/.venv_orats/bin/python restrike_pnl.py [etf|stock]
Writes cache/restrike_<kind>.parquet (one row per entry x band).
"""
import os
import sys
from concurrent.futures import ProcessPoolExecutor

import numpy as np
import pandas as pd

sys.path.insert(0, "/home/marco/trading/Systems/Options/Strategies/vrp_etf_v2/analysis")
from common import BASE, load  # noqa: E402

STRIKES = "/home/marco/trading/Systems/Options/Strategies/_data/strikes"
DTE_LO, DTE_HI, DTE_TGT = 20, 45, 30
MAX_MONEY = 0.03          # entry ATM tolerance
RESTRIKE_MONEY = 0.05     # new-ATM tolerance on a restrike (grid can be coarser after a move)
BANDS = [np.inf, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.15, 0.1]
COLS = ["tdate", "expir", "dte_ltd", "strike", "stk", "cBid", "cAsk", "pBid", "pAsk", "delta"]


def split_days(ch):
    """Corporate-action sessions (strike grid rescales with spot) — same robust local-z test
    as real_pnl.py. A trade whose hold touches one is dropped."""
    s = ch.drop_duplicates("td").set_index("td").sort_index().stk
    r = np.log(s / s.shift(1))
    scale = 1.4826 * r.abs().rolling(121, center=True, min_periods=40).median()
    z = r.abs() / scale
    hit = ((z > 12) & (r.abs() > 0.20)).fillna(False)
    return set(s.index[hit])


def valid(row):
    return (row.cBid > 0 and row.pBid > 0 and row.cAsk > row.cBid and row.pAsk > row.pBid)


def straddle_mid(row):
    return (row.cBid + row.cAsk) / 2.0 + (row.pBid + row.pAsk) / 2.0


def day_book(g):
    """Vectorized once-per-session digest shared by every band: valid strikes -> (mid, delta),
    plus the nearest-to-spot valid strike. Hoisting this out of the band loop is the whole
    speed-up — the chain work no longer scales with len(BANDS)."""
    k = g.index.values.astype(float)
    cb, ca, pb, pa = g.cBid.values, g.cAsk.values, g.pBid.values, g.pAsk.values
    ok = (cb > 0) & (pb > 0) & (ca > cb) & (pa > pb)
    if not ok.any():
        return None
    k, mid = k[ok], ((cb + ca) / 2.0 + (pb + pa) / 2.0)[ok]
    dlt = g.delta.values[ok]
    spot = float(g.stk.values[0])
    j = np.abs(k - spot).argmin()
    Knew = float(k[j]) if abs(k[j] / spot - 1) <= RESTRIKE_MONEY else None
    return dict(mid=dict(zip(k, mid)), dlt=dict(zip(k, dlt)), Knew=Knew,
                mid_new=float(mid[j]) if Knew is not None else np.nan)


def one_ticker(args):
    tk, entries = args
    try:
        ch = pd.read_parquet(f"{STRIKES}/ticker={tk}", columns=COLS)
    except Exception:
        return None
    ch["td"] = pd.to_datetime(ch.tdate)
    ch["ex"] = pd.to_datetime(ch.expir)
    ch = ch[(ch.dte_ltd >= 0) & (ch.stk > 0)]
    if ch.empty:
        return None
    bad = split_days(ch)
    out = []

    for d in entries:
        d = pd.Timestamp(d)
        day = ch[ch.td == d]
        if day.empty:
            continue
        cand = day[(day.dte_ltd >= DTE_LO) & (day.dte_ltd <= DTE_HI)]
        if cand.empty:
            continue
        dtes = cand.dte_ltd.unique()
        ex = cand[cand.dte_ltd == dtes[np.argmin(np.abs(dtes - DTE_TGT))]].ex.iloc[0]

        # full life of this expiry from entry to settlement, as a per-day chain
        life = ch[(ch.ex == ex) & (ch.td >= d)].sort_values("td")
        if not (life.dte_ltd == 0).any():
            continue
        exp_day = life[life.dte_ltd == 0].td.min()
        life = life[life.td <= exp_day]
        days = sorted(life.td.unique())
        if bad & set(days) or len(days) < 5:
            continue
        by_day = {t: g.set_index("strike") for t, g in life.groupby("td")}
        books = {t: day_book(g) for t, g in by_day.items()}

        # entry ATM
        b0 = books[days[0]]
        if b0 is None:
            continue
        S0 = float(by_day[days[0]].stk.iloc[0])
        ks = np.fromiter(b0["mid"].keys(), float)
        K0 = float(ks[np.abs(ks - S0).argmin()])
        if abs(K0 / S0 - 1) > MAX_MONEY:
            continue
        open0 = b0["mid"][K0]
        ST = float(by_day[days[-1]].stk.iloc[0])
        interior = [books[t] for t in days[1:-1] if books[t] is not None]

        for D in BANDS:
            K = K0
            new_opens = 0.0
            buybacks = 0.0
            n_rs = 0
            for b in interior:
                dlt = b["dlt"].get(K)
                if dlt is None:                        # strike gone or unquotable that day
                    continue
                if abs(2.0 * dlt - 1.0) <= D:
                    continue
                Knew = b["Knew"]
                if Knew is None or Knew == K:
                    continue
                buybacks += b["mid"][K]                # buy back drifted straddle
                new_opens += b["mid_new"]              # sell fresh ATM
                K = Knew
                n_rs += 1
            intrinsic = abs(ST - K)
            pnl = (open0 + new_opens) - buybacks - intrinsic
            margin = 0.20 * S0 + open0
            out.append(dict(ticker=tk, tradeDate=d, band=D, K0=K0, S0=S0, ST=ST,
                            held=len(days) - 1, n_restrike=n_rs, open0=open0,
                            pnl=pnl, r=pnl / margin, margin=margin))
    return pd.DataFrame(out) if out else None


def main():
    kind = sys.argv[1] if len(sys.argv) > 1 else "etf"
    d = load(kind)
    d = d[(d.tradeDate >= "2013-01-01") & (d.tradeDate <= "2024-11-15")]
    d["ym"] = d.tradeDate.values.astype("datetime64[M]")
    m = d.sort_values("tradeDate").groupby(["ticker", "ym"], as_index=False).first()
    jobs = [(tk, g.tradeDate.tolist()) for tk, g in m.groupby("ticker")]
    print(f"{kind}: {len(m):,} monthly entries over {len(jobs)} tickers", flush=True)

    # checkpoint so a killed run resumes instead of restarting (stock takes ~1h)
    ckpt = f"{BASE}/cache/_restrike_{kind}_ckpt.parquet"
    parts = []
    if os.path.exists(ckpt):
        prev = pd.read_parquet(ckpt)
        parts.append(prev)
        done = set(prev.ticker.unique())
        jobs = [j for j in jobs if j[0] not in done]
        print(f"  resuming: {len(done)} tickers already done, {len(jobs)} left", flush=True)

    def flush():
        tmp = ckpt + ".tmp"
        pd.concat(parts, ignore_index=True).to_parquet(tmp, index=False, compression="zstd")
        os.replace(tmp, ckpt)

    with ProcessPoolExecutor(10) as ex:
        for i, r in enumerate(ex.map(one_ticker, jobs, chunksize=4)):
            if r is not None:
                parts.append(r)
            if (i + 1) % 100 == 0:
                print(f"  {i+1}/{len(jobs)} tickers, {sum(len(p) for p in parts):,} rows", flush=True)
            if (i + 1) % 300 == 0 and parts:
                flush()
    t = pd.concat(parts, ignore_index=True)
    t = t.merge(m[["ticker", "tradeDate", "f_ivrv", "f_ffr", "f_vov", "f_ivlvl"]],
                on=["ticker", "tradeDate"], how="left")
    out = f"{BASE}/cache/restrike_{kind}.parquet"
    t.to_parquet(out, index=False, compression="zstd")
    if os.path.exists(ckpt):
        os.remove(ckpt)
    print(f"\nwrote {out}: {len(t):,} rows\n")

    def sharpe_m(g):
        s = pd.Series(g.r.values, index=g.tradeDate.values.astype("datetime64[M]"))
        s = s.groupby(level=0).mean()
        return s.mean() / s.std() * np.sqrt(12) if s.std() > 0 else np.nan

    print(f"{'band D':>8}{'mean/mo':>10}{'std':>8}{'Sharpe':>8}{'win':>7}{'restrikes/tr':>14}")
    for D in BANDS:
        g = t[t.band == D]
        lab = "naked" if not np.isfinite(D) else f"{D:.2f}"
        print(f"{lab:>8}{g.r.mean()*100:>+9.3f}%{g.r.std()*100:>7.1f}%"
              f"{sharpe_m(g):>+8.2f}{(g.r>0).mean()*100:>6.0f}%{g.n_restrike.mean():>14.1f}")


if __name__ == "__main__":
    main()
