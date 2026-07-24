#!/usr/bin/env python3
"""
AUDIT of two suspicious assumptions in the real-quote backtests.

(1) LOOK-AHEAD IN THE SPLIT FILTER. `split_days()` scales each session's |return| by a
    rolling median taken over a CENTRED 121-session window -- i.e. it uses up to 60 sessions
    of future data to decide whether today was a corporate action, and trades touching a
    flagged day are dropped. This re-runs the same test with a strictly TRAILING window and
    reports where the two disagree, per ticker and per dropped trade.

(2) ENTRY ATTRITION. Only ~76% of eligible monthly entries ever get priced. This instruments
    the eligibility walk to record WHY each entry was dropped, then tests whether the drop
    rate is elevated in stressed markets (which would mean the study systematically discards
    its own worst trades -- fatal for a short-vol result) or merely in illiquid names.

Run from analysis/:  /home/marco/trading/.venv_orats/bin/python audit_lookahead_attrition.py [etf|stock]
"""
import sys
from collections import Counter
from concurrent.futures import ProcessPoolExecutor

import numpy as np
import pandas as pd

sys.path.insert(0, "/home/marco/trading/Systems/Options/Strategies/vrp_etf_v2/analysis")
from common import load  # noqa: E402

STRIKES = "/home/marco/trading/Systems/Options/Strategies/_data/strikes"
DTE_LO, DTE_HI, DTE_TGT = 20, 45, 30
MAX_MONEY = 0.03
COLS = ["tdate", "expir", "dte_ltd", "strike", "stk", "cBid", "cAsk", "pBid", "pAsk", "delta"]


def _z(ch, center):
    s = ch.drop_duplicates("td").set_index("td").sort_index().stk
    r = np.log(s / s.shift(1))
    scale = 1.4826 * r.abs().rolling(121, center=center, min_periods=40).median()
    return s.index, ((r.abs() / scale > 12) & (r.abs() > 0.20)).fillna(False)


def one_ticker(args):
    tk, entries = args
    try:
        ch = pd.read_parquet(f"{STRIKES}/ticker={tk}", columns=COLS)
    except Exception:
        return dict(tk=tk, drops=Counter({"no_chain_file": len(entries)}), rows=[],
                    split_c=set(), split_t=set())
    ch["td"] = pd.to_datetime(ch.tdate)
    ch["ex"] = pd.to_datetime(ch.expir)
    ch = ch[(ch.dte_ltd >= 0) & (ch.stk > 0)]
    if ch.empty:
        return dict(tk=tk, drops=Counter({"empty_chain": len(entries)}), rows=[],
                    split_c=set(), split_t=set())

    idx, hit_c = _z(ch, True)          # the filter as shipped  (centred -> look-ahead)
    _, hit_t = _z(ch, False)           # strictly trailing      (causal)
    bad_c, bad_t = set(idx[hit_c]), set(idx[hit_t])

    drops, rows = Counter(), []
    for d in entries:
        d = pd.Timestamp(d)
        reason, days = None, None
        day = ch[ch.td == d]
        if day.empty:
            reason = "no_session_in_archive"
        else:
            cand = day[(day.dte_ltd >= DTE_LO) & (day.dte_ltd <= DTE_HI)]
            if cand.empty:
                reason = "no_expiry_in_dte_window"
            else:
                dtes = cand.dte_ltd.unique()
                ex = cand[cand.dte_ltd == dtes[np.argmin(np.abs(dtes - DTE_TGT))]].ex.iloc[0]
                life = ch[(ch.ex == ex) & (ch.td >= d)].sort_values("td")
                if not (life.dte_ltd == 0).any():
                    reason = "expiry_never_settles_in_archive"
                else:
                    life = life[life.td <= life[life.dte_ltd == 0].td.min()]
                    days = set(life.td.unique())
                    if len(days) < 5:
                        reason = "under_5_sessions"
                    elif bad_c & days:
                        reason = "split_day_touched"
                    else:
                        d0 = life[life.td == d].set_index("strike")
                        cb, ca, pb, pa = d0.cBid.values, d0.cAsk.values, d0.pBid.values, d0.pAsk.values
                        ok = (cb > 0) & (pb > 0) & (ca > cb) & (pa > pb)
                        if not ok.any():
                            reason = "no_two_sided_atm_quote"
                        else:
                            k = d0.index.values.astype(float)[ok]
                            S0 = float(d0.stk.values[0])
                            if abs(k[np.abs(k - S0).argmin()] / S0 - 1) > MAX_MONEY:
                                reason = "no_strike_within_3pct"
        drops[reason or "KEPT"] += 1
        # look-ahead disagreement: would the CAUSAL filter have made a different call?
        flag_c = reason == "split_day_touched"
        flag_t = bool(bad_t & days) if days is not None else False
        rows.append((tk, d, reason or "KEPT", flag_c, flag_t))
    return dict(tk=tk, drops=drops, rows=rows, split_c=bad_c, split_t=bad_t)


def main():
    kind = sys.argv[1] if len(sys.argv) > 1 else "etf"
    d = load(kind)
    d = d[(d.tradeDate >= "2013-01-01") & (d.tradeDate <= "2024-11-15")]
    d["ym"] = d.tradeDate.values.astype("datetime64[M]")
    m = d.sort_values("tradeDate").groupby(["ticker", "ym"], as_index=False).first()
    jobs = [(tk, g.tradeDate.tolist()) for tk, g in m.groupby("ticker")]
    print(f"{kind}: auditing {len(m):,} eligible monthly entries over {len(jobs)} tickers\n", flush=True)

    drops, rows, nc, nt, both = Counter(), [], 0, 0, 0
    with ProcessPoolExecutor(10) as ex:
        for r in ex.map(one_ticker, jobs, chunksize=4):
            drops += r["drops"]; rows += r["rows"]
            nc += len(r["split_c"]); nt += len(r["split_t"]); both += len(r["split_c"] & r["split_t"])
    R = pd.DataFrame(rows, columns=["ticker", "tradeDate", "reason", "flag_centred", "flag_trailing"])

    # ---------- (1) look-ahead ----------
    print("=" * 78)
    print("(1) LOOK-AHEAD: centred (as shipped) vs trailing (causal) split filter")
    print("=" * 78)
    print(f"  sessions flagged as corporate actions: centred {nc}, trailing {nt}, in both {both}")
    print(f"  centred-only {nc - both}  |  trailing-only {nt - both}")
    dis = R[R.flag_centred != R.flag_trailing]
    print(f"  entries where the two filters DISAGREE: {len(dis)} of {len(R):,} ({len(dis)/len(R)*100:.2f}%)")
    if len(dis):
        print(dis.groupby(["flag_centred", "flag_trailing"]).size().to_string())
        print("\n  sample of disagreements:")
        print(dis.head(12).to_string(index=False))

    # ---------- (2) attrition ----------
    print("\n" + "=" * 78)
    print("(2) ATTRITION: why eligible entries never get priced")
    print("=" * 78)
    tot = len(R)
    for k, v in drops.most_common():
        print(f"  {k:<34}{v:>7,}{v/tot*100:>8.1f}%")

    R["ym"] = R.tradeDate.values.astype("datetime64[M]")
    R["dropped"] = R.reason != "KEPT"
    # stress proxy: SPY iv30d at entry month, from the permitted panel
    spy = d[d.ticker == "SPY"].groupby("ym").iv30d.mean()
    by = R.groupby("ym").dropped.mean().to_frame("drop_rate").join(spy.rename("spy_iv30"))
    by = by.dropna()
    c = np.corrcoef(by.drop_rate, by.spy_iv30)[0, 1]
    hi = by[by.spy_iv30 >= by.spy_iv30.quantile(0.80)]
    lo = by[by.spy_iv30 <= by.spy_iv30.quantile(0.20)]
    print(f"\n  IS ATTRITION STRESS-RELATED?  corr(monthly drop rate, SPY iv30d) = {c:+.3f}")
    print(f"    top-quintile-vol months: drop rate {hi.drop_rate.mean()*100:.1f}%  (n={len(hi)} months)")
    print(f"    bottom-quintile-vol months: drop rate {lo.drop_rate.mean()*100:.1f}%  (n={len(lo)} months)")
    print("\n  worst 8 months by drop rate:")
    print(by.sort_values("drop_rate", ascending=False).head(8).to_string())

    # is it liquidity?
    v = m.set_index(["ticker", "tradeDate"]).avgOptVolu20d
    R2 = R.join(v, on=["ticker", "tradeDate"])
    q = pd.qcut(R2.avgOptVolu20d, 5, duplicates="drop")
    print("\n  IS ATTRITION LIQUIDITY-RELATED?  drop rate by option-volume quintile:")
    print(R2.groupby(q, observed=True).dropped.agg(["mean", "size"]).to_string())

    R.to_parquet(f"../out/audit_attrition_{kind}.parquet", index=False)
    print(f"\n  per-entry detail -> ../out/audit_attrition_{kind}.parquet")


if __name__ == "__main__":
    main()
