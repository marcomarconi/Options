"""
Dedicated audit of the LAG-2 book (the recommended trade) — 2026-07-11.

Failure modes that would fake a short-hold result specifically:
 A. ANNOUNCEMENT LEAKAGE: if ernDate/session codes are off by a day, the
    exit close already contains the jump. Test: underlying |return| ON the
    exit day (should be ordinary) vs on the FIRST day AFTER exit (should be
    huge = the jump we deliberately never hold). Flag trades with a jump-like
    exit day and a quiet post-day, and measure their pnl contribution.
 B. SPREAD-WIDENING ARTIFACT: quotes widen into events; mid can rise with no
    tradeable pnl. Test: recompute the edge bid-to-bid and ask-to-ask
    (entry_bid = 2*mid - ask; exit_ask = 2*mid - bid). A real edge survives
    on the bid side; a widening artifact shows ask>>mid>>bid ordering with
    bid-to-bid ~ 0.
 C. quote quality at the short horizon: zero exit bids, entry==exit-day
    trades, held-days distribution, outlier sensitivity.

Run from analysis/:  python3 audit_lag2.py
"""
import numpy as np
import pandas as pd

R = "ret_spot_mid"


def t(r):
    r = np.asarray(r, float)
    return r.mean() / r.std() * np.sqrt(len(r))


def main():
    ls = pd.read_parquet("../cache/trades_prern_lagscan.parquet")
    d = ls[ls.lag == 2].copy()
    print(f"lag-2 book: {len(d)} trades, mean {d[R].mean()*100:+.3f}%/spot, "
          f"t={t(d[R]):+.2f}")

    # ---- A. announcement leakage ------------------------------------------
    # CAUTION (found the hard way): ORATS core pxCls is the PREVIOUS close —
    # the row dated t carries close(t-1). True close(t) = pxCls(t+1). Using
    # pxCls unshifted moves the whole jump profile one day late and fakes an
    # "off-by-one calendar" finding. smv stk is same-day (verified vs AAPL
    # 2023-08-03/04).
    panel = pd.read_parquet("../cache/ern_panel.parquet",
                            columns=["ticker", "tradeDate", "pxCls"])
    panel = panel.sort_values(["ticker", "tradeDate"])
    panel["close"] = panel.groupby("ticker").pxCls.shift(-1)
    panel["ret"] = panel.groupby("ticker").close.pct_change(fill_method=None)
    panel["next_ret"] = panel.groupby("ticker").ret.shift(-1)
    d = d.merge(panel.rename(columns={"tradeDate": "exit", "ret": "exit_ret",
                                      "next_ret": "post_ret"})
                [["ticker", "exit", "exit_ret", "post_ret"]],
                on=["ticker", "exit"], how="left")
    print("\n== A. underlying |move| around the exit (medians, %) ==")
    for w, s in d.groupby("when"):
        print(f"{w:4s} n={len(s):5d}  exit day {s.exit_ret.abs().median()*100:.2f}"
              f"  day after exit {s.post_ret.abs().median()*100:.2f}"
              f"  (p90: {s.exit_ret.abs().quantile(.9)*100:.2f} vs "
              f"{s.post_ret.abs().quantile(.9)*100:.2f})")
    # suspicious: jump-sized exit day AND quiet day after = probably miscoded
    s = d[(d.exit_ret.abs() > 0.08) & (d.post_ret.abs() < 0.03)]
    frac_pnl = s[R].sum() / d[R].sum() * 100
    print(f"suspicious (|exit ret|>8%, |post ret|<3%): n={len(s)} "
          f"({len(s)/len(d)*100:.2f}% of trades), {frac_pnl:.1f}% of total pnl")
    clean = d.drop(s.index)
    print(f"edge excluding them: {clean[R].mean()*100:+.3f}%/spot t={t(clean[R]):+.2f}")

    # ---- B. bid-to-bid / ask-to-ask ---------------------------------------
    d["entry_bid"] = 2 * d.debit_mid - d.debit_ask
    d["exit_ask"] = 2 * d.exit_mid - d.exit_bid
    b2b = (d.exit_bid - d.entry_bid) / d.spot
    a2a = (d.exit_ask - d.debit_ask) / d.spot
    m2m = d[R]
    print("\n== B. same-side quote returns (spread-widening test) ==")
    for lab, r in [("bid-to-bid", b2b), ("mid-to-mid", m2m),
                   ("ask-to-ask", a2a)]:
        print(f"{lab:10s} mean {r.mean()*100:+.3f}%/spot  t={t(r):+.2f}")
    hs_in = (d.debit_ask - d.debit_mid) / d.spot
    hs_out = (d.exit_mid - d.exit_bid) / d.spot
    print(f"half-spread/spot: entry median {hs_in.median()*100:.3f}%  "
          f"exit median {hs_out.median()*100:.3f}%  "
          f"(widening at exit would inflate mid)")

    # ---- C. quote quality / outliers --------------------------------------
    print("\n== C. quote quality ==")
    print(f"zero exit bid: {(d.exit_bid <= 0).mean()*100:.2f}%   "
          f"held trading-day median {d.held.median():.0f} "
          f"(calendar; range {d.held.min()}-{d.held.max()})")
    r = d[R]
    print(f"outliers: min {r.min()*100:.1f}% max {r.max()*100:.1f}%; "
          f"clip ±10%: {r.clip(-.1,.1).mean()*100:+.3f}% t={t(r.clip(-.1,.1)):+.2f}; "
          f"trim 1/99: "
          f"{r[(r>r.quantile(.01))&(r<r.quantile(.99))].mean()*100:+.3f}%")


if __name__ == "__main__":
    main()
