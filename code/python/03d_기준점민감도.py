"""
================================================================================
 03d_기준점민감도.py  —  [단계 5 보조] net 점수 컷오프(기준점) 민감도
================================================================================
 입력 : full_patents_with_scores.csv (02 산출: top_k_pos_avg, top_k_neg_avg, Origin_Country)
 출력 : Threshold_Sensitivity.csv
        (컷오프별 진성 건수·비율, 기준(net>0) 대비 Jaccard, EP 및 국가별 건수·비율)

 net = top_k_pos_avg − top_k_neg_avg. 기준은 02와 같은 net > 0.
 비교 컷오프: ① 0 ± cfg.NET_SHIFT_GRID (절대 이동)
              ② 전체 net 분포의 cfg.NET_QUANTILE_GRID 분위수 (보존 비율 고정)
================================================================================
"""
import os, sys
import numpy as np
import pandas as pd
from datetime import datetime

import importlib.util as _u
_spec = _u.spec_from_file_location("cfg", os.path.join(os.path.dirname(__file__) if "__file__" in globals() else ".", "00_config.py"))
cfg = _u.module_from_spec(_spec); _spec.loader.exec_module(cfg)

if cfg.USE_COLAB:
    from google.colab import drive; drive.mount("/content/drive")

def to_bool(s):
    return s.astype(bool) if s.dtype == bool else s.astype(str).str.strip().str.upper().eq("TRUE")

# ── 로드 ────────────────────────────────────────────────────────────────────
df = pd.read_csv(cfg.P(cfg.SCORES_OUTPUT),
                 usecols=["_family_key", "Origin_Country", "top_k_pos_avg", "top_k_neg_avg", "is_fintech"],
                 low_memory=False)
net = (df["top_k_pos_avg"] - df["top_k_neg_avg"]).values
country = df["Origin_Country"].astype(str).values
N = len(df)

base = net > 0
n_mismatch = int((base != to_bool(df["is_fintech"]).values).sum())
print(f"  [재현] net>0 vs 저장 is_fintech 불일치 {n_mismatch}건")

# ── 컷오프 목록 ─────────────────────────────────────────────────────────────
cuts = [("baseline", "net > 0", 0.0)]
for d in sorted(cfg.NET_SHIFT_GRID):
    cuts.append(("shift", f"net > {d:+.4f}", float(d)))
for q in cfg.NET_QUANTILE_GRID:
    cuts.append(("quantile", f"net > Q{q:.3f} (상위 {(1-q)*100:.1f}%)", float(np.quantile(net, q))))

countries = [c for c in ["US", "CN", "JP", "KR", "EP"] if c in set(country)]
rows = []
for kind, label, c in cuts:
    s = net > c
    n = int(s.sum())
    inter = int((s & base).sum()); union = int((s | base).sum())
    row = {
        "type": kind, "rule": label, "cutoff": c,
        "n_retained": n, "share_pct": round(n / N * 100, 2), "n_vs_base": n - int(base.sum()),
        "jaccard_vs_base": round(inter / union, 4) if union else np.nan,
        "n_EP": int((s & (country == "EP")).sum()),
        "EP_share_pct": round((s & (country == "EP")).sum() / n * 100, 2) if n else np.nan,
        "EP_retention_pct": round((s & (country == "EP")).sum() / (country == "EP").sum() * 100, 2),
    }
    for cc in countries:
        if cc != "EP":
            row[f"n_{cc}"] = int((s & (country == cc)).sum())
    rows.append(row)

out = pd.DataFrame(rows)
out["base_reproduced"] = n_mismatch == 0
out["run_date"] = datetime.now().strftime("%Y-%m-%d")

print(out[["rule", "cutoff", "n_retained", "share_pct", "jaccard_vs_base", "n_EP", "EP_share_pct"]].to_string(index=False))
out.to_csv(cfg.P(cfg.THRESH_SENS_OUTPUT), index=False, encoding="utf-8-sig")
print(f"\n💾 저장: {cfg.THRESH_SENS_OUTPUT}")
