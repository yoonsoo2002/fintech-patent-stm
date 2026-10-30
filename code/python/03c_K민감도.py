"""
================================================================================
 03c_K민감도.py  —  [단계 5 보조] K-NN K 민감도 (K = cfg.KNN_K_GRID)
================================================================================
 입력 : pos/neg/full_patents_vectors.npy, full_patents_with_scores.csv (전부 02 산출)
 출력 : KNN_K_Sensitivity.csv  (K별 진성 건수·비율, 기준 K 대비 Jaccard, K≠기준 평균 Jaccard)

 02와 동일한 판정: Top-K 평균 코사인(진성 시드) > Top-K 평균 코사인(안티시드) → 진성.
 재임베딩 없이 02가 저장한 npy를 그대로 쓴다. 기준 K(cfg.KNN_K) 결과가
 full_patents_with_scores.csv 의 is_fintech 와 완전히 같은지 먼저 확인한다.
================================================================================
"""
import os, sys
import numpy as np
import pandas as pd
from sklearn.metrics.pairwise import cosine_similarity
from datetime import datetime

import importlib.util as _u
_spec = _u.spec_from_file_location("cfg", os.path.join(os.path.dirname(__file__) if "__file__" in globals() else ".", "00_config.py"))
cfg = _u.module_from_spec(_spec); _spec.loader.exec_module(cfg)

if cfg.USE_COLAB:
    from google.colab import drive; drive.mount("/content/drive")

def to_bool(s):
    return s.astype(bool) if s.dtype == bool else s.astype(str).str.strip().str.upper().eq("TRUE")

# ── 로드 ────────────────────────────────────────────────────────────────────
pos_vecs  = np.load(cfg.P(cfg.POS_VECS_NPY))
neg_vecs  = np.load(cfg.P(cfg.NEG_VECS_NPY))
full_vecs = np.load(cfg.P(cfg.FULL_VECS_NPY))
df = pd.read_csv(cfg.P(cfg.SCORES_OUTPUT), usecols=["_family_key", "is_fintech"], low_memory=False)
if len(df) != len(full_vecs):
    raise ValueError(f"행 수 불일치: scores {len(df)} vs full_vecs {len(full_vecs)}")
print(f"  전체 {len(full_vecs):,} / 진성 시드 {len(pos_vecs)} / 안티시드 {len(neg_vecs)}")

# ── 코사인 (한 번만 계산·오름차순 정렬, K마다 뒤 K개 평균 — 02와 같은 연산 순서) ──
sim_pos = np.sort(cosine_similarity(full_vecs, pos_vecs), axis=1)
sim_neg = np.sort(cosine_similarity(full_vecs, neg_vecs), axis=1)

K0 = cfg.KNN_K
grid = sorted(set(cfg.KNN_K_GRID) | {K0})
sel = {}
for K in grid:
    pos_avg = sim_pos[:, -K:].mean(axis=1)
    neg_avg = sim_neg[:, -K:].mean(axis=1)
    sel[K] = pos_avg > neg_avg

# 기준 K 재현 확인 (02 산출 is_fintech와 행 단위 일치해야 함)
base_saved = to_bool(df["is_fintech"]).values
n_mismatch = int((sel[K0] != base_saved).sum())
print(f"  [재현] K={K0} 재계산 vs 저장 is_fintech 불일치 {n_mismatch}건")
if n_mismatch:
    print("  ⚠️ 기준 K 재현 실패 — npy/CSV가 같은 02 실행 산출인지 확인하세요.")

# ── 표 ─────────────────────────────────────────────────────────────────────
N = len(df)
base = sel[K0]
rows = []
for K in grid:
    s = sel[K]
    inter = int((s & base).sum()); union = int((s | base).sum())
    jac = inter / union if union else np.nan
    rows.append({
        "K": K, "n_retained": int(s.sum()), "share_pct": round(s.sum() / N * 100, 2),
        "n_vs_base": int(s.sum()) - int(base.sum()),
        "intersect_base": inter, "union_base": union,
        "jaccard_vs_base": round(jac, 4), "is_base": K == K0,
    })
out = pd.DataFrame(rows)
mean_jac = out.loc[~out["is_base"], "jaccard_vs_base"].mean()
out = pd.concat([out, pd.DataFrame([{
    "K": f"mean(K≠{K0})", "n_retained": np.nan, "share_pct": np.nan, "n_vs_base": np.nan,
    "intersect_base": np.nan, "union_base": np.nan,
    "jaccard_vs_base": round(mean_jac, 4), "is_base": False,
}])], ignore_index=True)
for c in ["n_retained", "n_vs_base", "intersect_base", "union_base"]:
    out[c] = out[c].astype("Int64")
out["base_K"] = K0
out["base_reproduced"] = n_mismatch == 0
out["run_date"] = datetime.now().strftime("%Y-%m-%d")

print(out[["K", "n_retained", "share_pct", "jaccard_vs_base"]].to_string(index=False))
print(f"  ★평균 Jaccard (K≠{K0} vs K={K0}) = {mean_jac*100:.2f}%")
out.to_csv(cfg.P(cfg.K_SENS_OUTPUT), index=False, encoding="utf-8-sig")
print(f"\n💾 저장: {cfg.K_SENS_OUTPUT}")
