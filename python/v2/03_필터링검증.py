"""
================================================================================
 03_필터링검증.py  —  [단계 5] 시멘틱 필터링 정량 검증 (Cohen's d · F1)
================================================================================
 입력 : full_patents_with_scores.csv, final_seed.csv, full_patents_vectors.npy (전부 v2 산출)
 출력 : Semantic_Validation_Metrics.csv (부록 표 A-2)
        + 콘솔에 새 Cohen's d / F1 → 워크플로우 §5에서 위키 핵심수치레지스터에 기록

 v1(임베딩성능평가.py)을 충실히 옮기되:
   · ★v1의 치명적 함정: 셀7의 "1.924"/"14.0%"/"+0.271"는 계산값이 아니라 박힌 문자열이었음.
     → 이 스크립트는 전부 계산값으로 출력/저장한다(하드코딩 금지). 새 데이터에서 자동 갱신.
================================================================================
"""
import os, sys
import numpy as np
import pandas as pd
from scipy import stats
from sklearn.metrics import confusion_matrix, precision_score, recall_score, f1_score, accuracy_score
from datetime import datetime

import importlib.util as _u
_spec = _u.spec_from_file_location("cfg", os.path.join(os.path.dirname(__file__) if "__file__" in globals() else ".", "00_config.py"))
cfg = _u.module_from_spec(_spec); _spec.loader.exec_module(cfg)

if cfg.USE_COLAB:
    from google.colab import drive; drive.mount("/content/drive")

def to_bool(s):
    return s.astype(bool) if s.dtype == bool else s.astype(str).str.strip().str.upper().eq("TRUE")

# ── 로드 ────────────────────────────────────────────────────────────────────
df = pd.read_csv(cfg.P(cfg.SCORES_OUTPUT), low_memory=False).reset_index(drop=True)
seed = pd.read_csv(cfg.P(cfg.FINAL_SEED), low_memory=False)
df["net_score"] = df["top_k_pos_avg"] - df["top_k_neg_avg"]
mask = to_bool(df["is_fintech"])
net_t, net_f = df.loc[mask, "net_score"].values, df.loc[~mask, "net_score"].values
n1, n2 = len(net_t), len(net_f)

# ── 1) Cohen's d (계산값) ───────────────────────────────────────────────────
m1, m2 = net_t.mean(), net_f.mean()
sd1, sd2 = net_t.std(ddof=1), net_f.std(ddof=1)
pooled = np.sqrt(((n1-1)*sd1**2 + (n2-1)*sd2**2) / (n1+n2-2))
d = (m1 - m2) / pooled
corr = 1 - (3 / (4*(n1+n2) - 9)); g = d * corr
se = np.sqrt((n1+n2)/(n1*n2) + d**2/(2*(n1+n2)))
ci = (d - 1.96*se, d + 1.96*se)
t_stat, p_val = stats.ttest_ind(net_t, net_f, equal_var=False)

def interp(x):
    x = abs(x)
    return "negligible" if x<0.2 else "small" if x<0.5 else "medium" if x<0.8 else "large"

print("="*60)
print(f"  진성 n={n1:,} 평균net={m1:+.5f} | 노이즈 n={n2:,} 평균net={m2:+.5f}")
print(f"  Welch t={t_stat:.2f}, p={p_val:.2e}")
print(f"  ★Cohen's d = {d:.4f}  95%CI[{ci[0]:.4f},{ci[1]:.4f}]  ({interp(d)})")
print(f"   Hedges g = {g:.4f}")
print("="*60)

# ── 2) 자기참조 F1 (시드 매칭) ──────────────────────────────────────────────
seed["_gt"] = to_bool(seed["fintech"])
df["_pred"] = to_bool(df["is_fintech"])
merged = seed[["_family_key", "_gt"]].merge(df[["_family_key", "_pred"]], on="_family_key", how="left")
md = merged.dropna(subset=["_pred"])
y_true, y_pred = md["_gt"].astype(bool).values, md["_pred"].astype(bool).values
tn, fp, fn, tp = confusion_matrix(y_true, y_pred, labels=[False, True]).ravel()
P = precision_score(y_true, y_pred, zero_division=0); R = recall_score(y_true, y_pred, zero_division=0)
F1 = f1_score(y_true, y_pred, zero_division=0); SPEC = tn/(tn+fp) if (tn+fp) else 0
print(f"  [자기참조] P={P:.3f} R={R:.3f} F1={F1:.3f} Spec={SPEC:.3f}  (매칭 {len(md)}/{len(seed)})")

# ── 3) Leave-One-Out F1 (자기 cos=1.0 제외 → 보수적 하한) ──────────────────
full_vecs = np.load(cfg.P(cfg.FULL_VECS_NPY))
from sklearn.metrics.pairwise import cosine_similarity
key2idx = pd.Series(df.index.values, index=df["_family_key"]).to_dict()
seed["_idx"] = seed["_family_key"].map(key2idx)
sv = seed.dropna(subset=["_idx"]).copy(); sv["_idx"] = sv["_idx"].astype(int)
pos_idx = sv[sv["_gt"]]["_idx"].values; neg_idx = sv[~sv["_gt"]]["_idx"].values
pv, nv = full_vecs[pos_idx], full_vecs[neg_idx]
K = cfg.KNN_K
spp, snp = cosine_similarity(pv, pv), cosine_similarity(pv, nv)
spn, snn = cosine_similarity(nv, pv), cosine_similarity(nv, nv)
np.fill_diagonal(spp, -np.inf); np.fill_diagonal(snn, -np.inf)
loo = pd.DataFrame({
    "_gt": np.concatenate([np.ones(len(pos_idx), bool), np.zeros(len(neg_idx), bool)]),
    "pp": np.concatenate([np.sort(spp,1)[:,-K:].mean(1), np.sort(spn,1)[:,-K:].mean(1)]),
    "pn": np.concatenate([np.sort(snp,1)[:,-K:].mean(1), np.sort(snn,1)[:,-K:].mean(1)]),
})
loo["pred"] = loo["pp"] > loo["pn"]
tn2, fp2, fn2, tp2 = confusion_matrix(loo["_gt"], loo["pred"], labels=[False, True]).ravel()
P2 = precision_score(loo["_gt"], loo["pred"], zero_division=0); R2 = recall_score(loo["_gt"], loo["pred"], zero_division=0)
F12 = f1_score(loo["_gt"], loo["pred"], zero_division=0); SPEC2 = tn2/(tn2+fp2) if (tn2+fp2) else 0
flip = int((to_bool(df.loc[sv["_idx"], "is_fintech"]).values != loo["pred"].values).sum())
print(f"  [LOO]      P={P2:.3f} R={R2:.3f} F1={F12:.3f} Spec={SPEC2:.3f}  (예측변동 {flip}/{len(loo)} = {flip/len(loo)*100:.1f}%)")

# ── 4) 저장 (모두 계산값) ──────────────────────────────────────────────────
rows = [
    ["메타", "산출일자", datetime.now().strftime("%Y-%m-%d"), ""],
    ["메타", "모집단(패밀리정제 후)", f"{len(df):,}건", ""],
    ["메타", "진성", f"{n1:,}건", f"{n1/len(df)*100:.1f}%"],
    ["메타", "노이즈", f"{n2:,}건", f"{n2/len(df)*100:.1f}%"],
    ["메타", "시드/안티시드", f"{int(seed['_gt'].sum())}/{int((~seed['_gt']).sum())}", "final_seed.csv"],
    ["효과크기", "Cohen's d", f"{d:.4f}", f"95%CI[{ci[0]:.4f},{ci[1]:.4f}] ({interp(d)})"],
    ["효과크기", "Hedges g", f"{g:.4f}", f"보정 {corr:.4f}"],
    ["분포", "진성 평균 net", f"{m1:+.6f}", f"SD={sd1:.6f}"],
    ["분포", "노이즈 평균 net", f"{m2:+.6f}", f"SD={sd2:.6f}"],
    ["분포", "Welch t", f"{t_stat:.2f}", f"p={p_val:.2e}"],
    ["자기참조", "P/R/F1/Spec", f"{P:.3f}/{R:.3f}/{F1:.3f}/{SPEC:.3f}", f"TP/FP/FN/TN={tp}/{fp}/{fn}/{tn}"],
    ["LOO", "P/R/F1/Spec", f"{P2:.3f}/{R2:.3f}/{F12:.3f}/{SPEC2:.3f}", f"예측변동 {flip}건({flip/len(loo)*100:.1f}%)"],
]
out = pd.DataFrame(rows, columns=["Category", "Metric", "Value", "Detail"])
out.to_csv(cfg.P(cfg.VALIDATION_OUTPUT), index=False, encoding="utf-8-sig")
print(f"\n💾 저장: {cfg.VALIDATION_OUTPUT}")
print("\n★ 위 Cohen's d / F1 값을 위키 핵심수치레지스터 §2(v2)에 기록하세요 (워크플로우 §5).")
