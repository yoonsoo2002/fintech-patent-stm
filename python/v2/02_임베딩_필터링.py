"""
================================================================================
 02_임베딩_필터링.py  —  [단계 4] PaECTER 임베딩 + K-NN 코사인 시멘틱 필터링
================================================================================
 ★ GPU 필수 (Colab T4 기준 7만건 ~20-60분). CPU는 비권장.
 입력 : fintech_patents_family_cleaned.csv (01 산출)
        final_seed.csv (외부 LLM 분류본 — fintech 컬럼 TRUE/FALSE/OUT, _text 필수)
 출력 : final_fintech_patents_filtered.csv  (★진성만 → STM 입력)
        full_patents_with_scores.csv         (전체 + 점수 + is_fintech)
        pos/neg/full_patents_vectors.npy      (재임베딩 산출 — 기존 v1 npy는 폐기!)

 v1(embedding.py 블록7)을 충실히 옮김. 모델·K·풀링([CLS])·결합법 전부 동일.
================================================================================
"""
import os, sys
import numpy as np
import pandas as pd
import torch
from transformers import AutoTokenizer, AutoModel
from sklearn.metrics.pairwise import cosine_similarity
from tqdm import tqdm

import importlib.util as _u
_spec = _u.spec_from_file_location("cfg", os.path.join(os.path.dirname(__file__) if "__file__" in globals() else ".", "00_config.py"))
cfg = _u.module_from_spec(_spec); _spec.loader.exec_module(cfg)

if cfg.USE_COLAB:
    from google.colab import drive
    drive.mount("/content/drive")

# ── 모델 ────────────────────────────────────────────────────────────────────
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
print(f"장치: {device}  ({'⚠️ CPU — 매우 느림' if device.type=='cpu' else 'GPU OK'})")
tokenizer = AutoTokenizer.from_pretrained(cfg.PAECTER_MODEL)
model = AutoModel.from_pretrained(cfg.PAECTER_MODEL).to(device)
model.eval()

def get_embeddings(text_list, batch_size, desc):
    out = []
    text_list = [str(t) if pd.notna(t) else "" for t in text_list]
    for i in tqdm(range(0, len(text_list), batch_size), desc=desc):
        batch = text_list[i:i+batch_size]
        inputs = tokenizer(batch, padding=True, truncation=True,
                           max_length=cfg.MAX_LENGTH, return_tensors="pt").to(device)
        with torch.no_grad():
            outputs = model(**inputs)
            emb = outputs.last_hidden_state[:, 0, :].cpu().numpy()  # [CLS] 풀링
            out.append(emb)
    return np.vstack(out)

# ── 1) 시드/안티시드 로드·분리·임베딩 ──────────────────────────────────────
print("\n[1] 시드 로드/임베딩")
seed_df = pd.read_csv(cfg.P(cfg.FINAL_SEED))
seed_df["_f"] = seed_df["fintech"].astype(str).str.strip().str.upper()
pos = seed_df[seed_df["_f"] == "TRUE"].copy()
neg = seed_df[seed_df["_f"].isin(["FALSE", "OUT"])].copy()
print(f"  진성 시드 {len(pos)} / 안티시드 {len(neg)}  (개수는 자동결정 — v1 157/443에 안 맞춰도 됨)")

if "_text" not in pos.columns:
    raise ValueError("final_seed.csv 에 _text 컬럼이 필요합니다 (Title [SEP] Abstract).")
pos_vecs = get_embeddings(pos["_text"].tolist(), cfg.BATCH_SEED, "Positive")
neg_vecs = get_embeddings(neg["_text"].tolist(), cfg.BATCH_SEED, "Negative")

# ── 2) 전체 임베딩 ─────────────────────────────────────────────────────────
print("\n[2] 전체 임베딩")
full_df = pd.read_csv(cfg.P(cfg.FAMILY_CLEANED), low_memory=False)
if "_text" not in full_df.columns:
    full_df["_text"] = full_df[cfg.COL_TITLE].fillna("") + cfg.TEXT_SEP + full_df[cfg.COL_ABSTRACT].fillna("")
full_vecs = get_embeddings(full_df["_text"].tolist(), cfg.BATCH_FULL, "Full")
print(f"  임베딩 차원: {full_vecs.shape[1]}  (PaECTER는 보통 1024)")

# ── 3) Top-K 코사인 경쟁 → is_fintech ──────────────────────────────────────
print(f"\n[3] Top-{cfg.KNN_K} 평균 코사인 필터링")
K = cfg.KNN_K
sim_pos = cosine_similarity(full_vecs, pos_vecs)
sim_neg = cosine_similarity(full_vecs, neg_vecs)
full_df["top_k_pos_avg"] = np.sort(sim_pos, axis=1)[:, -K:].mean(axis=1)
full_df["top_k_neg_avg"] = np.sort(sim_neg, axis=1)[:, -K:].mean(axis=1)
full_df["is_fintech"]    = full_df["top_k_pos_avg"] > full_df["top_k_neg_avg"]

n_true = int(full_df["is_fintech"].sum())
print(f"  전체 {len(full_df):,} → 진성 {n_true:,} ({n_true/len(full_df)*100:.1f}%)  "
      f"| 노이즈 {len(full_df)-n_true:,}")
print("  진성 Origin 분포:")
print(full_df.loc[full_df['is_fintech'], 'Origin_Country'].value_counts())

# ── 4) 저장 ────────────────────────────────────────────────────────────────
full_df[full_df["is_fintech"]].to_csv(cfg.P(cfg.FILTERED_OUTPUT), index=False, encoding="utf-8-sig")
full_df.to_csv(cfg.P(cfg.SCORES_OUTPUT), index=False, encoding="utf-8-sig")
np.save(cfg.P(cfg.POS_VECS_NPY), pos_vecs)
np.save(cfg.P(cfg.NEG_VECS_NPY), neg_vecs)
np.save(cfg.P(cfg.FULL_VECS_NPY), full_vecs)
print(f"\n💾 저장 완료: {cfg.FILTERED_OUTPUT}, {cfg.SCORES_OUTPUT}, 3×npy")
print("  ⚠️ 이전 v1 .npy 는 반드시 폐기(이 파일들로 교체). R STM 입력 = final_fintech_patents_filtered.csv")
