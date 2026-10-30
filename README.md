# fintech-patent-stm

## What this is

An analysis pipeline for tracing the technological evolution of IP5 fintech patents (2019–2025) using PaECTER-embedding-based semantic filtering followed by Structural Topic Modeling (STM) with covariates.
The resulting work received an Encouragement Award (장려상) at the 2026 KSIE (Korean Society of Industrial and Systems Engineering) national student project competition.
This repository contains the code and the research-decision records only; raw patent data is not included (see Data).

## Pipeline

1. **Data collection** — patent records exported from The Lens (title/abstract keyword query, IP5 jurisdictions, filing date 2019-01-01 to 2026-01-01), merged and family-deduplicated (`code/python/00b_병합_검증.py`, `01_정제_시드후보.py`).
2. **Seed selection** — top-200 most-cited patents per period (3 periods × 200 = 600 candidates), labeled fintech / non-fintech (157 / 443) with Gemini using the researcher's Merton six-function prompt (`code/python/03b_시드분류_프롬프트.md`).
3. **PaECTER embedding** — title + abstract embedded with `mpi-inno-comp/paecter` (`02_임베딩_필터링.py`, GPU).
4. **Semantic filtering** — each patent scored by mean cosine similarity to its top-3 nearest positive seeds minus top-3 nearest negative seeds; patents with a positive net score are retained. Filter quality checked with Cohen's d and F1 (`03_필터링검증.py`).
5. **Preprocessing + STM** — K = 20 topics, spectral init, prevalence `~ Origin_Country + s(Publication.Year) + C_adj_i` (country, year spline, time-adjusted citations), 1 % lower vocabulary threshold (`code/R/04_STM_학습.R`).
6. **Interpretation** — country-gap effects with BH-FDR correction, topic clustering, B-spline momentum for a technology-life-cycle matrix, and Gompertz-fit technology maturity ratio (TMR) (`code/R/05`–`07`; noise-topic check in `code/R/09_노이즈STM_검증.R`).

`code/python/` and `code/R/` hold the config-driven pipeline. Its defaults reproduce the paper (v1 data: 71,663 families → 21,381 fintech patents); this was checked against the v1 outputs on 2026-10-30. The original v1 scripts are kept for reference only in `code/legacy_v1/` (not runnable).

## How Claude is used

Research design, methodology decisions, and code iteration were carried out through conversations with Claude (Projects).
The resulting decisions, rejected alternatives, and hand-off notes are recorded in `CLAUDE.md` and `stages/`, so each new session starts from the full decision history (`stages/01_v1_국내학회_KSIE/` shows the B-1 / B-2 / B-3 loops as they actually happened; `stages/03_최종제출_준비/` records the final pre-submission check).
The human owns the research questions, method choices, and validation of every number; Claude owns the repetitive execution, refactoring, and bookkeeping.

## Data

Raw patent data is not included for licensing reasons.
Records were exported from The Lens (lens.org) with a fintech title/abstract keyword query, jurisdictions US / EP / CN / JP / KR, filing dates 2019-01-01 to 2026-01-01, and legal status Active or Pending. The exact query is `code/queries/the lens 쿼리_v1.txt`.
See `data/README.md` for the expected file names and how to reproduce the export.

## How to run

R packages: `stm`, `tm`, `SnowballC`, `tidyverse` (dplyr, tidyr, stringr, ggplot2), `tidytext`, `stopwords`, `splines`, `scales`, `ggrepel`, `patchwork`, `fmsb`, `boot`, `igraph`, `ggraph`, `tidygraph` (versions: `code/R/sessionInfo.txt`).
Python packages: see `requirements.txt`.

Run from the repository root. Data files go in `data/`, or set `FINTECH_DATA_DIR` to the folder that holds them (both configs read it):

```
python code/python/00b_병합_검증.py      # merge Lens exports, sanity checks
python code/python/01_정제_시드후보.py    # family cleaning + seed candidates
# -> label the 600 seed candidates with Gemini using code/python/03b_시드분류_프롬프트.md -> data/final_seed.csv
python code/python/02_임베딩_필터링.py    # PaECTER embedding + semantic filter (GPU)
python code/python/03_필터링검증.py       # Cohen's d / F1 validation (+ confusion counts)
python code/python/03c_K민감도.py         # K-NN K = 1..9 sensitivity, Jaccard vs K = 3
python code/python/03d_기준점민감도.py     # net-score cutoff sensitivity
Rscript code/R/04_STM_학습.R             # STM K=20 -> writes topic_mapping template
# -> fill data/topic_mapping.csv (topic labels + clusters)
Rscript code/R/05_분석_국가갭_클러스터.R  # country effects (BH-FDR), clusters
Rscript code/R/06_TLC_모멘텀.R           # B-spline momentum, TLC matrix
Rscript code/R/07_Gompertz_TMR.R         # Gompertz TMR
Rscript code/R/09_노이즈STM_검증.R       # STM on the removed (noise) patents, qualitative check
Rscript code/R/10_본문그림_재현.R        # remaining paper figures/tables (dendrogram, scatter, network, heatmap, radar)
Rscript code/R/11_국가효과_표준오차.R    # country-effect standard errors (EP power limitation)
python code/python/08_EDA_그림.py         # EDA figures
```

All paths are set in `code/python/00_config.py` and `code/R/00_config.R`. Figure ↔ script map: `results/README.md`.

## Team

Yoonsoo Cho — Lead Researcher, with co-authors at Kyung Hee University.
