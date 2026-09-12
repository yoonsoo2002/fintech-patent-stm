# fintech-patent-stm

## What this is

An analysis pipeline for tracing the technological evolution of IP5 fintech patents (2019–2025) using PaECTER-embedding-based semantic filtering followed by Structural Topic Modeling (STM) with covariates.
The resulting work received an Encouragement Award (장려상) at the 2026 KSIE (Korean Society of Industrial and Systems Engineering) national student project competition.
This repository contains the code and the research-decision records only; raw patent data is not included (see Data).

## Pipeline

1. **Data collection** — patent records exported from The Lens (title/abstract keyword query, IP5 jurisdictions + EPO member states, filing date 2019–2025), merged and family-deduplicated (`python/v2/00b_병합_검증.py`, `01_정제_시드후보.py`).
2. **Seed selection** — top-200 most-cited patents per period (3 periods × 200 = 600 candidates), labeled fintech / non-fintech by an external LLM using the prompt in `python/v2/03b_시드분류_프롬프트.md`.
3. **PaECTER embedding** — title + abstract embedded with `mpi-inno-comp/paecter` (`02_임베딩_필터링.py`, GPU).
4. **Semantic filtering** — each patent scored by mean cosine similarity to its top-3 nearest positive seeds minus top-3 nearest negative seeds; patents with a positive net score are retained. Filter quality checked with Cohen's d and F1 (`03_필터링검증.py`).
5. **Preprocessing + STM** — K = 20 topics, spectral init, prevalence `~ Origin_Country + s(Publication.Year) + C_adj_i` (country, year spline, time-adjusted citations), 1 % lower vocabulary threshold (`R/v2/04_STM_학습.R`).
6. **Interpretation** — country-gap effects with BH-FDR correction, topic clustering, B-spline momentum for a technology-life-cycle matrix, and Gompertz-fit technology maturity ratio (TMR) (`R/v2/05`–`07`, `R/Gompertz_Diagnostics.R`, `R/TLC_BSpline_Momentum.R`).

`R/Fintech_Project.R` is the original (v1) monolithic script; `R/v2/` and `python/v2/` are the refactored, config-driven version used for the European-data re-run.

## How Claude is used

Research design, methodology decisions, and code iteration were carried out through conversations with Claude (Projects).
The resulting decisions, rejected alternatives, and hand-off notes are recorded in `CLAUDE.md` and `docs/`, so each new session starts from the full decision history (`docs/methodology-decisions.md` shows the B-1 / B-2 / B-3 loops as they actually happened).
The human owns the research questions, method choices, and validation of every number; Claude owns the repetitive execution, refactoring, and bookkeeping.

## Data

Raw patent data is not included for licensing reasons.
Records were exported from The Lens (lens.org) with a fintech title/abstract keyword query, jurisdictions US / EP / CN / JP / KR plus major EPO member states, filing dates 2019-01-01 to 2025-12-31, and legal status Active or Pending.
See `data/README.md` for the expected file names and how to reproduce the export.

## How to run

R packages: `stm`, `tidyverse` (dplyr, tidyr, stringr, ggplot2), `tidytext`, `stopwords`, `splines`, `scales`, `ggrepel`, `patchwork`, `fmsb`, `boot`, `igraph`, `ggraph`, `tidygraph`, `here`.
Python packages: `pandas`, `numpy`, `torch`, `transformers`, `scikit-learn`, `scipy`, `tqdm`, `matplotlib`, `seaborn`.

Run from the repository root with raw CSVs placed in `data/`:

```
python python/v2/00b_병합_검증.py      # merge Lens exports, sanity checks
python python/v2/01_정제_시드후보.py    # family cleaning + seed candidates
# -> label seed candidates with an LLM using python/v2/03b_시드분류_프롬프트.md -> data/final_seed.csv
python python/v2/02_임베딩_필터링.py    # PaECTER embedding + semantic filter (GPU)
python python/v2/03_필터링검증.py       # Cohen's d / F1 validation
Rscript R/v2/04_STM_학습.R             # STM K=20 -> writes topic_mapping template
# -> fill data/topic_mapping.csv (topic labels + clusters)
Rscript R/v2/05_분석_국가갭_클러스터.R  # country effects (BH-FDR), clusters
Rscript R/v2/06_TLC_모멘텀.R           # B-spline momentum, TLC matrix
Rscript R/v2/07_Gompertz_TMR.R         # Gompertz TMR
python python/v2/08_EDA_그림.py         # EDA figures
```

All paths are set in `python/v2/00_config.py` and `R/v2/00_config.R`.

## Team

Yoonsoo Cho — Lead Researcher, with co-authors at Kyung Hee University.
