# CLAUDE.md — fintech-patent-stm

> Note: the original research folder did not carry a project-level CLAUDE.md; the working
> rules lived in `0_유럽데이터_워크플로우/README_먼저읽기.md` and the hand-off notes under
> `4_보고서/작업이력/`. This file restates those rules so a Claude Code session starts with
> the same context. Full decision history: `stages/`.

## Project

IP5 fintech patents (2019–2025) → PaECTER embedding → semantic filter (K-NN top-3, pos − neg) →
STM (K = 20, prevalence `~ Origin_Country + s(Publication.Year) + C_adj_i`) → country gap / TLC / Gompertz TMR.
The paper uses the data and results reported at KSIE 2026 (71,663 families → 21,381 fintech patents). Config defaults reproduce them.

## Working rules

- Read `stages/01_v1_국내학회_KSIE/` (files 02–08) before touching any method. B-1 (filter validation), B-2 (TLC momentum), B-3 (Gompertz TMR) record what was tried, what was rejected, and why.
- Edit only `code/python/00_config.py` and `code/R/00_config.R` for paths, country mode, years, K. Do not hard-code values in numbered scripts.
- Decision gates (must be settled before a run): ① EUROPE_MODE (aggregate / strict / exclude), ② reference country (US), ③ seed parameters (TOP_N = 200 per period), ④ PaECTER + K-NN K = 3 and STM K = 20 — keep fixed.
- Every reported number must come from a script output file, never typed by hand. If a value cannot be recomputed, mark it as missing rather than estimating.
- Run order: `01 → 02 → (LLM seed labeling) → 02 → 03 → 04 → topic_mapping.csv → 05 → 06 → 07 → 08`. Python steps on GPU (Colab or local), R steps in RStudio / Rscript.
- `code/legacy_v1/` is reference-only v1 code; never run or edit it as part of the pipeline.
- Never commit raw data, embeddings, `.RData`, or anything under `data/` except `data/README.md`.

## Division of labor

Human: research questions, method selection, interpretation, final verification of every number.
Claude: running and iterating the pipeline, refactoring, figure generation, keeping the decision log up to date.
