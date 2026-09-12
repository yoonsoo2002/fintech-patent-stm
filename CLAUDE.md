# CLAUDE.md — fintech-patent-stm

> Note: the original research folder did not carry a project-level CLAUDE.md; the working
> rules lived in `0_유럽데이터_워크플로우/README_먼저읽기.md` and the hand-off notes under
> `4_보고서/작업이력/`. This file restates those rules so a Claude Code session starts with
> the same context. Full decision history: `docs/methodology-decisions.md`, `docs/workflow.md`.

## Project

IP5 fintech patents (2019–2025) → PaECTER embedding → semantic filter (K-NN top-3, pos − neg) →
STM (K = 20, prevalence `~ Origin_Country + s(Publication.Year) + C_adj_i`) → country gap / TLC / Gompertz TMR.
Reference baseline = the v1 paper (KSIE 2026). v2 changes only the data (Europe aggregated as one EP block) and figures.

## Working rules

- Read `docs/methodology-decisions.md` before touching any method. B-1 (filter validation), B-2 (TLC momentum), B-3 (Gompertz TMR) record what was tried, what was rejected, and why.
- Edit only `python/v2/00_config.py` and `R/v2/00_config.R` for paths, country mode, years, K. Do not hard-code values in numbered scripts.
- Decision gates (must be settled before a run): ① EUROPE_MODE (aggregate / strict / exclude), ② reference country (US), ③ seed parameters (TOP_N = 200 per period), ④ PaECTER + K-NN K = 3 and STM K = 20 — keep fixed.
- Every reported number must come from a script output file, never typed by hand. If a value cannot be recomputed, mark it as missing rather than estimating.
- Run order: `01 → 02 → (LLM seed labeling) → 02 → 03 → 04 → topic_mapping.csv → 05 → 06 → 07 → 08`. Python steps on GPU (Colab or local), R steps in RStudio / Rscript.
- Validate each step against `docs/workflow.md` (단계별 검증 체크리스트) before moving on.
- Never commit raw data, embeddings, `.RData`, or anything under `data/` except `data/README.md`.

## Division of labor

Human: research questions, method selection, interpretation, final verification of every number.
Claude: running and iterating the pipeline, refactoring, figure generation, keeping the decision log up to date.
