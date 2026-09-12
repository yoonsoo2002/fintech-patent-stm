# data/

**No data is included in this repository.** Patent records exported from The Lens are subject to its
terms of use, and the derived files (embeddings, document–term matrices, R workspaces) are several GB.

## How the data was obtained

- Source: The Lens (https://www.lens.org), Patent search.
- Query: fintech keyword expression over title / abstract (identical between v1 and v2).
- Jurisdictions: US, EP, CN, JP, KR (v1); v2 adds major EPO member-state offices (DE, FR, GB, …), which the
  pipeline collapses into a single `EP` block (`EUROPE_MODE = "aggregate"` in `python/v2/00_config.py`).
- Filing date: 2019-01-01 to 2025-12-31. Legal status: Active + Pending.
- Export: CSV in three date ranges (`merged-v2-2019-2021.csv`, `merged-v2-2022-2023.csv`, `merged-v2-2024-2026.csv`),
  then merged by `python/v2/00b_병합_검증.py` into `merged_3files.csv`.

## Expected files (produced by the pipeline, git-ignored)

| File | Produced by |
|---|---|
| `merged_3files.csv` | Lens exports merged (00b) |
| `fintech_patents_family_cleaned.csv`, `seed_patents_top600.csv` | 01 |
| `final_seed.csv` | external LLM labeling of seed candidates |
| `pos_seeds_vectors.npy`, `neg_seeds_vectors.npy`, `full_patents_vectors.npy` | 02 |
| `final_fintech_patents_filtered.csv`, `full_patents_with_scores.csv` | 02 |
| `Semantic_Validation_Metrics.csv` | 03 |
| `STM_full_workspace_Phase0.RData`, `topic_mapping.csv` | 04 |
