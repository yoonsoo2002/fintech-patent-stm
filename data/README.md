# data/

**No data is included in this repository.** Patent records exported from The Lens are subject to its
terms of use, and the derived files (embeddings, document–term matrices, R workspaces) are several GB.

## How the data was obtained

- Source: The Lens (https://www.lens.org), Patent search.
- Query: fintech keyword expression over title / abstract (`code/queries/the lens 쿼리_v1.txt`).
- Jurisdictions: US, EP (EPO), CN, JP, KR. Origin country = first-priority country; only a literal `EP` priority counts as Europe (`EUROPE_MODE = "strict"`).
- Location: put the files here, or point the env var `FINTECH_DATA_DIR` at the folder that holds them.
- Filing date: 2019-01-01 to 2026-01-01. Legal status: Active + Pending.
  Exact queries: `code/queries/`.
- Export: CSV in three date ranges, merged by `code/python/00b_병합_검증.py` into `merged_3files.csv`.

## Expected files (produced by the pipeline, git-ignored)

| File | Produced by |
|---|---|
| `merged_3files.csv` | Lens exports merged (00b) |
| `fintech_patents_family_cleaned.csv`, `seed_patents_top600.csv` | 01 |
| `final_seed.csv` | Gemini labeling of the 600 seed candidates (157 fintech / 443 not) with the six-function prompt in `code/python/03b_시드분류_프롬프트.md` |
| `pos_seeds_vectors.npy`, `neg_seeds_vectors.npy`, `full_patents_vectors.npy` | 02 |
| `final_fintech_patents_filtered.csv`, `full_patents_with_scores.csv` | 02 |
| `Semantic_Validation_Metrics.csv` | 03 |
| `STM_full_workspace_Phase0.RData`, `topic_mapping.csv` | 04 |
