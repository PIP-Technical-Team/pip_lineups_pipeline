# PIP Lineups Pipeline

## Setup

1. **Set the version** in `pipeline-run.R` by editing the `version` variable (e.g., `"20260324_2021_01_02_PROD"`). The PPP year is extracted automatically from this string.

2. **Set paths**: Ensure `version_path` and `dir_dist_stats` point to valid directories on your machine.

3. **Configure flags** at the top of the script:
   - `use_csum_fst` — Use cumulative-sum FST workflow (`TRUE`) or the standard write/load workflow (`FALSE`).
   - `update_dist_stats` — Rebuild and save distribution statistics.
   - `update_subset_dist_stats` — Merge new dist stats with existing ones (requires a prior `LD_dist_stats.fst`).
   - `big_grp_est` — Run the large grouped estimation step (slow; usually `FALSE`).

## Running

Source the script end-to-end:

```r
source("pipeline-run.R")
```

The script will:

1. Load packages (`fastverse`, `qs`, `joyn`) and source helper files (`init.R`, `pipdata_pip_*.R`).
2. Read the reference-year estimation file and auxiliary data.
3. Write lineup distributions for all years in `lineup_years` (1981–2025).
4. Compute and save distribution statistics (`lineup_dist_stats.fst`).
5. Prepare and write the final reference-year and lineup-year tables used downstream by the PIP API pipeline.

## Key Outputs

| File | Location |
|---|---|
| `lineup_dist_stats.fst` | `<version_path>/estimations/` and `<dir_dist_stats>/` |
| `prod_refy_estimation.fst` | `<version_path>/estimations/` |
| `lineup_years.fst` | `<version_path>/estimations/` |
| Per-country lineup data | `<version_path>/lineup_data/` |
