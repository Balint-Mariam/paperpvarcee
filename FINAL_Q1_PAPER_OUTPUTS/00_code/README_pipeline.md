# Unified Pipeline README

Run `00_master_pipeline_full_paper.R` from the project root.

Default behavior uses validated cached intermediate outputs when they exist and consolidates them into `FINAL_Q1_PAPER_OUTPUTS`.

In a clean clone, those intermediate folders are intentionally not tracked. The master script detects that and rebuilds the full workflow from `incercare v2.xlsx`.

Install required packages first:

```r
source("00_install_packages.R")
```

For a full rerun, set environment variables before running R:

```r
Sys.setenv(RUN_FROM_SCRATCH = 'true')
Sys.setenv(USE_CACHED_INTERMEDIATE_OUTPUTS = 'false')
```

The full rerun can take substantially longer because the structural sign-restriction stage uses 50,000 candidate rotations.

Main stages: data construction, reduced-form FE/LSDV PVAR(1), robustness, structural refined4 S1, historical decomposition, counterfactual analysis, final consolidation.

Optional post-consolidation scripts:

- `21_polish_q1_figures_tables.R` regenerates polished figures and manuscript tables from existing final outputs only.
- `22_pre_model_diagnostics_cleanup.R` regenerates the cleaned pre-model diagnostics from the final model-ready dataset only.
- `23_fe_lsdv_pvar_dk_inference.R` regenerates alternative Driscoll-Kraay coefficient-level inference for the baseline FE/LSDV PVAR only.
- `24_methodological_code_audit.R` regenerates the methodological code audit from existing final outputs only.
- `25_pipeline_handoff_docs.R` regenerates the external handoff guide, output manifest, script manifest and methodology map from existing final outputs only.

Optional external handoff wrapper:

- `00_master_pipeline_full_paper_handoff.R` runs the final stage scripts in sequence through explicit flags. Inspect the flags before running because the full structural rebuild can take a long time.

Manuscript selection rule:

- Use `03_figures/main_paper_polished`, `03_figures/appendix_polished`, `MASTER_polished_tables_for_manuscript.xlsx` and `MASTER_appendix_tables.xlsx`.
- Treat `03_figures/main_paper` and `03_figures/exhaustive_all_combinations` as replication traceability outputs, not final manuscript figures.
- Main counterfactual scenarios are `CF1_no_energy`, `CF4_no_sovereign` and `CF6_no_energy_no_sovereign`.
