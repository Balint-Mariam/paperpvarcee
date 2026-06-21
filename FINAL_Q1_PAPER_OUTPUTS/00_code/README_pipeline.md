# Unified Pipeline README

Run `00_master_pipeline_full_paper.R` from the project root.

Default behavior uses validated cached intermediate outputs and consolidates them into `FINAL_Q1_PAPER_OUTPUTS`.

For a full rerun, set environment variables before running R:

```r
Sys.setenv(RUN_FROM_SCRATCH = 'true')
Sys.setenv(USE_CACHED_INTERMEDIATE_OUTPUTS = 'false')
```

The full rerun can take substantially longer because the structural sign-restriction stage uses 50,000 candidate rotations.

Main stages: data construction, reduced-form FE/LSDV PVAR(1), robustness, structural refined4 S1, historical decomposition, counterfactual analysis, final consolidation.
