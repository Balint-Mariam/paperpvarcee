# Replication Guide

## R Version and Packages

The workflow was developed and validated in R with the package set installed by:

```bash
Rscript code/00_install_packages.R
```

Core packages include `readxl`, `openxlsx`, `dplyr`, `tidyr`, `tibble`, `plm`, `lmtest`, `sandwich`, `tseries`, `moments`, `ggplot2`, `scales`, `panelvar`, `parallel`, `corrplot`, `patchwork` and `ragg`.

## Input File

Main input:

- `data/raw/incercare v2.xlsx`

## Full Pipeline

Recommended command from the repository root:

```bash
Rscript code/00_master_pipeline_full_paper_handoff.R
```

Inspect the flags at the top of the script before running. The wrapper calls each stage in a separate Rscript process.

## Selected Stages

Package setup:

```bash
Rscript code/00_install_packages.R
```

Final data, reduced-form baseline, GMM robustness and LP-DK robustness:

```bash
Rscript code/05_structural_pvar/15_structural_pvar_full7_final_workflow.R
```

Structural PVAR:

```bash
Rscript code/05_structural_pvar/17_structural_pvar_full7_refined4.R
```

Historical decomposition:

```bash
Rscript code/06_historical_decomposition/19_structural_pvar_full7_hd_refined4.R
```

Counterfactual analysis:

```bash
Rscript code/07_counterfactuals/20_structural_pvar_full7_counterfactual_refined4.R
```

Polished tables and figures:

```bash
Rscript code/08_tables_figures/21_polish_q1_figures_tables.R
```

Diagnostics, DK inference and validation:

```bash
Rscript code/02_pre_model_diagnostics/22_pre_model_diagnostics_cleanup.R
Rscript code/04_robustness/23_fe_lsdv_pvar_dk_inference.R
Rscript code/09_validation/24_methodological_code_audit.R
Rscript code/09_validation/25_pipeline_handoff_docs.R
```

## Runtime Expectations

- Package setup: short.
- Data, FE/LSDV, GMM and LP-DK workflow: medium.
- Structural PVAR: potentially long because the final sign-restriction stage uses 50,000 candidate rotations.
- Historical decomposition and counterfactual analysis: short to medium.
- Polishing, diagnostics, DK inference and validation: short.

## Output Locations

- Model-ready data: `outputs/01_model_ready_data/`
- Main tables: `outputs/02_tables/main_paper/`
- Appendix and robustness tables: `outputs/02_tables/appendix/` and `outputs/02_tables/robustness/`
- Main figures: `outputs/03_figures/main_paper/`
- Appendix figures: `outputs/03_figures/appendix/`
- Reports: `outputs/04_reports/`
- Logs: `outputs/05_logs/`
- Internal audit: `archive/internal_audit/`

## Verify Success

Check:

1. `outputs/05_logs/reproducibility_checks.txt`
2. `outputs/04_reports/final_model_verdict.md`
3. `docs/VALIDATION_SUMMARY.md`
4. `docs/SCRIPT_MANIFEST.xlsx`
5. `docs/OUTPUT_MANIFEST.xlsx`

## Interpreting Warnings

The final audit has 82 PASS checks, 3 minor warnings and 0 FAIL checks. The warnings concern output selection and documentation only. They do not indicate a methodological failure.

## What Not to Rerun Unless Necessary

The full structural stage should not be rerun casually because it is the expensive stage and the final representative draw is already documented. Rerun it only if the input data or model specification is intentionally changed.
