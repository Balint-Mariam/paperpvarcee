# Energy-Carbon Pressures, Financial Stress and Sovereign-Risk Repricing in Central and Eastern Europe

This repository is the clean external-facing replication package for the final CEE Structural Panel VAR paper workflow.

The package contains the final input workbook, reproducible R code, manuscript-ready outputs, robustness evidence, validation material and archived internal audit files. It excludes exploratory API-fetching scripts and earlier trial specifications from the main workflow.

## 1. Project Overview

The project studies how energy-carbon pressures, systemic financial stress, inflation and monetary conditions are transmitted to sovereign-risk repricing in Central and Eastern Europe.

The final empirical design combines:

- a seven-variable FE/LSDV Panel VAR(1) baseline;
- Driscoll-Kraay coefficient-level inference for the same FE/LSDV equations;
- robustness checks using PVAR-GMM(1), panel local projections with Driscoll-Kraay inference and pre-model diagnostics;
- a sign-restricted Structural PVAR layer;
- historical decomposition and counterfactual analysis.

## 2. Research Question

The paper asks whether energy-carbon shocks and financial stress amplify sovereign-risk repricing in CEE, and how much of the 2021Q1-2023Q4 sovereign-risk movement can be attributed to energy and sovereign-risk shocks.

## 3. Data and Sample

Main input:

- `data/raw/incercare v2.xlsx`

Final model-ready data:

- `outputs/01_model_ready_data/model_ready_dataset.xlsx`

Sample:

- 11 CEE countries
- quarterly panel
- 2014Q2-2025Q4
- 517 observations
- balanced panel

## 4. Final Model Architecture

Final variable order:

1. `Energy_Factor`
2. `d_CISS`
3. `d_CPI`
4. `GDP_Growth`
5. `d_3MRate`
6. `d_FiscalBalanceGDP`
7. `dlog_CDS`

Baseline reduced-form model:

- FE/LSDV Panel VAR(1)
- coefficients are FE/LSDV coefficients
- Driscoll-Kraay inference changes only standard errors, p-values and significance stars
- the dynamic matrix used for Structural PVAR, FEVD, HD and counterfactuals remains the FE/LSDV dynamic matrix

Final structural model:

- `Structural PVAR refined4 S1 - four-shock sign-only, h=0,1,2`
- shocks: energy-carbon pressure, systemic financial stress, inflationary monetary reaction, sovereign-risk repricing
- representative draw: candidate draw 23085 / accepted draw 5782

## 5. Repository Structure

```text
paperpvarcee/
  README.md
  LICENSE
  .gitignore
  .gitattributes
  data/
    raw/
    processed/
  code/
    00_install_packages.R
    00_master_pipeline_full_paper_handoff.R
    02_pre_model_diagnostics/
    04_robustness/
    05_structural_pvar/
    06_historical_decomposition/
    07_counterfactuals/
    08_tables_figures/
    09_validation/
  outputs/
    01_model_ready_data/
    02_tables/
    03_figures/
    04_reports/
    05_logs/
    06_replication_only/
  docs/
  archive/
```

## 6. How to Reproduce the Analysis

Install required packages:

```bash
Rscript code/00_install_packages.R
```

Recommended entry point:

```bash
Rscript code/00_master_pipeline_full_paper_handoff.R
```

Inspect the flags at the top of the master script before running. A full structural rebuild can take substantially longer because the structural sign-restriction stage uses 50,000 candidate rotations.

Detailed instructions are in:

- `docs/REPLICATION_GUIDE.md`

## 7. Main Outputs

Main paper tables:

- `outputs/02_tables/main_paper/`

Main paper figures:

- `outputs/03_figures/main_paper/`

Reports:

- `outputs/04_reports/empirical_results_summary.md`
- `outputs/04_reports/final_model_verdict.md`
- `outputs/04_reports/pre_model_diagnostics_cleanup_report.md`
- `outputs/02_tables/robustness/dk_inference/DK_inference_report.md`

## 8. Robustness and Validation

Robustness material:

- `outputs/02_tables/robustness/`
- `outputs/02_tables/appendix/MASTER_appendix_tables.xlsx`

Validation summary:

- `docs/VALIDATION_SUMMARY.md`

Full internal audit:

- `archive/internal_audit/methodological_code_audit/`

Audit result:

- 82 PASS
- 3 minor warnings
- 0 FAIL

## 9. Manuscript-Ready Tables and Figures

Use in the manuscript:

- polished main figures in `outputs/03_figures/main_paper/`
- polished appendix figures in `outputs/03_figures/appendix/`
- polished manuscript tables in `outputs/02_tables/main_paper/MASTER_polished_tables_for_manuscript.xlsx`
- appendix and robustness tables in `outputs/02_tables/appendix/` and `outputs/02_tables/robustness/`

Do not use as main manuscript outputs:

- old unpolished figures;
- exhaustive all-combinations figures;
- repaired4 or earlier trial variants;
- CF2, CF3, CF5 or CF7 as main counterfactual scenarios;
- raw internal audit workbooks.

The full selection guide is:

- `docs/MANUSCRIPT_OUTPUT_SELECTION.md`

## 10. Notes for Coauthors and Supervisors

Start with:

1. `docs/OUTPUT_ROADMAP.md`
2. `outputs/04_reports/empirical_results_summary.md`
3. `outputs/04_reports/final_model_verdict.md`
4. `docs/METHODOLOGY_OVERVIEW.md`
5. `docs/MANUSCRIPT_OUTPUT_SELECTION.md`

The repository is organized so external readers can inspect final results first and move to replication, robustness and audit material only if needed.
