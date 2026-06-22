# Project Handoff Guide

Final empirical model: Structural PVAR refined4 S1 - four-shock sign-only, h=0,1,2

## What this package contains

- A clean replication input workbook: `incercare v2.xlsx`.
- Final source scripts only, archived both in the project root and in `FINAL_Q1_PAPER_OUTPUTS/00_code`.
- Final model-ready data, tables, polished figures, reports, logs, audit outputs and handoff manifests.

## Final model specification

- Variable order: Energy_Factor, d_CISS, d_CPI, GDP_Growth, d_3MRate, d_FiscalBalanceGDP, dlog_CDS .
- Reduced-form baseline: FE/LSDV Panel VAR(1).
- Coefficient-level inference robustness: Driscoll-Kraay standard errors for the baseline FE/LSDV equations.
- Structural layer: refined4 S1 sign-only identification at horizons 0, 1 and 2.
- Historical decomposition and counterfactuals use the selected stable structural draw.

## Audit status

- Final audit verdict: PASS WITH MINOR WARNINGS - pipeline is coherent, but minor documentation fixes are needed. .
- The audit rechecks sample, transformations, variable order, FE/LSDV dynamics, DK inference, diagnostics, GMM robustness, LP-DK robustness, structural identification, FEVD, historical decomposition, counterfactuals and manuscript output selection.
- The audit does not re-estimate the model.

## Manuscript rule

- Use `03_figures/main_paper_polished` and `MASTER_polished_tables_for_manuscript.xlsx` for the main paper.
- Use `03_figures/appendix_polished`, `MASTER_appendix_tables.xlsx`, diagnostics and DK outputs for appendix or robustness.
- Do not cite the older `03_figures/main_paper` exports as final paper figures; they are retained only for traceability.
- Main counterfactual scenarios are CF1_no_energy, CF4_no_sovereign and CF6_no_energy_no_sovereign.

## Key handoff files

- `HOW_TO_REPRODUCE.md`: commands for a clean reproduction.
- `script_execution_manifest.xlsx`: script order, inputs and outputs.
- `output_manifest_final.xlsx`: final output inventory and manuscript status.
- `methodology_to_outputs_map.xlsx`: map from methodology components to output files.
- `MANUSCRIPT_OUTPUT_SELECTION.md`: concise list of what to use in the paper.
