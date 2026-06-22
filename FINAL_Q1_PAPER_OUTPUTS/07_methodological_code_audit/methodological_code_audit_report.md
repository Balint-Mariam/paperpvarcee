# Methodological Code Audit Report

## Executive summary

Final verdict: PASS WITH MINOR WARNINGS - pipeline is coherent, but minor documentation fixes are needed.
Total checks: 85; PASS: 82; WARNING: 3; FAIL: 0.
This audit reads existing final outputs only and does not re-estimate the PVAR, Structural PVAR, historical decomposition or counterfactual analysis.

## Data and sample audit

Checks: 10; PASS: 10; WARNING: 0; FAIL: 0.

No issues detected.

## Variable construction audit

Checks: 9; PASS: 9; WARNING: 0; FAIL: 0.

No issues detected.

## Variable order audit

Checks: 10; PASS: 9; WARNING: 1; FAIL: 0.

- C_21_polish_q1_figures_tables.R: WARNING - Polishing script uses output-driven variable ordering and labels; source model ordering is audited in model scripts.

## FE/LSDV PVAR audit

Checks: 5; PASS: 5; WARNING: 0; FAIL: 0.

No issues detected.

## Driscoll-Kraay inference audit

Checks: 6; PASS: 6; WARNING: 0; FAIL: 0.

No issues detected.

## Pre-model diagnostics audit

Checks: 7; PASS: 7; WARNING: 0; FAIL: 0.

No issues detected.

## GMM robustness audit

Checks: 6; PASS: 6; WARNING: 0; FAIL: 0.

No issues detected.

## LP-DK robustness audit

Checks: 3; PASS: 3; WARNING: 0; FAIL: 0.

No issues detected.

## Structural PVAR audit

Checks: 10; PASS: 10; WARNING: 0; FAIL: 0.

No issues detected.

## FEVD audit

Checks: 3; PASS: 3; WARNING: 0; FAIL: 0.

No issues detected.

## Historical Decomposition audit

Checks: 4; PASS: 4; WARNING: 0; FAIL: 0.

No issues detected.

## Counterfactual audit

Checks: 6; PASS: 5; WARNING: 1; FAIL: 0.

- L02: WARNING - The raw scenario table marks CF2 as main, but polished manuscript selection and handoff documentation should keep only CF1, CF4 and CF6 in the main paper.

## Figures and tables audit

Checks: 6; PASS: 5; WARNING: 1; FAIL: 0.

- M06: WARNING - Unpolished figures remain in the replication package but should not be used as main-paper figures.

## Critical issues found

No critical methodological implementation issues were detected.

## Minor issues found

- M06 [WARNING]: Old unpolished main figures are superseded by polished selection. Unpolished figures remain in the replication package but should not be used as main-paper figures.
- C_21_polish_q1_figures_tables.R [WARNING]: MODEL_VARS order in 21_polish_q1_figures_tables.R. Polishing script uses output-driven variable ordering and labels; source model ordering is audited in model scripts.
- L02 [WARNING]: CF2/CF3/CF5/CF7 are not promoted to main-paper polished outputs. The raw scenario table marks CF2 as main, but polished manuscript selection and handoff documentation should keep only CF1, CF4 and CF6 in the main paper.

## Recommendations before manuscript writing

- Use FE/LSDV coefficients with Driscoll-Kraay coefficient-level inference for reduced-form Table 3.
- Use polished figures and polished table workbook for the manuscript.
- Treat GMM and LP-DK as robustness evidence, not as the structural baseline.
- Keep Structural PVAR, HD and counterfactual interpretation tied to the stable FE/LSDV dynamic matrix.

## Final verdict

PASS WITH MINOR WARNINGS - pipeline is coherent, but minor documentation fixes are needed.
