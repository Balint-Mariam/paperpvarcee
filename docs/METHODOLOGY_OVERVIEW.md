# Methodology Overview

## Data

The final dataset is a quarterly balanced panel of 11 Central and Eastern European countries.

- Sample: 2014Q2-2025Q4
- Frequency: quarterly
- Observations: 517
- Panel status: balanced
- Main input file: `data/raw/incercare v2.xlsx`
- Final model-ready file: `outputs/01_model_ready_data/model_ready_dataset.xlsx`

## Variables

Final variable order:

1. `Energy_Factor`
2. `d_CISS`
3. `d_CPI`
4. `GDP_Growth`
5. `d_3MRate`
6. `d_FiscalBalanceGDP`
7. `dlog_CDS`

`Energy_Factor` is the common energy-carbon pressure factor constructed from the transformed energy and carbon variables. `d_CISS` captures changes in systemic financial stress. `dlog_CDS` is the sovereign-risk repricing measure used in the final baseline.

## Baseline Reduced-Form Model

The baseline reduced-form model is a FE/LSDV Panel VAR(1).

The reported coefficient estimates are FE/LSDV estimates. Driscoll-Kraay inference is applied at the coefficient level to account for heteroskedasticity, serial correlation and cross-sectional dependence. DK inference changes standard errors, p-values and significance markers only. It does not change the FE/LSDV coefficients or the dynamic matrix used downstream.

## Robustness

Robustness evidence includes:

- PVAR-GMM(1);
- panel local projections with Driscoll-Kraay inference;
- Driscoll-Kraay sensitivity lags 2, 4 and 6;
- pre-model diagnostics with CIPS/CADF stationarity checks.

GMM and LP-DK results are robustness checks, not replacements for the baseline FE/LSDV PVAR(1).

## Structural Layer

The final structural layer is:

`Structural PVAR refined4 S1 - four-shock sign-only, h=0,1,2`

The four structural shocks are:

1. Energy-carbon pressure shock
2. Systemic financial stress shock
3. Inflationary monetary-reaction shock
4. Sovereign-risk repricing shock

The sign restrictions are imposed at horizons 0, 1 and 2. The historical decomposition and counterfactual analysis use the selected representative accepted draw. The accepted structural draw is tied to the FE/LSDV dynamic matrix, not to the DK standard-error layer.

## Historical Decomposition

Historical decomposition is computed over the full effective sample. The manuscript interpretation focuses on 2021Q1-2023Q4, the main stress window used to discuss sovereign-risk repricing.

## Counterfactual Analysis

Main manuscript scenarios:

- No Energy: `CF1_no_energy`
- No Sovereign: `CF4_no_sovereign`
- No Energy + No Sovereign: `CF6_no_energy_no_sovereign`

The following scenarios are retained only for appendix or replication traceability and should not be used as main manuscript scenarios:

- `CF2_no_ciss`
- `CF3_no_inflationary_monetary`
- `CF5_no_energy_no_inflationary`
- `CF7_no_macro_financial`
