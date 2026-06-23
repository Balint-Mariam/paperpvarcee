# PVARX Common-Factor Robustness

## Specification

The endogenous block contains d_CPI, GDP_Growth, d_3MRate, d_FiscalBalanceGDP, and dlog_CDS. Energy_Factor and d_CISS enter as common factors. Country fixed effects are included. Time fixed effects are excluded because they would be collinear with the common factors.

The baseline specification includes current common factors. The extended specification includes current and one-quarter-lagged common factors. Driscoll-Kraay standard errors use lag 4, with lag 2 and lag 6 sensitivity checks.

## Data Checks

- Countries: 11.
- Source quarters: 47.
- Baseline estimation observations: 506.
- Energy_Factor values per quarter: exactly one in all quarters.
- d_CISS values per quarter: exactly one in all quarters.

## Baseline Key Channels

- Energy_Factor -> dlog_CDS: coefficient = 0.0132, DK p-value = 0.121, positive but statistically weak.
- d_CISS -> dlog_CDS: coefficient = 0.7768, DK p-value = <0.001, positive and statistically supported.
- d_CPI -> dlog_CDS: coefficient = 0.0137, DK p-value = 0.002, positive and statistically supported.
- Energy_Factor -> d_CPI: coefficient = 0.4158, DK p-value = <0.001, positive and statistically supported.
- Energy_Factor -> d_3MRate: coefficient = 0.0583, DK p-value = <0.001, positive and statistically supported.
- d_3MRate -> dlog_CDS: coefficient = 0.0300, DK p-value = 0.184, direct channel remains statistically weak.
- d_FiscalBalanceGDP -> dlog_CDS: coefficient = -0.0081, DK p-value = 0.237, direct channel remains statistically weak.

## Extended-Specification Key Channels

- Energy_Factor -> dlog_CDS: coefficient = 0.0120, DK p-value = 0.115, positive but statistically weak.
- d_CISS -> dlog_CDS: coefficient = 0.7201, DK p-value = <0.001, positive and statistically supported.
- d_CPI -> dlog_CDS: coefficient = 0.0073, DK p-value = 0.087, positive and statistically supported.
- Energy_Factor -> d_CPI: coefficient = 0.3930, DK p-value = <0.001, positive and statistically supported.
- Energy_Factor -> d_3MRate: coefficient = 0.0519, DK p-value = <0.001, positive and statistically supported.
- d_3MRate -> dlog_CDS: coefficient = 0.0291, DK p-value = 0.132, direct channel remains statistically weak.
- d_FiscalBalanceGDP -> dlog_CDS: coefficient = -0.0110, DK p-value = 0.072, direct channel is statistically informative.

## Stability

- baseline_current_factors: stable = TRUE, maximum root modulus = 0.6335.
- extended_current_and_lagged_factors: stable = TRUE, maximum root modulus = 0.6273.

## Joint Common-Factor Tests in the dlog_CDS Equation

- baseline_current_factors, Energy_Factor: tested terms = Energy_Factor, Wald p-value = 0.121, coefficient sum = 0.0132, sum p-value = 0.121.
- baseline_current_factors, d_CISS: tested terms = d_CISS, Wald p-value = <0.001, coefficient sum = 0.7768, sum p-value = <0.001.
- extended_current_and_lagged_factors, Energy_Factor: tested terms = Energy_Factor + L1_Energy_Factor, Wald p-value = 0.082, coefficient sum = 0.0219, sum p-value = 0.026.
- extended_current_and_lagged_factors, d_CISS: tested terms = d_CISS + L1_d_CISS, Wald p-value = <0.001, coefficient sum = 1.2406, sum p-value = <0.001.

## Common-Factor Shock Scaling

- Energy_Factor: time-series standard deviation = 1.5218.
- d_CISS: time-series standard deviation = 0.0843.

## dlog_CDS Dynamic Multipliers

- baseline_current_factors, Energy_Factor, h=0: 0.02010
- baseline_current_factors, Energy_Factor, h=1: 0.00563
- baseline_current_factors, Energy_Factor, h=4: 0.00173
- baseline_current_factors, Energy_Factor, h=12: 0.00006
- baseline_current_factors, d_CISS, h=0: 0.06551
- baseline_current_factors, d_CISS, h=1: -0.00266
- baseline_current_factors, d_CISS, h=4: 0.00136
- baseline_current_factors, d_CISS, h=12: 0.00003
- extended_current_and_lagged_factors, Energy_Factor, h=0: 0.01830
- extended_current_and_lagged_factors, Energy_Factor, h=1: 0.01319
- extended_current_and_lagged_factors, Energy_Factor, h=4: 0.00037
- extended_current_and_lagged_factors, Energy_Factor, h=12: 0.00002
- extended_current_and_lagged_factors, d_CISS, h=0: 0.06073
- extended_current_and_lagged_factors, d_CISS, h=1: 0.03404
- extended_current_and_lagged_factors, d_CISS, h=4: 0.00242
- extended_current_and_lagged_factors, d_CISS, h=12: 0.00005

## Methodological Scope

The estimates are conditional on treating the current common factors as exogenous. The extended specification allows lagged common-factor effects but does not by itself establish causal exogeneity. Dynamic multipliers are reduced-form PVARX responses, not structural shocks from a seven-equation endogenous system.

## Files

- Workbook: `outputs/02_tables/robustness/pvarx_common_factors/PVARX_common_factors_results.xlsx`.
- Figures: `outputs/03_figures/appendix/pvarx_common_factors/`.
- Run log: `outputs/05_logs/PVARX_common_factors_run_log.txt`.
