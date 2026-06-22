# Pre-Model Diagnostics Cleanup Report

This cleanup uses the final model-ready dataset only. It does not re-estimate the PVAR, alter the structural PVAR refined4 S1, change the historical decomposition, or change counterfactual results.

## 1. Were all seven final variables tested?

Yes. Tested variables: Energy_Factor, d_CISS, d_CPI, GDP_Growth, d_3MRate, d_FiscalBalanceGDP, dlog_CDS.

## 2. Was d_FiscalBalanceGDP included explicitly?

Yes. d_FiscalBalanceGDP is included in the panel unit-root tests, Pesaran CIPS/CADF, Pesaran CD test, summary workbook, updated master workbook, and final diagnostic verdict.

## 3. What stationarity tests were run?

For country-specific variables, the script reports LLC, IPS, Fisher ADF / Maddala-Wu, Fisher PP, and Pesaran CIPS/CADF. For common replicated variables, it reports ADF, PP, and KPSS time-series tests.

## 4. Which variables are stationary?

Stationarity is supported for: Energy_Factor, d_CISS, d_CPI, GDP_Growth, d_3MRate, d_FiscalBalanceGDP, dlog_CDS.

## 5. Is any variable problematic?

No variable is flagged as problematic by the cleaned pre-model diagnostics.

## 6. Was CIPS/CADF rerun successfully?

Yes. CIPS/CADF was rerun successfully for all five country-specific variables using pseries objects from a pdata.frame indexed by Country and quarter_index.

## 7. Why is CIPS/CADF not reported for some variables?

CIPS/CADF is not reported for Energy_Factor and d_CISS because these are common variables replicated across countries; treating them as independent panel series would be mechanically misleading.

## 8. Is there cross-sectional dependence?

Yes. Pesaran CD detects cross-sectional dependence for the country-specific variables: d_CPI, GDP_Growth, d_3MRate, d_FiscalBalanceGDP, dlog_CDS.

## 9. For which variables is Pesaran CD relevant?

Pesaran CD is relevant for country-specific variables: d_CPI, GDP_Growth, d_3MRate, d_FiscalBalanceGDP, dlog_CDS. It is marked not applicable for Energy_Factor and d_CISS because they are common replicated variables.

## 10. Suggested diagnostic wording for the paper

Pre-model diagnostics indicate that the transformed variables used in the PVAR system are stationary according to standard unit-root diagnostics. For the country-specific variables, LLC, IPS, Fisher ADF, Fisher PP and Pesaran CIPS/CADF tests reject the unit-root null at conventional levels. For the common replicated variables, ADF and PP tests reject unit roots, while KPSS tests do not reject level stationarity. Pesaran CD tests detect strong cross-sectional dependence for the country-specific macro-financial variables, which is expected in a CEE panel exposed to common European energy, inflation and financial-stress shocks. The empirical strategy therefore complements the baseline FE/LSDV PVAR with robustness checks based on PVAR-GMM and panel local projections with Driscoll-Kraay inference.

## Sample Used

- Countries: 11
- Quarters: 47 (2014Q2-2025Q4)
- Observations: 517
- Balanced panel: TRUE
- Missing values in final model variables: 0

## Output Files

- outputs/02_tables/robustness/pre_model_diagnostics_cleaned.xlsx
- outputs/02_tables/main_paper/MASTER_all_tables_for_paper_updated_diagnostics.xlsx
- outputs/04_reports/pre_model_diagnostics_cleanup_report.md
