# Final Visual Polishing Report

## 1. Figures Redone
- F01 energy-carbon factor was rebuilt as a two-panel factor/loadings figure.
- F02 was rebuilt as a selected 2x3 structural IRF figure.
- F03 was rebuilt as a horizon-12 FEVD stacked bar chart for the main macro-financial variables.
- F04 combines historical decompositions for dlog_CDS, d_CPI and d_3MRate.
- F05 combines the three main dlog_CDS counterfactual scenarios.
- F06 combines country heterogeneity rankings for the three main counterfactual scenarios.

## 2. Figures Combined
- Separate HD figures for dlog_CDS, d_CPI and d_3MRate were combined into F04.
- Separate CF1, CF4 and CF6 counterfactual figures were combined into F05.
- Separate country ranking figures were combined into F06.

## 3. Figures Removed From Main Paper
- The original reduced-form heatmap is not recommended for the main paper in its previous form.
- The original dlog_CDS-only FEVD figure is replaced by F03.
- Separate counterfactual, HD and ranking figures are replaced by combined panels.

## 4. Recommended Main Manuscript Tables
- Table 1: Variables, definitions and transformations.
- Table 2: Descriptive statistics and PCA loadings.
- Table 3: Full FE/LSDV PVAR(1) coefficient matrix.
- Table 4: Robustness comparison.
- Table 5: Structural sign restrictions.
- Table 6: Structural FEVD at h=12 for main variables.
- Table 7: Historical decomposition summary.
- Table 8: Counterfactual CDS effects.
- Table 9: Country heterogeneity in CDS counterfactuals, optional if Figure 6 is retained.

## 5. Appendix / Replication Package
- Exhaustive structural IRFs, all-variable FEVD, macro/fiscal HD and additional counterfactuals remain appendix or replication material.
- Diagnostics and full exhaustive outputs should remain outside the main manuscript unless requested by reviewers.

## 6. Visual Style
- All polished figures use a neutral colorblind-friendly palette.
- Shock labels are shortened consistently: Energy, CISS, Inflation/Rate, Sovereign, Other and Initial.
- Counterfactual labels are shortened consistently: Actual, Fitted, No Energy, No Sovereign and No Energy + No Sovereign.
- Figures use ggplot2 with the custom theme_q1 and are exported through ragg/cairo.

## 7. Export Check
- Main polished figures exported: 6 PNG and PDF pairs.
- Appendix polished figures exported: 7 PNG and PDF pairs.

## 8. Remaining Visual Issues
- No model-result changes were made.
- Manual manuscript placement should check journal column width and whether Table 3 is too large for the main text.

## 9. Final Recommended Main Paper Figures
1. F01_energy_factor_pca_components_polished
2. F02_selected_structural_irfs_polished
3. F03_structural_fevd_h12_main_variables_polished
4. F04_historical_decomposition_main_variables_polished
5. F05_counterfactual_dlog_CDS_selected_scenarios_polished
6. F06_country_heterogeneity_counterfactual_CDS_polished

## 10. Manual Checks Before Submit
- Verify figure sizing after insertion into the manuscript template.
- Check whether Table 3 should move to the appendix if the journal imposes strict table-length limits.
- Confirm that captions match final manuscript numbering.
- Confirm that the shaded 2021Q1-2023Q4 episode is described consistently in the text.
