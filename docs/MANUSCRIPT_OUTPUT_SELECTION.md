# Manuscript Output Selection

## Use in Main Manuscript

### Figures

Use `outputs/03_figures/main_paper/`.

1. Energy factor / PCA components
2. Selected structural IRFs
3. Structural FEVD h12
4. Historical decomposition main variables
5. Counterfactual `dlog_CDS` selected scenarios
6. Country heterogeneity counterfactual effects

### Tables

Use `outputs/02_tables/main_paper/MASTER_polished_tables_for_manuscript.xlsx`.

1. Variables, definitions and transformations
2. Descriptive statistics and PCA
3. FE/LSDV PVAR(1) with Driscoll-Kraay standard errors
4. Robustness comparison
5. Sign restrictions
6. Structural FEVD h12
7. Historical decomposition summary
8. Counterfactual CDS effects
9. Country heterogeneity, optional

## Use in Appendix or Robustness Sections

- Appendix tables: `outputs/02_tables/appendix/MASTER_appendix_tables.xlsx`
- Robustness tables: `outputs/02_tables/robustness/`
- Appendix figures: `outputs/03_figures/appendix/`
- Full diagnostics: `outputs/02_tables/robustness/pre_model_diagnostics_cleaned.xlsx`

## Do Not Use in Main Manuscript

- old unpolished figures;
- old reduced-form heatmap;
- exhaustive all-combinations figures;
- repaired4;
- baseline3;
- `CF2_no_ciss`;
- `CF3_no_inflationary_monetary`;
- `CF5_no_energy_no_inflationary`;
- `CF7_no_macro_financial`;
- raw diagnostics tables;
- internal audit workbooks.

These materials are retained in `outputs/06_replication_only/` or `archive/` for traceability only.
