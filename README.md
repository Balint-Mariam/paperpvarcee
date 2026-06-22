# paperpvarcee

Clean replication package for the final CEE Structural PVAR paper workflow.

## Contents

- `incercare v2.xlsx` - final Excel input used by the full 7-variable workflow.
- `00_install_packages.R` - helper script for installing required CRAN packages.
- `00_master_pipeline_full_paper.R` - unified master script for final consolidation.
- `15_structural_pvar_full7_final_workflow.R` - final data preparation, FE/LSDV PVAR, GMM robustness, LP robustness.
- `17_structural_pvar_full7_refined4.R` - final Structural PVAR refined4 S1 sign-restriction stage.
- `19_structural_pvar_full7_hd_refined4.R` - historical decomposition using the final representative draw.
- `20_structural_pvar_full7_counterfactual_refined4.R` - counterfactual analysis using the validated HD.
- `21_polish_q1_figures_tables.R` - final Q1 paper-ready figure/table polishing from existing outputs only.
- `22_pre_model_diagnostics_cleanup.R` - pre-model diagnostics cleanup from the final model-ready dataset only.
- `FINAL_Q1_PAPER_OUTPUTS/` - final paper-ready outputs only.

The final structural model is:

`Structural PVAR refined4 S1 - four-shock sign-only, h=0,1,2`

No repaired4, baseline trial, API-fetching, or exploratory intermediate folders are included.

## Final Outputs

Key files are in `FINAL_Q1_PAPER_OUTPUTS/`:

- `02_master_excel/MASTER_all_tables_for_paper.xlsx`
- `02_master_excel/MASTER_all_tables_for_paper_updated_diagnostics.xlsx`
- `02_master_excel/pre_model_diagnostics_cleaned.xlsx`
- `02_master_excel/MASTER_appendix_tables.xlsx`
- `02_master_excel/table_manifest.xlsx`
- `03_figures/figure_manifest.xlsx`
- `03_figures/main_paper/`
- `03_figures/main_paper_polished/`
- `03_figures/appendix/`
- `03_figures/appendix_polished/`
- `03_figures/exhaustive_all_combinations/`
- `03_figures/figure_manifest_polished.xlsx`
- `04_reports/`
- `05_logs/reproducibility_checks.txt`

Polished manuscript tables are in:

- `02_master_excel/MASTER_polished_tables_for_manuscript.xlsx`
- `02_master_excel/table_manifest_polished.xlsx`

Polished captions and the visual polishing report are in:

- `04_reports/paper_figure_captions_polished.md`
- `04_reports/paper_table_captions_polished.md`
- `04_reports/final_visual_polishing_report.md`
- `04_reports/pre_model_diagnostics_cleanup_report.md`

## Reproducing

To inspect the final committed outputs, open `FINAL_Q1_PAPER_OUTPUTS`.

Install the required R packages first:

```bash
Rscript 00_install_packages.R
```

In a clean clone, the master script detects that validated intermediate caches are absent and rebuilds the full workflow from the Excel input:

```bash
Rscript 00_master_pipeline_full_paper.R
```

You can also force a full rebuild explicitly:

```r
Sys.setenv(RUN_FROM_SCRATCH = "true")
Sys.setenv(USE_CACHED_INTERMEDIATE_OUTPUTS = "false")
source("00_master_pipeline_full_paper.R")
```

or from a shell:

```bash
RUN_FROM_SCRATCH=true USE_CACHED_INTERMEDIATE_OUTPUTS=false Rscript 00_master_pipeline_full_paper.R
```

On Windows PowerShell:

```powershell
$env:RUN_FROM_SCRATCH = "true"
$env:USE_CACHED_INTERMEDIATE_OUTPUTS = "false"
Rscript .\00_master_pipeline_full_paper.R
```

The full rebuild can take substantially longer than the final consolidation because the structural stage uses 50,000 candidate rotations.

To regenerate only the polished figures and manuscript tables from existing final outputs, without rerunning any model:

```bash
Rscript 21_polish_q1_figures_tables.R
```

To regenerate only the cleaned pre-model diagnostics from the final model-ready dataset, without rerunning any model:

```bash
Rscript 22_pre_model_diagnostics_cleanup.R
```

## Reproducibility Snapshot

The final committed run reports:

- countries: 11
- quarters: 47
- final sample: 2014Q2-2025Q4
- HD/CF effective sample: 2014Q3-2025Q4
- variable order: `Energy_Factor`, `d_CISS`, `d_CPI`, `GDP_Growth`, `d_3MRate`, `d_FiscalBalanceGDP`, `dlog_CDS`
- accepted rotations: 12,715
- acceptance rate: 25.43%
- representative draw: candidate draw 23085 / accepted draw 5782
- HD reconstruction error: 4.441e-16
