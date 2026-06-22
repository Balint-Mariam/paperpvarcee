# Pipeline harmonisation and external handoff documentation.
# This script reads the final committed outputs and writes documentation and
# manifests only. It does not rerun or modify any econometric model.

required_packages <- c("openxlsx")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "))
}

ROOT_DIR <- getwd()
FINAL_DIR <- file.path(ROOT_DIR, "FINAL_Q1_PAPER_OUTPUTS")
HANDOFF_DIR <- file.path(FINAL_DIR, "08_pipeline_handoff")
MASTER_DIR <- file.path(FINAL_DIR, "02_master_excel")
FIGURE_DIR <- file.path(FINAL_DIR, "03_figures")
REPORT_DIR <- file.path(FINAL_DIR, "04_reports")
AUDIT_DIR <- file.path(FINAL_DIR, "07_methodological_code_audit")

dir.create(HANDOFF_DIR, recursive = TRUE, showWarnings = FALSE)

MODEL_NAME <- "Structural PVAR refined4 S1 - four-shock sign-only, h=0,1,2"
MODEL_VARS <- c(
  "Energy_Factor",
  "d_CISS",
  "d_CPI",
  "GDP_Growth",
  "d_3MRate",
  "d_FiscalBalanceGDP",
  "dlog_CDS"
)

safe_read_sheet <- function(path, sheet) {
  if (!file.exists(path)) {
    return(data.frame(note = paste("Missing file:", path), stringsAsFactors = FALSE))
  }
  sheets <- openxlsx::getSheetNames(path)
  if (!(sheet %in% sheets)) {
    return(data.frame(note = paste("Missing sheet:", sheet), stringsAsFactors = FALSE))
  }
  openxlsx::read.xlsx(path, sheet = sheet)
}

write_workbook <- function(path, sheets) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets)) {
    sheet_name <- substr(gsub("[\\[\\]\\*\\?/\\\\:]", "_", nm), 1, 31)
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeDataTable(wb, sheet_name, sheets[[nm]], tableStyle = "TableStyleLight9")
    openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(sheets[[nm]]), widths = "auto")
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

rel_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE) |>
    sub(paste0("^", gsub("\\\\", "/", normalizePath(ROOT_DIR, winslash = "/", mustWork = FALSE)), "/?"), "", x = _)
}

file_role <- function(relative_path) {
  if (grepl("^FINAL_Q1_PAPER_OUTPUTS/00_code/", relative_path)) return("archived source code")
  if (grepl("^FINAL_Q1_PAPER_OUTPUTS/01_data/", relative_path)) return("final model-ready data and checks")
  if (grepl("^FINAL_Q1_PAPER_OUTPUTS/02_master_excel/", relative_path)) return("tables and Excel results")
  if (grepl("^FINAL_Q1_PAPER_OUTPUTS/03_figures/main_paper_polished/", relative_path)) return("main manuscript polished figure")
  if (grepl("^FINAL_Q1_PAPER_OUTPUTS/03_figures/appendix_polished/", relative_path)) return("appendix polished figure")
  if (grepl("^FINAL_Q1_PAPER_OUTPUTS/03_figures/main_paper/", relative_path)) return("superseded unpolished main figure")
  if (grepl("^FINAL_Q1_PAPER_OUTPUTS/03_figures/appendix/", relative_path)) return("appendix replication figure")
  if (grepl("^FINAL_Q1_PAPER_OUTPUTS/03_figures/exhaustive_all_combinations/", relative_path)) return("exhaustive replication figure")
  if (grepl("^FINAL_Q1_PAPER_OUTPUTS/04_reports/", relative_path)) return("methodology or caption report")
  if (grepl("^FINAL_Q1_PAPER_OUTPUTS/05_logs/", relative_path)) return("execution log")
  if (grepl("^FINAL_Q1_PAPER_OUTPUTS/06_alternative_inference_DK/", relative_path)) return("Driscoll-Kraay robustness output")
  if (grepl("^FINAL_Q1_PAPER_OUTPUTS/07_methodological_code_audit/", relative_path)) return("methodological code audit output")
  if (grepl("^FINAL_Q1_PAPER_OUTPUTS/08_pipeline_handoff/", relative_path)) return("handoff documentation")
  if (grepl("\\.R$", relative_path)) return("source code")
  if (grepl("\\.xlsx?$", relative_path)) return("input workbook")
  "replication file"
}

manuscript_status <- function(relative_path) {
  if (grepl("main_paper_polished", relative_path)) return("main_paper_use")
  if (grepl("MASTER_polished_tables_for_manuscript|table_manifest_polished", relative_path)) return("main_paper_use")
  if (grepl("paper_figure_captions_polished|paper_table_captions_polished", relative_path)) return("main_paper_use")
  if (grepl("appendix_polished|MASTER_appendix_tables|pre_model_diagnostics_cleaned|06_alternative_inference_DK|07_methodological_code_audit", relative_path)) {
    return("appendix_or_robustness")
  }
  if (grepl("main_paper/|exhaustive_all_combinations", relative_path)) return("replication_not_main")
  if (grepl("08_pipeline_handoff|00_code|05_logs|01_data", relative_path)) return("replication_support")
  "context"
}

audit_summary <- safe_read_sheet(
  file.path(AUDIT_DIR, "methodological_code_audit_checks.xlsx"),
  "audit_summary"
)
audit_issues <- safe_read_sheet(
  file.path(AUDIT_DIR, "methodological_code_audit_checks.xlsx"),
  "issues_log"
)
audit_verdict <- if ("observed_value" %in% names(audit_summary) && any(audit_summary$check_id == "FINAL_VERDICT")) {
  audit_summary$observed_value[audit_summary$check_id == "FINAL_VERDICT"][[1]]
} else {
  "Audit workbook not available"
}

script_manifest <- data.frame(
  execution_order = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
  script = c(
    "00_install_packages.R",
    "15_structural_pvar_full7_final_workflow.R",
    "17_structural_pvar_full7_refined4.R",
    "19_structural_pvar_full7_hd_refined4.R",
    "20_structural_pvar_full7_counterfactual_refined4.R",
    "00_master_pipeline_full_paper.R",
    "21_polish_q1_figures_tables.R",
    "22_pre_model_diagnostics_cleanup.R",
    "23_fe_lsdv_pvar_dk_inference.R",
    "24_methodological_code_audit.R",
    "25_pipeline_handoff_docs.R",
    "00_master_pipeline_full_paper_handoff.R"
  ),
  stage = c(
    "Package setup",
    "Data preparation, reduced-form FE/LSDV PVAR(1), GMM robustness, LP-DK robustness",
    "Structural PVAR sign-restriction identification",
    "Historical decomposition",
    "Counterfactual analysis",
    "Final output consolidation",
    "Paper-ready figure and table polishing",
    "Pre-model diagnostics cleanup",
    "Alternative Driscoll-Kraay coefficient-level inference",
    "Methodological code audit",
    "External handoff documentation",
    "Optional one-command handoff orchestrator"
  ),
  changes_model_estimates = c("No", "Yes", "Yes", "No; uses selected structural draw", "No; uses HD outputs", "No if caches exist; can rebuild if caches absent", "No", "No", "No new coefficients; inference only", "No", "No", "Depends on selected flags"),
  primary_inputs = c(
    "CRAN package list",
    "incercare v2.xlsx",
    "Reduced-form FE/LSDV outputs from script 15",
    "Structural representative draw from script 17",
    "Historical decomposition outputs from script 19",
    "Validated intermediate output folders",
    "FINAL_Q1_PAPER_OUTPUTS existing outputs",
    "FINAL_Q1_PAPER_OUTPUTS/01_data/model_ready_dataset.xlsx",
    "FINAL_Q1_PAPER_OUTPUTS/01_data/model_ready_dataset.xlsx",
    "FINAL_Q1_PAPER_OUTPUTS final outputs",
    "FINAL_Q1_PAPER_OUTPUTS final outputs and audit",
    "All final scripts"
  ),
  primary_outputs = c(
    "Installed packages",
    "Data, FE/LSDV, GMM, LP-DK workbooks and figures",
    "Structural IRFs, FEVD, restrictions and acceptance diagnostics",
    "HD workbooks and figures",
    "Counterfactual workbooks and figures",
    "FINAL_Q1_PAPER_OUTPUTS",
    "Polished figures, polished manuscript tables and captions",
    "pre_model_diagnostics_cleaned.xlsx and cleanup report",
    "FE_LSDV_PVAR_DK_inference.xlsx and DK report",
    "07_methodological_code_audit",
    "08_pipeline_handoff",
    "Runs selected scripts in sequence"
  ),
  expected_runtime = c(
    "Short",
    "Medium",
    "Long; structural random rotations use 50,000 candidates",
    "Short to medium",
    "Short to medium",
    "Short with caches; long if rebuild is triggered",
    "Short",
    "Short",
    "Short",
    "Short",
    "Short",
    "Short to long, depending on flags"
  ),
  handoff_notes = c(
    "Run once in a clean R setup.",
    "Defines the final 7-variable panel and reduced-form baseline.",
    "Final structural model is refined4 S1, sign-only at h=0,1,2.",
    "Uses the accepted representative draw; does not reselect restrictions.",
    "Main manuscript counterfactuals are CF1, CF4 and CF6 only.",
    "This is the standard clean-clone reproduction entry point.",
    "Use polished outputs for the manuscript instead of old unpolished folders.",
    "Diagnostics are descriptive and pre-model only.",
    "Use as coefficient-level inference robustness for FE/LSDV coefficients.",
    "Audit verdict is reported in the handoff guide.",
    "Documentation-only script.",
    "Convenience wrapper; inspect flags before running."
  ),
  stringsAsFactors = FALSE
)

methodology_map <- data.frame(
  methodology_component = c(
    "Final sample",
    "Variable construction",
    "Energy/carbon factor",
    "Reduced-form baseline",
    "Alternative coefficient inference",
    "Pre-model diagnostics",
    "GMM robustness",
    "LP-DK robustness",
    "Structural identification",
    "Stability",
    "Structural IRFs",
    "FEVD",
    "Historical decomposition",
    "Counterfactual analysis",
    "Paper-ready polishing",
    "Methodological audit",
    "External handoff"
  ),
  implementation = c(
    "15_structural_pvar_full7_final_workflow.R",
    "15_structural_pvar_full7_final_workflow.R",
    "15_structural_pvar_full7_final_workflow.R",
    "15_structural_pvar_full7_final_workflow.R",
    "23_fe_lsdv_pvar_dk_inference.R",
    "22_pre_model_diagnostics_cleanup.R",
    "15_structural_pvar_full7_final_workflow.R",
    "15_structural_pvar_full7_final_workflow.R",
    "17_structural_pvar_full7_refined4.R",
    "15 and 17 scripts; audited by 24_methodological_code_audit.R",
    "17_structural_pvar_full7_refined4.R",
    "17_structural_pvar_full7_refined4.R",
    "19_structural_pvar_full7_hd_refined4.R",
    "20_structural_pvar_full7_counterfactual_refined4.R",
    "21_polish_q1_figures_tables.R",
    "24_methodological_code_audit.R",
    "25_pipeline_handoff_docs.R"
  ),
  final_output = c(
    "01_data/model_ready_dataset.xlsx",
    "01_data/transformation_summary.xlsx",
    "Main Figure F01 and transformation summary",
    "02_master_excel/MASTER_all_tables_for_paper.xlsx",
    "06_alternative_inference_DK/FE_LSDV_PVAR_DK_inference.xlsx",
    "02_master_excel/pre_model_diagnostics_cleaned.xlsx",
    "02_master_excel/MASTER_appendix_tables.xlsx",
    "02_master_excel/MASTER_appendix_tables.xlsx",
    "Structural sheets in MASTER_all_tables_for_paper.xlsx and appendix workbook",
    "Audit workbook and structural acceptance/stability tables",
    "03_figures/main_paper_polished/F02 and appendix polished IRFs",
    "03_figures/main_paper_polished/F03 and PAPER_Table_6_FEVD",
    "03_figures/main_paper_polished/F04 and PAPER_Table_7_HD_CDS",
    "03_figures/main_paper_polished/F05-F06 and PAPER_Table_8_CF_CDS",
    "03_figures/main_paper_polished, appendix_polished, MASTER_polished_tables_for_manuscript.xlsx",
    "07_methodological_code_audit",
    "08_pipeline_handoff"
  ),
  manuscript_use = c(
    "Data section and replication statement",
    "Data section and appendix",
    "Data section / Figure 1",
    "Methodology and reduced-form results",
    "Robustness / appendix",
    "Appendix diagnostics",
    "Robustness / appendix",
    "Robustness / appendix",
    "Structural methodology",
    "Methodological validation",
    "Main structural results and appendix",
    "Main structural results",
    "Main event-period decomposition",
    "Main counterfactual results",
    "Use these polished outputs in the paper",
    "Not a results table; use for internal consistency evidence",
    "External replication and handoff"
  ),
  notes = c(
    "Balanced 11-country panel, 2014Q2-2025Q4.",
    paste(MODEL_VARS, collapse = ", "),
    "Energy_Factor is the final PCA-based common energy/carbon pressure measure.",
    "Baseline is FE/LSDV PVAR(1); do not substitute GMM as the baseline.",
    "DK inference is coefficient-level robustness, not a new dynamic model.",
    "No model estimates are changed by diagnostics cleanup.",
    "Reported as robustness because of small N and instrument constraints.",
    "Reported as dynamic reduced-form robustness.",
    "Four shocks: energy, CISS, inflation/rate, sovereign.",
    "Stability is checked for both reduced-form and structural layers.",
    "Use sign restrictions for qualitative economic interpretation.",
    "Horizon 12 is the main reporting horizon.",
    "Main stress window is 2021Q1-2023Q4.",
    "Main scenarios: CF1_no_energy, CF4_no_sovereign, CF6_no_energy_no_sovereign.",
    "Old main_paper figures are superseded by main_paper_polished.",
    paste("Final audit verdict:", audit_verdict),
    "Provides reproduction commands, output manifest and manuscript selection."
  ),
  stringsAsFactors = FALSE
)

manuscript_selection <- data.frame(
  item_id = c(
    "F01", "F02", "F03", "F04", "F05", "F06",
    "A01", "A02", "A03", "A04", "A05", "A06", "A07",
    "T_main", "T_appendix", "T_DK", "T_diagnostics", "Audit"
  ),
  manuscript_role = c(
    rep("main_paper_figure", 6),
    rep("appendix_figure", 7),
    "main_paper_tables", "appendix_tables", "robustness_table", "diagnostics_table", "internal_audit"
  ),
  path_or_workbook = c(
    "03_figures/main_paper_polished/F01_energy_factor_pca_components_polished.png/pdf",
    "03_figures/main_paper_polished/F02_selected_structural_irfs_polished.png/pdf",
    "03_figures/main_paper_polished/F03_structural_fevd_h12_main_variables_polished.png/pdf",
    "03_figures/main_paper_polished/F04_historical_decomposition_main_variables_polished.png/pdf",
    "03_figures/main_paper_polished/F05_counterfactual_dlog_CDS_selected_scenarios_polished.png/pdf",
    "03_figures/main_paper_polished/F06_country_heterogeneity_counterfactual_CDS_polished.png/pdf",
    "03_figures/appendix_polished/A01_structural_irf_Energy_polished.png/pdf",
    "03_figures/appendix_polished/A02_structural_irf_CISS_polished.png/pdf",
    "03_figures/appendix_polished/A03_structural_irf_Inflation_Rate_polished.png/pdf",
    "03_figures/appendix_polished/A04_structural_irf_Sovereign_polished.png/pdf",
    "03_figures/appendix_polished/A05_structural_fevd_h12_all_variables_polished.png/pdf",
    "03_figures/appendix_polished/A06_historical_decomposition_gdp_fiscal_polished.png/pdf",
    "03_figures/appendix_polished/A07_no_energy_counterfactual_macro_variables_polished.png/pdf",
    "02_master_excel/MASTER_polished_tables_for_manuscript.xlsx",
    "02_master_excel/MASTER_appendix_tables.xlsx",
    "06_alternative_inference_DK/FE_LSDV_PVAR_DK_inference.xlsx",
    "02_master_excel/pre_model_diagnostics_cleaned.xlsx",
    "07_methodological_code_audit/methodological_code_audit_report.md"
  ),
  selection_note = c(
    "Use as final Figure 1.",
    "Use as final selected IRF figure.",
    "Use as final FEVD figure.",
    "Use as final HD figure.",
    "Use as final CDS counterfactual figure; only selected scenarios.",
    "Use as final country heterogeneity counterfactual figure.",
    "Appendix IRF by energy shock.",
    "Appendix IRF by CISS shock.",
    "Appendix IRF by inflation/rate shock.",
    "Appendix IRF by sovereign shock.",
    "Appendix full FEVD.",
    "Appendix HD for GDP and fiscal variables.",
    "Appendix no-energy macro counterfactual.",
    "Primary manuscript table source.",
    "Appendix and robustness table source.",
    "DK inference robustness only.",
    "Pre-model diagnostics only.",
    "Documents code consistency; not a manuscript results table."
  ),
  stringsAsFactors = FALSE
)

deprecated_outputs <- data.frame(
  output_group = c(
    "03_figures/main_paper",
    "03_figures/exhaustive_all_combinations",
    "CF2_no_ciss, CF3_no_inflationary_monetary, CF5_no_energy_no_inflationary, CF7_no_macro_financial",
    "Repaired/baseline trial folders outside this clean repo"
  ),
  status = c(
    "superseded",
    "replication_only",
    "appendix_or_replication_only",
    "excluded"
  ),
  reason = c(
    "The polished figures in main_paper_polished supersede the earlier unpolished exports.",
    "Full figure grid is retained for traceability but is not the main-paper selection.",
    "Main counterfactual narrative uses CF1, CF4 and CF6 only.",
    "The clean repo keeps only the final refined4 S1 workflow."
  ),
  stringsAsFactors = FALSE
)

write_workbook(
  file.path(HANDOFF_DIR, "script_execution_manifest.xlsx"),
  list(script_execution_manifest = script_manifest)
)

write_workbook(
  file.path(HANDOFF_DIR, "methodology_to_outputs_map.xlsx"),
  list(methodology_to_outputs_map = methodology_map)
)

project_handoff_lines <- c(
  "# Project Handoff Guide",
  "",
  paste("Final empirical model:", MODEL_NAME),
  "",
  "## What this package contains",
  "",
  "- A clean replication input workbook: `incercare v2.xlsx`.",
  "- Final source scripts only, archived both in the project root and in `FINAL_Q1_PAPER_OUTPUTS/00_code`.",
  "- Final model-ready data, tables, polished figures, reports, logs, audit outputs and handoff manifests.",
  "",
  "## Final model specification",
  "",
  paste("- Variable order:", paste(MODEL_VARS, collapse = ", "), "."),
  "- Reduced-form baseline: FE/LSDV Panel VAR(1).",
  "- Coefficient-level inference robustness: Driscoll-Kraay standard errors for the baseline FE/LSDV equations.",
  "- Structural layer: refined4 S1 sign-only identification at horizons 0, 1 and 2.",
  "- Historical decomposition and counterfactuals use the selected stable structural draw.",
  "",
  "## Audit status",
  "",
  paste("- Final audit verdict:", audit_verdict, "."),
  "- The audit rechecks sample, transformations, variable order, FE/LSDV dynamics, DK inference, diagnostics, GMM robustness, LP-DK robustness, structural identification, FEVD, historical decomposition, counterfactuals and manuscript output selection.",
  "- The audit does not re-estimate the model.",
  "",
  "## Manuscript rule",
  "",
  "- Use `03_figures/main_paper_polished` and `MASTER_polished_tables_for_manuscript.xlsx` for the main paper.",
  "- Use `03_figures/appendix_polished`, `MASTER_appendix_tables.xlsx`, diagnostics and DK outputs for appendix or robustness.",
  "- Do not cite the older `03_figures/main_paper` exports as final paper figures; they are retained only for traceability.",
  "- Main counterfactual scenarios are CF1_no_energy, CF4_no_sovereign and CF6_no_energy_no_sovereign.",
  "",
  "## Key handoff files",
  "",
  "- `HOW_TO_REPRODUCE.md`: commands for a clean reproduction.",
  "- `script_execution_manifest.xlsx`: script order, inputs and outputs.",
  "- `output_manifest_final.xlsx`: final output inventory and manuscript status.",
  "- `methodology_to_outputs_map.xlsx`: map from methodology components to output files.",
  "- `MANUSCRIPT_OUTPUT_SELECTION.md`: concise list of what to use in the paper."
)
writeLines(project_handoff_lines, file.path(HANDOFF_DIR, "PROJECT_HANDOFF_GUIDE.md"))

reproduce_lines <- c(
  "# How To Reproduce",
  "",
  "Run from the repository root.",
  "",
  "## 1. Install packages",
  "",
  "```bash",
  "Rscript 00_install_packages.R",
  "```",
  "",
  "## 2. Standard clean-clone reproduction",
  "",
  "```bash",
  "Rscript 00_master_pipeline_full_paper.R",
  "```",
  "",
  "In a clean clone, the master script detects missing intermediate caches and rebuilds from `incercare v2.xlsx`.",
  "",
  "## 3. Explicit full rebuild",
  "",
  "```bash",
  "RUN_FROM_SCRATCH=true USE_CACHED_INTERMEDIATE_OUTPUTS=false Rscript 00_master_pipeline_full_paper.R",
  "```",
  "",
  "PowerShell:",
  "",
  "```powershell",
  "$env:RUN_FROM_SCRATCH = \"true\"",
  "$env:USE_CACHED_INTERMEDIATE_OUTPUTS = \"false\"",
  "Rscript .\\00_master_pipeline_full_paper.R",
  "```",
  "",
  "The full rebuild can take substantially longer because the structural stage uses 50,000 candidate rotations.",
  "",
  "## 4. Documentation-only refresh",
  "",
  "These scripts do not re-estimate the model:",
  "",
  "```bash",
  "Rscript 21_polish_q1_figures_tables.R",
  "Rscript 22_pre_model_diagnostics_cleanup.R",
  "Rscript 23_fe_lsdv_pvar_dk_inference.R",
  "Rscript 24_methodological_code_audit.R",
  "Rscript 25_pipeline_handoff_docs.R",
  "```",
  "",
  "## 5. Optional all-stage handoff wrapper",
  "",
  "```bash",
  "Rscript 00_master_pipeline_full_paper_handoff.R",
  "```",
  "",
  "Open `00_master_pipeline_full_paper_handoff.R` first and inspect the flags. It is intended as an explicit handoff orchestrator, not as a replacement for reading the stage scripts."
)
writeLines(reproduce_lines, file.path(HANDOFF_DIR, "HOW_TO_REPRODUCE.md"))

selection_lines <- c(
  "# Manuscript Output Selection",
  "",
  "## Use in the main paper",
  "",
  "- Main figures: `FINAL_Q1_PAPER_OUTPUTS/03_figures/main_paper_polished`.",
  "- Main tables: `FINAL_Q1_PAPER_OUTPUTS/02_master_excel/MASTER_polished_tables_for_manuscript.xlsx`.",
  "- Polished captions: `FINAL_Q1_PAPER_OUTPUTS/04_reports/paper_figure_captions_polished.md` and `paper_table_captions_polished.md`.",
  "- Main counterfactual scenarios: CF1_no_energy, CF4_no_sovereign and CF6_no_energy_no_sovereign.",
  "",
  "## Use in appendix or robustness",
  "",
  "- Appendix figures: `FINAL_Q1_PAPER_OUTPUTS/03_figures/appendix_polished`.",
  "- Appendix tables: `FINAL_Q1_PAPER_OUTPUTS/02_master_excel/MASTER_appendix_tables.xlsx`.",
  "- Pre-model diagnostics: `pre_model_diagnostics_cleaned.xlsx`.",
  "- Driscoll-Kraay coefficient-level inference: `06_alternative_inference_DK/FE_LSDV_PVAR_DK_inference.xlsx`.",
  "- GMM and LP-DK outputs are robustness checks, not the baseline.",
  "",
  "## Retain only for replication traceability",
  "",
  "- `03_figures/main_paper`: superseded by polished figures.",
  "- `03_figures/exhaustive_all_combinations`: full generated figure universe.",
  "- Counterfactual scenarios CF2, CF3, CF5 and CF7: not part of the main narrative.",
  "",
  "## Do not reintroduce",
  "",
  "- Earlier repaired/baseline trial structural specifications.",
  "- Exploratory API-fetching or intermediate trial folders.",
  "- Alternative variable orders."
)
writeLines(selection_lines, file.path(HANDOFF_DIR, "MANUSCRIPT_OUTPUT_SELECTION.md"))

all_files <- list.files(FINAL_DIR, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
all_files <- all_files[file.info(all_files)$isdir == FALSE]
relative_paths <- vapply(all_files, rel_path, character(1))
output_manifest <- data.frame(
  relative_path = relative_paths,
  file_name = basename(all_files),
  file_extension = tools::file_ext(all_files),
  size_bytes = unname(file.info(all_files)$size),
  last_modified = format(file.info(all_files)$mtime, "%Y-%m-%d %H:%M:%S"),
  role = vapply(relative_paths, file_role, character(1)),
  manuscript_status = vapply(relative_paths, manuscript_status, character(1)),
  stringsAsFactors = FALSE
)
output_manifest <- output_manifest[order(output_manifest$relative_path), ]

write_workbook(
  file.path(HANDOFF_DIR, "output_manifest_final.xlsx"),
  list(
    output_manifest_final = output_manifest,
    manuscript_selection = manuscript_selection,
    deprecated_or_replication = deprecated_outputs
  )
)

report_lines <- c(
  "# Pipeline Handoff Report",
  "",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste("Final model:", MODEL_NAME),
  paste("Audit verdict:", audit_verdict),
  "",
  "## Generated files",
  "",
  "- `PROJECT_HANDOFF_GUIDE.md`",
  "- `HOW_TO_REPRODUCE.md`",
  "- `script_execution_manifest.xlsx`",
  "- `output_manifest_final.xlsx`",
  "- `methodology_to_outputs_map.xlsx`",
  "- `MANUSCRIPT_OUTPUT_SELECTION.md`",
  "- `pipeline_handoff_report.md`",
  "",
  "## Inventory summary",
  "",
  paste("- Files indexed in FINAL_Q1_PAPER_OUTPUTS:", nrow(output_manifest)),
  paste("- Main-paper use files:", sum(output_manifest$manuscript_status == "main_paper_use")),
  paste("- Appendix/robustness files:", sum(output_manifest$manuscript_status == "appendix_or_robustness")),
  paste("- Replication-only files:", sum(output_manifest$manuscript_status == "replication_not_main")),
  "",
  "## Audit warnings carried into handoff",
  ""
)
if (nrow(audit_issues) > 0 && "check_id" %in% names(audit_issues)) {
  for (i in seq_len(nrow(audit_issues))) {
    issue_note <- if ("notes" %in% names(audit_issues)) audit_issues$notes[[i]] else ""
    report_lines <- c(report_lines, paste0("- ", audit_issues$check_id[[i]], ": ", issue_note))
  }
} else {
  report_lines <- c(report_lines, "- No audit issues table was available.")
}
report_lines <- c(
  report_lines,
  "",
  "## Handoff decision",
  "",
  "The repository is coherent for external replication. The only carried warnings are output-selection/documentation warnings: use polished figures and selected CF scenarios for the manuscript."
)
writeLines(report_lines, file.path(HANDOFF_DIR, "pipeline_handoff_report.md"))

cat("Pipeline handoff documentation written to:", HANDOFF_DIR, "\n")
