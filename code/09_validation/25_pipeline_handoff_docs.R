# Clean-release handoff documentation and manifests.
# This script reads the external-facing repository structure only. It does not
# rerun models and does not change empirical tables or figures.

required_packages <- c("openxlsx")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "))
}

ROOT_DIR <- getwd()
DOCS_DIR <- file.path(ROOT_DIR, "docs")
OUTPUTS_DIR <- file.path(ROOT_DIR, "outputs")
CODE_DIR <- file.path(ROOT_DIR, "code")
ARCHIVE_DIR <- file.path(ROOT_DIR, "archive")

dir.create(DOCS_DIR, recursive = TRUE, showWarnings = FALSE)

MODEL_NAME <- "Structural PVAR refined4 S1 - four-shock sign-only, h=0,1,2"
MODEL_VARS <- c("Energy_Factor", "d_CISS", "d_CPI", "GDP_Growth", "d_3MRate", "d_FiscalBalanceGDP", "dlog_CDS")

rel_path <- function(path) {
  root <- normalizePath(ROOT_DIR, winslash = "/", mustWork = FALSE)
  x <- normalizePath(path, winslash = "/", mustWork = FALSE)
  sub(paste0("^", root, "/?"), "", x)
}

write_workbook <- function(path, sheets) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets)) {
    sheet_name <- substr(gsub("[\\[\\]\\*\\?/\\\\:]", "_", nm), 1, 31)
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeDataTable(wb, sheet_name, sheets[[nm]], tableStyle = "TableStyleLight9")
    openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(sheets[[nm]])), widths = "auto")
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

file_role <- function(path) {
  if (grepl("^outputs/01_model_ready_data/", path)) return("model-ready data")
  if (grepl("^outputs/02_tables/main_paper/", path)) return("main paper table workbook")
  if (grepl("^outputs/02_tables/appendix/", path)) return("appendix table workbook")
  if (grepl("^outputs/02_tables/robustness/", path)) return("robustness table/report")
  if (grepl("^outputs/03_figures/main_paper/", path)) return("main paper figure")
  if (grepl("^outputs/03_figures/appendix/", path)) return("appendix figure")
  if (grepl("^outputs/04_reports/", path)) return("external report/caption")
  if (grepl("^outputs/05_logs/", path)) return("replication log")
  if (grepl("^outputs/06_replication_only/", path)) return("replication only")
  if (grepl("^docs/", path)) return("external documentation")
  if (grepl("^archive/", path)) return("archive/internal")
  if (grepl("^code/", path)) return("source code")
  if (grepl("^data/raw/", path)) return("raw input data")
  "repository file"
}

manuscript_status <- function(path) {
  if (grepl("^outputs/02_tables/main_paper/", path)) return("main")
  if (grepl("^outputs/03_figures/main_paper/", path)) return("main")
  if (grepl("^outputs/02_tables/appendix/", path)) return("appendix")
  if (grepl("^outputs/03_figures/appendix/", path)) return("appendix")
  if (grepl("^outputs/02_tables/robustness/", path)) return("robustness")
  if (grepl("^outputs/06_replication_only/|^archive/", path)) return("archive")
  if (grepl("^outputs/04_reports/|^docs/", path)) return("documentation")
  if (grepl("^outputs/05_logs/", path)) return("replication")
  "support"
}

script_manifest <- data.frame(
  order = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
  script = c(
    "code/00_install_packages.R",
    "code/00_master_pipeline_full_paper_handoff.R",
    "code/05_structural_pvar/15_structural_pvar_full7_final_workflow.R",
    "code/02_pre_model_diagnostics/22_pre_model_diagnostics_cleanup.R",
    "code/04_robustness/23_fe_lsdv_pvar_dk_inference.R",
    "code/05_structural_pvar/17_structural_pvar_full7_refined4.R",
    "code/06_historical_decomposition/19_structural_pvar_full7_hd_refined4.R",
    "code/07_counterfactuals/20_structural_pvar_full7_counterfactual_refined4.R",
    "code/08_tables_figures/21_polish_q1_figures_tables.R",
    "code/09_validation/24_methodological_code_audit.R"
  ),
  purpose = c(
    "Install required R packages",
    "Recommended root-run pipeline orchestrator",
    "Build data, transformations, baseline FE/LSDV PVAR, GMM and LP-DK robustness",
    "Regenerate cleaned pre-model diagnostics from model-ready data",
    "Regenerate DK coefficient-level inference for FE/LSDV equations",
    "Run final sign-restricted Structural PVAR refined4 S1",
    "Run historical decomposition from selected structural draw",
    "Run counterfactual analysis from HD contributions",
    "Regenerate polished paper tables and figures",
    "Regenerate internal methodological audit"
  ),
  primary_input = c(
    "CRAN package list",
    "All final scripts",
    "data/raw/incercare v2.xlsx",
    "outputs/01_model_ready_data/model_ready_dataset.xlsx",
    "outputs/01_model_ready_data/model_ready_dataset.xlsx",
    "Reduced-form FE/LSDV outputs from script 15",
    "Structural outputs from script 17",
    "HD outputs from script 19",
    "Existing final outputs",
    "outputs/ and code/"
  ),
  primary_output = c(
    "Installed packages",
    "Full reproducible run",
    "Intermediate model output folders and final workbooks",
    "outputs/02_tables/robustness/pre_model_diagnostics_cleaned.xlsx",
    "outputs/02_tables/robustness/dk_inference/",
    "Structural IRFs, FEVD, B matrix and acceptance diagnostics",
    "HD tables and figures",
    "Counterfactual tables and figures",
    "outputs/02_tables/main_paper and outputs/03_figures/main_paper",
    "archive/internal_audit/methodological_code_audit"
  ),
  status = c("setup", "recommended_master", "model_estimation", "diagnostic", "robustness", "structural", "structural", "structural", "presentation", "validation"),
  notes = c(
    "Run once before reproduction.",
    "Inspect flags before running; full rebuild can be long.",
    "PVAR-GMM(2) is intentionally not part of the final workflow.",
    "Does not change model estimates.",
    "Changes standard errors and p-values only, not FE/LSDV coefficients.",
    "Final four-shock sign-only identification.",
    "Uses representative accepted draw.",
    "Main scenarios are CF1, CF4 and CF6.",
    "No econometric rerun.",
    "Internal consistency check; not a manuscript table."
  ),
  stringsAsFactors = FALSE
)

output_files <- list.files(c(OUTPUTS_DIR, DOCS_DIR, ARCHIVE_DIR, CODE_DIR, file.path(ROOT_DIR, "data")), recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
output_files <- output_files[file.info(output_files)$isdir == FALSE]
output_paths <- vapply(output_files, rel_path, character(1))
keep <- !grepl("^archive/old_outputs/local_untracked_intermediate_outputs/", output_paths)
output_files <- output_files[keep]
output_paths <- output_paths[keep]
output_manifest <- data.frame(
  relative_path = output_paths,
  file_name = basename(output_files),
  extension = tools::file_ext(output_files),
  size_bytes = unname(file.info(output_files)$size),
  last_modified = format(file.info(output_files)$mtime, "%Y-%m-%d %H:%M:%S"),
  role = vapply(output_paths, file_role, character(1)),
  manuscript_status = vapply(output_paths, manuscript_status, character(1)),
  stringsAsFactors = FALSE
)
output_manifest <- output_manifest[order(output_manifest$relative_path), ]

table_figure_manifest <- rbind(
  data.frame(
    item_type = "main_table",
    relative_path = list.files(file.path(OUTPUTS_DIR, "02_tables", "main_paper"), full.names = TRUE, recursive = TRUE) |>
      vapply(rel_path, character(1)),
    role = "Use in main manuscript",
    stringsAsFactors = FALSE
  ),
  data.frame(
    item_type = "appendix_table",
    relative_path = list.files(file.path(OUTPUTS_DIR, "02_tables", "appendix"), full.names = TRUE, recursive = TRUE) |>
      vapply(rel_path, character(1)),
    role = "Use in appendix",
    stringsAsFactors = FALSE
  ),
  data.frame(
    item_type = "robustness_table",
    relative_path = list.files(file.path(OUTPUTS_DIR, "02_tables", "robustness"), full.names = TRUE, recursive = TRUE) |>
      vapply(rel_path, character(1)),
    role = "Use for robustness or diagnostics",
    stringsAsFactors = FALSE
  ),
  data.frame(
    item_type = "main_figure",
    relative_path = list.files(file.path(OUTPUTS_DIR, "03_figures", "main_paper"), full.names = TRUE, recursive = TRUE) |>
      vapply(rel_path, character(1)),
    role = "Use in main manuscript",
    stringsAsFactors = FALSE
  ),
  data.frame(
    item_type = "appendix_figure",
    relative_path = list.files(file.path(OUTPUTS_DIR, "03_figures", "appendix"), full.names = TRUE, recursive = TRUE) |>
      vapply(rel_path, character(1)),
    role = "Use in appendix",
    stringsAsFactors = FALSE
  )
)

write_workbook(file.path(DOCS_DIR, "SCRIPT_MANIFEST.xlsx"), list(script_manifest = script_manifest))
write_workbook(file.path(DOCS_DIR, "OUTPUT_MANIFEST.xlsx"), list(output_manifest = output_manifest))
write_workbook(file.path(DOCS_DIR, "TABLE_FIGURE_MANIFEST.xlsx"), list(table_figure_manifest = table_figure_manifest))

summary_lines <- c(
  "# Clean Release Manifest Report",
  "",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste("Final model:", MODEL_NAME),
  paste("Variable order:", paste(MODEL_VARS, collapse = ", ")),
  "",
  paste("- Indexed files:", nrow(output_manifest)),
  paste("- Main manuscript files:", sum(output_manifest$manuscript_status == "main")),
  paste("- Appendix files:", sum(output_manifest$manuscript_status == "appendix")),
  paste("- Robustness files:", sum(output_manifest$manuscript_status == "robustness")),
  paste("- Archive/replication-only files:", sum(output_manifest$manuscript_status == "archive")),
  "",
  "The external-facing repository uses data/, code/, outputs/, docs/ and archive/ as its top-level structure."
)
writeLines(summary_lines, file.path(DOCS_DIR, "CLEAN_RELEASE_MANIFEST_REPORT.md"))

cat("Clean-release manifests written to:", DOCS_DIR, "\n")
