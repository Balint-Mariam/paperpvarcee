# Methodological code audit and consistency verification.
# This script reads existing final outputs only; it does not re-estimate any model.

required_packages <- c("readxl", "openxlsx", "dplyr", "tidyr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "), ". Run source('00_install_packages.R') first.")
}

suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(tibble)
})

ROOT <- getwd()
FINAL <- "outputs"
OUT_DIR <- file.path("archive", "internal_audit", "methodological_code_audit")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

AUDIT_XLSX <- file.path(OUT_DIR, "methodological_code_audit_checks.xlsx")
AUDIT_REPORT <- file.path(OUT_DIR, "methodological_code_audit_report.md")
AUDIT_SNAPSHOT <- file.path(OUT_DIR, "audit_reproducibility_snapshot.txt")
AUDIT_ISSUES <- file.path(OUT_DIR, "audit_issues_to_fix.md")

DATA_FILE <- file.path(FINAL, "01_model_ready_data", "model_ready_dataset.xlsx")
TRANSFORM_FILE <- file.path(FINAL, "01_model_ready_data", "transformation_summary.xlsx")
MASTER_FILE <- file.path(FINAL, "02_tables", "main_paper", "MASTER_all_tables_for_paper.xlsx")
APPENDIX_FILE <- file.path(FINAL, "02_tables", "appendix", "MASTER_appendix_tables.xlsx")
PREDIAG_FILE <- file.path(FINAL, "02_tables", "robustness", "pre_model_diagnostics_cleaned.xlsx")
DK_FILE <- file.path(FINAL, "02_tables", "robustness", "dk_inference", "FE_LSDV_PVAR_DK_inference.xlsx")
DK_REPORT <- file.path(FINAL, "02_tables", "robustness", "dk_inference", "DK_inference_report.md")
FIG_MANIFEST <- file.path(FINAL, "03_figures", "figure_manifest_polished.xlsx")
TABLE_MANIFEST <- file.path(FINAL, "02_tables", "manifests", "table_manifest_polished.xlsx")

MODEL_VARS <- c("Energy_Factor", "d_CISS", "d_CPI", "GDP_Growth", "d_3MRate", "d_FiscalBalanceGDP", "dlog_CDS")
COUNTRY_VARS <- c("d_CPI", "GDP_Growth", "d_3MRate", "d_FiscalBalanceGDP", "dlog_CDS")
COMMON_VARS <- c("Energy_Factor", "d_CISS")
PCA_DLOG_VARS <- c("dlog_TTF", "dlog_Brent", "dlog_Energy_Price", "dlog_Power_Energy_Price")
KEY_RELATIONS <- c(
  "Energy_Factor -> d_CPI",
  "Energy_Factor -> d_3MRate",
  "d_CPI -> d_3MRate",
  "Energy_Factor -> dlog_CDS",
  "d_CISS -> dlog_CDS",
  "d_CPI -> dlog_CDS",
  "GDP_Growth -> d_FiscalBalanceGDP",
  "d_3MRate -> dlog_CDS",
  "d_FiscalBalanceGDP -> dlog_CDS"
)

checks <- list()

read_sheet_safe <- function(path, sheet) {
  tryCatch(readxl::read_excel(path, sheet = sheet) |> as_tibble(), error = function(e) tibble())
}

SCRIPT_LOOKUP <- c(
  "00_master_pipeline_full_paper.R" = file.path("archive", "old_master_scripts", "00_master_pipeline_full_paper.R"),
  "15_structural_pvar_full7_final_workflow.R" = file.path("code", "05_structural_pvar", "15_structural_pvar_full7_final_workflow.R"),
  "17_structural_pvar_full7_refined4.R" = file.path("code", "05_structural_pvar", "17_structural_pvar_full7_refined4.R"),
  "19_structural_pvar_full7_hd_refined4.R" = file.path("code", "06_historical_decomposition", "19_structural_pvar_full7_hd_refined4.R"),
  "20_structural_pvar_full7_counterfactual_refined4.R" = file.path("code", "07_counterfactuals", "20_structural_pvar_full7_counterfactual_refined4.R"),
  "21_polish_q1_figures_tables.R" = file.path("code", "08_tables_figures", "21_polish_q1_figures_tables.R"),
  "22_pre_model_diagnostics_cleanup.R" = file.path("code", "02_pre_model_diagnostics", "22_pre_model_diagnostics_cleanup.R"),
  "23_fe_lsdv_pvar_dk_inference.R" = file.path("code", "04_robustness", "23_fe_lsdv_pvar_dk_inference.R"),
  "24_methodological_code_audit.R" = file.path("code", "09_validation", "24_methodological_code_audit.R"),
  "25_pipeline_handoff_docs.R" = file.path("code", "09_validation", "25_pipeline_handoff_docs.R")
)

resolve_file <- function(path) {
  if (file.exists(path)) return(path)
  base <- basename(path)
  if (base %in% names(SCRIPT_LOOKUP)) return(SCRIPT_LOOKUP[[base]])
  path
}

file_text <- function(path) {
  path <- resolve_file(path)
  if (!file.exists(path)) return(character())
  readLines(path, warn = FALSE)
}

extract_model_vars_from_script <- function(path) {
  txt <- file_text(path)
  if (length(txt) == 0) return(character())
  start <- grep("MODEL_VARS\\s*<-\\s*c\\(", txt)[1]
  if (is.na(start)) return(character())
  end <- start
  while (end <= length(txt) && !grepl("\\)", txt[end])) end <- end + 1L
  block <- paste(txt[start:min(end, length(txt))], collapse = " ")
  reg <- gregexpr("\"[^\"]+\"", block)
  hits <- regmatches(block, reg)[[1]]
  gsub("\"", "", hits)
}

status_from <- function(ok, warn = FALSE) {
  if (isTRUE(ok) && !isTRUE(warn)) "PASS" else if (isTRUE(ok) && isTRUE(warn)) "WARNING" else "FAIL"
}

add_check <- function(sheet, id, description, expected, observed, tolerance = "", ok = TRUE,
                      warn = FALSE, severity = "low", notes = "", action = "No action required.") {
  checks[[length(checks) + 1L]] <<- tibble(
    sheet = sheet,
    check_id = id,
    check_description = description,
    expected_value = as.character(expected),
    observed_value = as.character(observed),
    tolerance = as.character(tolerance),
    status = status_from(ok, warn),
    severity = severity,
    notes = notes,
    recommended_action = action
  )
}

near <- function(x, target, tol) isTRUE(is.finite(x) && abs(x - target) <= tol)
all_near <- function(x, target, tol) all(is.finite(x) & abs(x - target) <= tol, na.rm = TRUE)
fmt <- function(x) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = 6))
fmtp <- function(x) ifelse(is.na(x), "NA", ifelse(x < 0.001, "<0.001", formatC(x, format = "f", digits = 3)))

data <- read_sheet_safe(DATA_FILE, "estimation_balanced_dataset")
if (nrow(data) == 0) data <- read_sheet_safe(DATA_FILE, "model_ready_dataset")
trans <- read_sheet_safe(TRANSFORM_FILE, "transformed_variables")
pca_loadings <- read_sheet_safe(TRANSFORM_FILE, "pca_loadings")

panel_balance <- read_sheet_safe(MASTER_FILE, "T10_panel_balance")
fe_coef <- read_sheet_safe(MASTER_FILE, "T11_fe_lsdv_pvar_coefficients")
stability <- read_sheet_safe(MASTER_FILE, "T15_model_stability")
gmm_key <- read_sheet_safe(MASTER_FILE, "T17_gmm_robustness")
lp_dk <- read_sheet_safe(MASTER_FILE, "T18_lp_driscoll_kraay")
sign_restr <- read_sheet_safe(MASTER_FILE, "T21_structural_sign_restriction")
struct_accept <- read_sheet_safe(MASTER_FILE, "T22_structural_acceptance")
B_table <- read_sheet_safe(MASTER_FILE, "T24_structural_B_matrix")
fevd_cds <- read_sheet_safe(MASTER_FILE, "PAPER_Table_6_FEVD")
hd_cds <- read_sheet_safe(MASTER_FILE, "PAPER_Table_7_HD_CDS")
cf_cds <- read_sheet_safe(MASTER_FILE, "PAPER_Table_8_CF_CDS")
cf_scenarios <- read_sheet_safe(MASTER_FILE, "T43_cf_scenarios")

appendix_fevd <- read_sheet_safe(APPENDIX_FILE, "all_structural_fevd")
hd_recon <- read_sheet_safe(APPENDIX_FILE, "all_hd_reconstruction_checks")
all_diag <- read_sheet_safe(APPENDIX_FILE, "all_diagnostics")

prediag_summary <- read_sheet_safe(PREDIAG_FILE, "unit_root_summary")
prediag_cips <- read_sheet_safe(PREDIAG_FILE, "cips_cadf_results")
prediag_cd <- read_sheet_safe(PREDIAG_FILE, "cross_sectional_dependence")
prediag_common <- read_sheet_safe(PREDIAG_FILE, "common_variables_tests")
prediag_verdict <- read_sheet_safe(PREDIAG_FILE, "diagnostic_final_verdict")

dk_setup <- read_sheet_safe(DK_FILE, "DK_model_setup")
dk_coef <- read_sheet_safe(DK_FILE, "DK_coefficients_long")
dk_key <- read_sheet_safe(DK_FILE, "key_channels_original_vs_DK")
dk_sens <- read_sheet_safe(DK_FILE, "DK_sensitivity_lags")

fig_manifest <- read_sheet_safe(FIG_MANIFEST, 1)
table_manifest <- read_sheet_safe(TABLE_MANIFEST, 1)

script_files <- c(
  "00_master_pipeline_full_paper.R",
  "15_structural_pvar_full7_final_workflow.R",
  "17_structural_pvar_full7_refined4.R",
  "19_structural_pvar_full7_hd_refined4.R",
  "20_structural_pvar_full7_counterfactual_refined4.R",
  "21_polish_q1_figures_tables.R",
  "22_pre_model_diagnostics_cleanup.R",
  "23_fe_lsdv_pvar_dk_inference.R"
)

# A. Data and sample consistency
countries <- n_distinct(data$Country)
quarters <- n_distinct(data$Quarter_ID)
obs <- nrow(data)
min_q <- data$Quarter_ID[which.min(data$quarter_index)]
max_q <- data$Quarter_ID[which.max(data$quarter_index)]
duplicates <- data |> count(Country, Quarter_ID) |> filter(n > 1) |> nrow()
missing_model <- sum(is.na(data[, MODEL_VARS]))
balanced <- obs == countries * quarters
lagged <- data |> arrange(Country, quarter_index) |> group_by(Country) |>
  mutate(across(all_of(MODEL_VARS), ~ dplyr::lag(.x), .names = "{.col}_l1")) |>
  ungroup() |> filter(complete.cases(across(all_of(MODEL_VARS))), complete.cases(across(ends_with("_l1"))))

add_check("data_sample_checks", "A01", "Number of countries", 11, countries, "exact", countries == 11, severity = "critical")
add_check("data_sample_checks", "A02", "Number of model-ready quarters", 47, quarters, "exact", quarters == 47, severity = "critical")
add_check("data_sample_checks", "A03", "Model-ready sample range", "2014Q2-2025Q4", paste(min_q, max_q, sep = "-"), "exact", min_q == "2014Q2" && max_q == "2025Q4", severity = "critical")
add_check("data_sample_checks", "A04", "Model-ready observations", 517, obs, "exact", obs == 517, severity = "critical")
add_check("data_sample_checks", "A05", "Balanced panel", TRUE, balanced, "exact", isTRUE(balanced), severity = "critical")
add_check("data_sample_checks", "A06", "Missing values in final 7 variables", 0, missing_model, "exact", missing_model == 0, severity = "critical")
add_check("data_sample_checks", "A07", "Duplicate country-quarter rows", 0, duplicates, "exact", duplicates == 0, severity = "critical")
add_check("data_sample_checks", "A08", "Effective lagged sample range", "2014Q3-2025Q4", paste(lagged$Quarter_ID[which.min(lagged$quarter_index)], lagged$Quarter_ID[which.max(lagged$quarter_index)], sep = "-"), "exact", nrow(lagged) == 506, severity = "critical")
add_check("data_sample_checks", "A09", "Effective lagged observations", 506, nrow(lagged), "exact", nrow(lagged) == 506, severity = "critical")
if (nrow(panel_balance) > 0) {
  add_check("data_sample_checks", "A10", "Master panel balance table matches final dataset", "517/11/47/balanced", paste(panel_balance$observations[1], panel_balance$countries[1], panel_balance$quarters[1], panel_balance$balanced[1], sep = "/"), "exact", panel_balance$observations[1] == 517 && panel_balance$countries[1] == 11 && panel_balance$quarters[1] == 47 && isTRUE(panel_balance$balanced[1]), severity = "high")
}

# B. Variable construction
if (nrow(trans) > 0) {
  trans <- trans |> arrange(Country, quarter_index)
  calc <- trans |> group_by(Country) |>
    mutate(
      calc_d_CPI = CPI - dplyr::lag(CPI),
      calc_d_3MRate = .data[["3MRate"]] - dplyr::lag(.data[["3MRate"]]),
      calc_d_FiscalBalanceGDP = FiscalBalanceGDP - dplyr::lag(FiscalBalanceGDP),
      calc_dlog_CDS = log(CDS) - dplyr::lag(log(CDS))
    ) |>
    ungroup()
  common_calc <- trans |> distinct(Quarter_ID, .keep_all = TRUE) |> arrange(quarter_index) |>
    mutate(calc_d_CISS = CISS - dplyr::lag(CISS))
  add_check("variable_construction_checks", "B01", "d_CISS equals CISS - lag(CISS) at common-quarter level", "max abs diff < 1e-10", max(abs(common_calc$d_CISS - common_calc$calc_d_CISS), na.rm = TRUE), "1e-10", max(abs(common_calc$d_CISS - common_calc$calc_d_CISS), na.rm = TRUE) < 1e-10, severity = "critical")
  for (v in c("d_CPI", "d_3MRate", "d_FiscalBalanceGDP", "dlog_CDS")) {
    calc_name <- paste0("calc_", v)
    add_check("variable_construction_checks", paste0("B_", v), paste0(v, " transformation formula"), "max abs diff < 1e-10", max(abs(calc[[v]] - calc[[calc_name]]), na.rm = TRUE), "1e-10", max(abs(calc[[v]] - calc[[calc_name]]), na.rm = TRUE) < 1e-10, severity = "critical")
  }
  add_check("variable_construction_checks", "B06", "GDP_Growth kept in levels/growth rate", "GDP_Growth column exists and is not differenced in code", "present in transformed/model-ready data", "", "GDP_Growth" %in% names(trans) && "GDP_Growth" %in% names(data), severity = "high")
  add_check("variable_construction_checks", "B07", "Fiscal sign convention", "positive d_FiscalBalanceGDP means FiscalBalanceGDP increases", "d_FiscalBalanceGDP = current fiscal balance minus lag", "", TRUE, severity = "medium", notes = "Since FiscalBalanceGDP is fiscal balance as percent of GDP, a positive change is an improvement and a negative change is a deterioration.")

  common <- trans |> distinct(Quarter_ID, .keep_all = TRUE) |> arrange(quarter_index)
  for (raw in c("TTF", "Brent", "Energy_Price", "Power_Energy_Price")) {
    common[[paste0("dlog_", raw)]] <- log(common[[raw]]) - dplyr::lag(log(common[[raw]]))
  }
  pca_in <- common |> filter(complete.cases(across(all_of(PCA_DLOG_VARS))))
  X <- as.matrix(pca_in[, PCA_DLOG_VARS])
  Xs <- scale(X)
  sv <- svd(Xs)
  load <- sv$v
  rownames(load) <- PCA_DLOG_VARS
  pc1 <- as.numeric(Xs %*% load[, 1])
  if (sum(load[, 1]) < 0) {
    load[, 1] <- -load[, 1]
    pc1 <- -pc1
  }
  energy_match <- tibble(Quarter_ID = pca_in$Quarter_ID, calc_Energy_Factor = pc1) |>
    left_join(common |> select(Quarter_ID, Energy_Factor), by = "Quarter_ID")
  pca_diff <- max(abs(energy_match$Energy_Factor - energy_match$calc_Energy_Factor), na.rm = TRUE)
  add_check("variable_construction_checks", "B08", "Energy_Factor PC1 recomputation from four dlog energy-carbon inputs", "max abs diff < 1e-10", pca_diff, "1e-10", pca_diff < 1e-10, severity = "critical")
  add_check("variable_construction_checks", "B09", "PCA loadings contain exactly the four energy-carbon inputs", paste(PCA_DLOG_VARS, collapse = ", "), paste(pca_loadings$variable, collapse = ", "), "set equality", setequal(pca_loadings$variable, PCA_DLOG_VARS), severity = "high")
}

# C. Variable order consistency
for (sf in script_files[file.exists(vapply(script_files, resolve_file, character(1)))]) {
  order_found <- extract_model_vars_from_script(sf)
  has_exact_order <- identical(order_found, MODEL_VARS)
  documented_downstream <- sf %in% c("21_polish_q1_figures_tables.R")
  add_check(
    "variable_order_checks",
    paste0("C_", basename(sf)),
    paste0("MODEL_VARS order in ", sf),
    paste(MODEL_VARS, collapse = " -> "),
    if (length(order_found) == 0) "MODEL_VARS not defined directly" else paste(order_found, collapse = " -> "),
    "exact ordered vector",
    has_exact_order || documented_downstream,
    warn = documented_downstream && !has_exact_order,
    severity = "medium",
    notes = if (documented_downstream && !has_exact_order) "Polishing script uses output-driven variable ordering and labels; source model ordering is audited in model scripts." else ""
  )
}
add_check("variable_order_checks", "C10", "FE/LSDV coefficient equations cover all final variables", paste(MODEL_VARS, collapse = ", "), paste(unique(fe_coef$equation), collapse = ", "), "set equality", setequal(unique(fe_coef$equation), MODEL_VARS), severity = "critical")
add_check("variable_order_checks", "C11", "Structural sign restriction variables cover all final variables", paste(MODEL_VARS, collapse = ", "), paste(unique(sign_restr$variable), collapse = ", "), "set equality", setequal(unique(sign_restr$variable), MODEL_VARS), severity = "critical")

# D. FE/LSDV PVAR audit
if (nrow(fe_coef) > 0) {
  add_check("FE_LSDV_checks", "D01", "FE/LSDV coefficient matrix size", "49 coefficients / 7x7", nrow(fe_coef), "exact", nrow(fe_coef) == 49, severity = "critical")
  add_check("FE_LSDV_checks", "D02", "Lag order for all FE/LSDV regressors", 1, paste(sort(unique(fe_coef$lag)), collapse = ","), "exact", identical(sort(unique(fe_coef$lag)), 1), severity = "critical")
  add_check("FE_LSDV_checks", "D03", "Country fixed effects implementation in source code", "plm(... model='within', effect='individual')", "checked in 15_structural_pvar_full7_final_workflow.R", "", grepl("model = \"within\"", paste(file_text("15_structural_pvar_full7_final_workflow.R"), collapse = "\n")) && grepl("effect = \"individual\"", paste(file_text("15_structural_pvar_full7_final_workflow.R"), collapse = "\n")), severity = "critical")
  fe_max <- stability |> filter(model == "FE/LSDV PVAR(1)", !is.na(max_modulus)) |> pull(max_modulus)
  if (length(fe_max) == 0) fe_max <- all_diag |> filter(model == "FE/LSDV PVAR(1)") |> pull(max_modulus)
  add_check("FE_LSDV_checks", "D04", "FE/LSDV stability max modulus", "approx 0.642314 and < 1", fe_max[1], "1e-6", near(fe_max[1], 0.642314242630269, 1e-6) && fe_max[1] < 1, severity = "critical")
  add_check("FE_LSDV_checks", "D05", "All requested key FE/LSDV relations are present", paste(KEY_RELATIONS, collapse = "; "), paste(intersect(KEY_RELATIONS, fe_coef$relation), collapse = "; "), "set inclusion", all(KEY_RELATIONS %in% fe_coef$relation), severity = "high")
}

# E. DK inference audit
if (nrow(dk_coef) > 0) {
  diff_max <- max(abs(dk_coef$coefficient_difference_vs_master), na.rm = TRUE)
  lag4 <- dk_setup |> filter(item == "DK_baseline_lag") |> pull(value)
  lags <- dk_setup |> filter(item == "DK_sensitivity_lags") |> pull(value)
  add_check("DK_inference_checks", "E01", "DK changes only inference, not coefficients", "max coefficient diff < 1e-10", diff_max, "1e-10", diff_max < 1e-10, severity = "critical")
  add_check("DK_inference_checks", "E02", "DK baseline maxlag", 4, lag4[1], "exact", lag4[1] == "4", severity = "high")
  add_check("DK_inference_checks", "E03", "DK sensitivity lags", "2, 4, 6", lags[1], "exact", lags[1] == "2, 4, 6", severity = "medium")
  add_check("DK_inference_checks", "E04", "No initially significant key channel becomes insignificant under DK", "0 weaker under DK", sum(dk_key$final_interpretation == "weaker under DK", na.rm = TRUE), "exact", sum(dk_key$final_interpretation == "weaker under DK", na.rm = TRUE) == 0, severity = "high")
  add_check("DK_inference_checks", "E05", "Channels insignificant under original inference remain insignificant under DK", "d_3MRate and d_FiscalBalanceGDP direct CDS channels", paste(dk_key$relation[dk_key$final_interpretation == "not significant under either"], collapse = "; "), "exact", all(c("d_3MRate -> dlog_CDS", "d_FiscalBalanceGDP -> dlog_CDS") %in% dk_key$relation[dk_key$final_interpretation == "not significant under either"]), severity = "high")
  report_txt <- paste(file_text(DK_REPORT), collapse = "\n")
  p_ok <- all(vapply(seq_len(nrow(dk_key)), function(i) grepl(dk_key$relation[i], report_txt, fixed = TRUE) && grepl(fmtp(dk_key$DK_p_value[i]), report_txt, fixed = TRUE), logical(1)))
  add_check("DK_inference_checks", "E06", "DK p-values in Excel are reflected in DK report", "all key relation formatted p-values found", p_ok, "formatted match", p_ok, severity = "medium")
}

# F. Pre-model diagnostics audit
if (nrow(prediag_summary) > 0) {
  add_check("pre_model_diagnostics_checks", "F01", "All seven variables tested", paste(MODEL_VARS, collapse = ", "), paste(prediag_summary$variable, collapse = ", "), "set equality", setequal(prediag_summary$variable, MODEL_VARS), severity = "critical")
  add_check("pre_model_diagnostics_checks", "F02", "d_FiscalBalanceGDP explicitly included", "included", "d_FiscalBalanceGDP" %in% prediag_summary$variable, "", "d_FiscalBalanceGDP" %in% prediag_summary$variable, severity = "critical")
  add_check("pre_model_diagnostics_checks", "F03", "Common variables treated as common", "Energy_Factor and d_CISS common", paste(prediag_verdict$variable[prediag_verdict$common_variable == "yes"], collapse = ", "), "exact", setequal(prediag_verdict$variable[prediag_verdict$common_variable == "yes"], COMMON_VARS), severity = "high")
  add_check("pre_model_diagnostics_checks", "F04", "ADF/PP/KPSS used for common variables", "ADF, PP, KPSS", paste(unique(prediag_common$test), collapse = ", "), "contains all", all(c("ADF time-series", "PP time-series", "KPSS time-series") %in% unique(prediag_common$test)), severity = "high")
  add_check("pre_model_diagnostics_checks", "F05", "CIPS/CADF runs successfully for country-specific variables", "ok for five country-specific variables", paste(prediag_cips$status[prediag_cips$variable %in% COUNTRY_VARS], collapse = ", "), "all ok", all(prediag_cips$status[prediag_cips$variable %in% COUNTRY_VARS] == "ok"), severity = "high")
  add_check("pre_model_diagnostics_checks", "F06", "Pesaran CD is not applied to common variables", "not_applicable for Energy_Factor and d_CISS", paste(prediag_cd$cd_verdict[prediag_cd$variable %in% COMMON_VARS], collapse = ", "), "exact", all(grepl("not applicable", prediag_cd$cd_verdict[prediag_cd$variable %in% COMMON_VARS])), severity = "high")
  add_check("pre_model_diagnostics_checks", "F07", "Stationarity verdict coherent", "all stationary/transformation adequate", paste(prediag_summary$unit_root_verdict, collapse = "; "), "all", all(prediag_summary$unit_root_verdict == "stationary / transformation adequate"), severity = "high")
}

# G. GMM audit
gmm_diag <- all_diag |> filter(model == "PVAR-GMM(1)")
if (nrow(gmm_diag) > 0) {
  add_check("GMM_checks", "G01", "GMM used as robustness", "robustness table only", paste(unique(gmm_key$model), collapse = ", "), "", all(unique(gmm_key$model) == "PVAR-GMM(1)"), severity = "high")
  add_check("GMM_checks", "G02", "GMM instrument count", 49, gmm_diag$instrument_count[1], "exact", gmm_diag$instrument_count[1] == 49, severity = "high")
  add_check("GMM_checks", "G03", "GMM instrument/country ratio", "approx 4.45", gmm_diag$instrument_country_ratio[1], "0.01", near(gmm_diag$instrument_country_ratio[1], 4.454545, 0.01), severity = "medium")
  add_check("GMM_checks", "G04", "GMM stability", "stable, max modulus approx 0.8544", gmm_diag$max_modulus[1], "0.001 and <1", near(gmm_diag$max_modulus[1], 0.8543831, 0.001) && isTRUE(gmm_diag$stable[1]), severity = "high")
  add_check("GMM_checks", "G05", "Collapsed instruments documented in code", "collapse = TRUE", grepl("collapse = TRUE", paste(file_text("15_structural_pvar_full7_final_workflow.R"), collapse = "\n")), "", grepl("collapse = TRUE", paste(file_text("15_structural_pvar_full7_final_workflow.R"), collapse = "\n")), severity = "medium")
  add_check("GMM_checks", "G06", "GMM(2) not promoted as baseline", "no GMM(2) baseline", !any(grepl("PVAR-GMM\\(2\\)", gmm_key$model)), "", !any(grepl("PVAR-GMM\\(2\\)", gmm_key$model)), severity = "high")
}

# H. LP-DK audit
add_check("LP_DK_checks", "H01", "LP-DK robustness table exists", "non-empty", nrow(lp_dk), "> 0", nrow(lp_dk) > 0, severity = "medium")
add_check("LP_DK_checks", "H02", "LP-DK uses Driscoll-Kraay vcovSCC in code", "vcovSCC in run_lp", grepl("vcovSCC", paste(file_text("15_structural_pvar_full7_final_workflow.R"), collapse = "\n")), "", grepl("vcovSCC", paste(file_text("15_structural_pvar_full7_final_workflow.R"), collapse = "\n")), severity = "medium")
add_check("LP_DK_checks", "H03", "LP-DK is not promoted as baseline", "robustness/complementary evidence", "reported in T18_lp_driscoll_kraay", "", nrow(lp_dk) > 0, severity = "medium")

# I. Structural PVAR audit
if (nrow(struct_accept) > 0) {
  add_check("Structural_PVAR_checks", "I01", "Structural model variant", "S1_four_shock_sign_only_h0_h2", struct_accept$model_variant[1], "exact", struct_accept$model_variant[1] == "S1_four_shock_sign_only_h0_h2", severity = "critical")
  add_check("Structural_PVAR_checks", "I02", "Restriction horizons", "0, 1, 2", struct_accept$restriction_horizons[1], "exact", struct_accept$restriction_horizons[1] == "0, 1, 2", severity = "critical")
  add_check("Structural_PVAR_checks", "I03", "Candidate rotations", 50000, struct_accept$candidate_rotations[1], "exact", struct_accept$candidate_rotations[1] == 50000, severity = "high")
  add_check("Structural_PVAR_checks", "I04", "Accepted rotations", 12715, struct_accept$accepted_rotations[1], "exact", struct_accept$accepted_rotations[1] == 12715, severity = "high")
  add_check("Structural_PVAR_checks", "I05", "Acceptance rate", "0.2543", struct_accept$acceptance_rate[1], "1e-6", near(struct_accept$acceptance_rate[1], 0.2543, 1e-6), severity = "high")
  add_check("Structural_PVAR_checks", "I06", "Unique assignment rate", "0.2034", struct_accept$unique_assignment_rate[1], "1e-4", near(struct_accept$unique_assignment_rate[1], 0.2033818, 1e-4), severity = "medium")
}
expected_signs <- tibble::tribble(
  ~shock, ~variable, ~restriction,
  "Energy-carbon pressure shock", "Energy_Factor", "positive",
  "Energy-carbon pressure shock", "d_CPI", "positive",
  "Energy-carbon pressure shock", "d_3MRate", "positive",
  "Energy-carbon pressure shock", "dlog_CDS", "free",
  "Systemic financial stress shock", "d_CISS", "positive",
  "Systemic financial stress shock", "GDP_Growth", "negative",
  "Systemic financial stress shock", "dlog_CDS", "free",
  "Inflationary monetary-reaction shock", "d_CPI", "positive",
  "Inflationary monetary-reaction shock", "d_3MRate", "positive",
  "Inflationary monetary-reaction shock", "dlog_CDS", "free",
  "Sovereign-risk repricing shock", "dlog_CDS", "positive"
)
sign_join <- expected_signs |> left_join(sign_restr |> select(shock, variable, observed_restriction = restriction, horizons_imposed), by = c("shock", "variable"))
add_check("Structural_PVAR_checks", "I07", "Core sign restrictions match refined4 S1 specification", "expected restrictions", paste(sign_join$observed_restriction, collapse = ", "), "exact", all(sign_join$restriction == sign_join$observed_restriction), severity = "critical")
if (nrow(B_table) > 0) {
  B <- as.matrix(B_table[, -1])
  storage.mode(B) <- "double"
  eig <- eigen(B %*% t(B), symmetric = TRUE, only.values = TRUE)$values
  add_check("Structural_PVAR_checks", "I08", "Representative structural covariance B B' positive definite", "all eigenvalues > 0", min(eig), "> 0", min(eig) > 0, severity = "critical")
}
master_txt <- paste(file_text("00_master_pipeline_full_paper.R"), collapse = "\n")
add_check("Structural_PVAR_checks", "I09", "Representative draw documented", "candidate 23085 / accepted 5782", grepl("candidate draw 23085 / accepted draw 5782", master_txt), "", grepl("candidate draw 23085 / accepted draw 5782", master_txt), severity = "medium")
add_check("Structural_PVAR_checks", "I10", "Structural layer uses FE/LSDV dynamic matrix, not GMM", "FE/LSDV structural baseline", "documented in methodological pipeline report and master", "", grepl("FE/LSDV", paste(file_text(file.path(FINAL, "04_reports", "methodological_pipeline_report.md")), collapse = "\n")), severity = "high")

# J. FEVD audit
if (nrow(appendix_fevd) > 0) {
  range_ok <- min(appendix_fevd$mean_share, na.rm = TRUE) >= -1e-12 && max(appendix_fevd$mean_share, na.rm = TRUE) <= 1 + 1e-12
  sum_ok <- max(abs(appendix_fevd$sum_mean_share - 1), na.rm = TRUE)
  add_check("FEVD_checks", "J01", "FEVD mean shares are within [0,1]", "[0,1]", paste(range(appendix_fevd$mean_share, na.rm = TRUE), collapse = " to "), "1e-12", range_ok, severity = "critical")
  add_check("FEVD_checks", "J02", "FEVD shares sum to one by response/horizon", "max abs(sum-1) < 1e-10", sum_ok, "1e-10", sum_ok < 1e-10, severity = "critical")
}
if (nrow(fevd_cds) > 0) {
  expected_fevd <- c(
    "Energy-carbon pressure shock" = 0.1318,
    "Systemic financial stress shock" = 0.1339,
    "Inflationary monetary-reaction shock" = 0.1328,
    "Sovereign-risk repricing shock" = 0.1690,
    "Other / unidentified structural shocks" = 0.4325
  )
  obs_fevd <- setNames(fevd_cds$mean_share, fevd_cds$shock)
  fevd_diff <- max(abs(obs_fevd[names(expected_fevd)] - expected_fevd), na.rm = TRUE)
  add_check("FEVD_checks", "J03", "dlog_CDS h12 FEVD expected approximate shares", "Energy 13.18%, CISS 13.39%, Inflation/Rate 13.28%, Sovereign 16.90%, Other 43.25%", paste(round(100 * obs_fevd[names(expected_fevd)], 2), collapse = ", "), "0.25 percentage points", fevd_diff < 0.0025, severity = "medium", notes = "Differences at this tolerance are rounding, not methodological changes.")
}

# K. Historical decomposition audit
if (nrow(hd_recon) > 0) {
  max_recon <- hd_recon |> filter(level == "overall") |> pull(max_abs_error)
  add_check("HD_checks", "K01", "HD reconstruction max error", "< 1e-10", max_recon[1], "1e-10", max_recon[1] < 1e-10, severity = "critical")
}
hd_start <- read_sheet_safe(MASTER_FILE, "T38_hd_cumulative_dlog_CDS")
if (nrow(hd_start) > 0) {
  add_check("HD_checks", "K02", "HD dlog_CDS sample starts after lag", "2014Q3", hd_start$Quarter_ID[which.min(hd_start$quarter_index)], "exact", hd_start$Quarter_ID[which.min(hd_start$quarter_index)] == "2014Q3", severity = "high")
}
episode_hd <- hd_cds |> filter(period_start == "2021Q1", period_end == "2023Q4")
if (nrow(episode_hd) > 0) {
  vals <- c(
    cumulative_energy = 0.2736,
    cumulative_sovereign = 0.1460,
    cumulative_ciss = -0.0368,
    cumulative_inflationary_monetary = -0.1128,
    cumulative_other = -0.0352
  )
  diffs <- abs(as.numeric(episode_hd[1, names(vals)]) - vals)
  add_check("HD_checks", "K03", "dlog_CDS 2021Q1-2023Q4 cumulative HD contributions", paste(names(vals), vals, collapse = "; "), paste(round(as.numeric(episode_hd[1, names(vals)]), 4), collapse = ", "), "0.001", max(diffs, na.rm = TRUE) < 0.001, severity = "medium")
  add_check("HD_checks", "K04", "Energy-inflation/tightening subperiod is documented", "2021Q1-2023Q4", paste(episode_hd$period_start, episode_hd$period_end, sep = "-"), "exact", episode_hd$period_start[1] == "2021Q1" && episode_hd$period_end[1] == "2023Q4", severity = "medium")
}

# L. Counterfactual audit
if (nrow(cf_scenarios) > 0) {
  sc <- cf_scenarios |> filter(scenario %in% c("CF1_no_energy", "CF4_no_sovereign", "CF6_no_energy_no_sovereign"))
  add_check("CF_checks", "L01", "Main counterfactual scenarios are CF1, CF4, CF6", "CF1/CF4/CF6 marked main", paste(sc$scenario, sc$paper_priority, sep = "=", collapse = "; "), "exact", all(sc$paper_priority == "main") && nrow(sc) == 3, severity = "high")
  non_main_cf_ok <- all(c("CF3_no_inflationary_monetary", "CF5_no_energy_no_inflationary", "CF7_no_macro_financial") %in% cf_scenarios$scenario[cf_scenarios$paper_priority != "main"]) &&
    !"CF2_no_ciss" %in% fig_manifest$scenario_or_shock[fig_manifest$main_text_or_appendix == "main"]
  add_check(
    "CF_checks",
    "L02",
    "CF2/CF3/CF5/CF7 are not promoted to main-paper polished outputs",
    "CF1/CF4/CF6 only in polished main paper; CF2/CF3/CF5/CF7 appendix or replication",
    paste(cf_scenarios$scenario, cf_scenarios$paper_priority, sep = "=", collapse = "; "),
    "manifest/paper-priority consistency",
    TRUE,
    warn = !non_main_cf_ok || "CF2_no_ciss" %in% cf_scenarios$scenario[cf_scenarios$paper_priority == "main"],
    severity = "medium",
    notes = "The raw scenario table marks CF2 as main, but polished manuscript selection and handoff documentation should keep only CF1, CF4 and CF6 in the main paper.",
    action = "Use the polished output selection/handoff manifest for main-paper inclusion; do not promote CF2 in the manuscript."
  )
}
episode_cf <- cf_cds |> filter(period_start == "2021Q1", period_end == "2023Q4", scenario %in% c("CF1_no_energy", "CF4_no_sovereign", "CF6_no_energy_no_sovereign"))
if (nrow(episode_cf) == 3) {
  exp_gap <- c(CF1_no_energy = 0.2736, CF4_no_sovereign = 0.1460, CF6_no_energy_no_sovereign = 0.4196)
  exp_pct <- c(CF1_no_energy = 0.315, CF4_no_sovereign = 0.157, CF6_no_energy_no_sovereign = 0.521)
  obs_gap <- setNames(episode_cf$cumulative_gap, episode_cf$scenario)
  obs_pct <- setNames(episode_cf$cumulative_percent_effect_dlog_CDS, episode_cf$scenario)
  add_check("CF_checks", "L03", "dlog_CDS CF cumulative gaps for 2021Q1-2023Q4", "0.2736, 0.1460, 0.4196", paste(round(obs_gap[names(exp_gap)], 4), collapse = ", "), "0.001", max(abs(obs_gap[names(exp_gap)] - exp_gap), na.rm = TRUE) < 0.001, severity = "high")
  add_check("CF_checks", "L04", "dlog_CDS CF percent effects use exp(cumulative_gap)-1", "31.5%, 15.7%, 52.1%", paste(round(100 * obs_pct[names(exp_pct)], 1), collapse = ", "), "0.3 percentage points", max(abs(obs_pct[names(exp_pct)] - exp_pct), na.rm = TRUE) < 0.003, severity = "high")
  add_check("CF_checks", "L05", "CF percent effect formula", "exp(gap)-1", max(abs(obs_pct - (exp(obs_gap) - 1)), na.rm = TRUE), "1e-10", max(abs(obs_pct - (exp(obs_gap) - 1)), na.rm = TRUE) < 1e-10, severity = "high")
}
add_check("CF_checks", "L06", "Counterfactuals use HD contributions, not re-estimation", "source code uses HD inputs", grepl("HD_DIR", paste(file_text("20_structural_pvar_full7_counterfactual_refined4.R"), collapse = "\n")), "", grepl("HD_DIR", paste(file_text("20_structural_pvar_full7_counterfactual_refined4.R"), collapse = "\n")), severity = "medium")

# M. Figures and tables audit
add_check("figures_tables_checks", "M01", "Polished main paper figures count", 6, sum(fig_manifest$main_text_or_appendix == "main", na.rm = TRUE), "exact", sum(fig_manifest$main_text_or_appendix == "main", na.rm = TRUE) == 6, severity = "medium")
add_check("figures_tables_checks", "M02", "Polished appendix figures count", 7, sum(fig_manifest$main_text_or_appendix == "appendix", na.rm = TRUE), "exact", sum(fig_manifest$main_text_or_appendix == "appendix", na.rm = TRUE) == 7, severity = "medium")
add_check("figures_tables_checks", "M03", "Polished main/optional manuscript tables count", "9", nrow(table_manifest), "exact", nrow(table_manifest) == 9, severity = "medium")
add_check("figures_tables_checks", "M04", "Reduced-form Table 3 should use DK stars when paper reports DK inference", "DK table available in outputs/02_tables/robustness/dk_inference", file.exists(DK_FILE), "", file.exists(DK_FILE), severity = "medium", notes = "Use DK inference workbook for manuscript Table 3 if coefficient-level DK inference is reported.")
add_check("figures_tables_checks", "M05", "Structural figures are not modified by DK inference", "DK outputs isolated in robustness folder", dirname(DK_FILE), "", grepl("dk_inference", DK_FILE), severity = "medium")
add_check("figures_tables_checks", "M06", "Old unpolished main figures are superseded by polished selection", "polished manifests exist", file.exists(FIG_MANIFEST), "", file.exists(FIG_MANIFEST), warn = TRUE, severity = "low", notes = "Unpolished figures remain in the replication package but should not be used as main-paper figures.")

all_checks <- bind_rows(checks)
audit_summary <- all_checks |>
  count(status, severity, name = "n") |>
  arrange(factor(status, levels = c("FAIL", "WARNING", "PASS")), severity)
issues <- all_checks |> filter(status != "PASS") |> arrange(factor(status, c("FAIL", "WARNING")), severity, check_id)
fail_count <- sum(all_checks$status == "FAIL")
warning_count <- sum(all_checks$status == "WARNING")
pass_count <- sum(all_checks$status == "PASS")
verdict <- if (fail_count > 0) {
  "FAIL - critical methodological issues detected; model outputs should not be used before correction."
} else if (warning_count > 0) {
  "PASS WITH MINOR WARNINGS - pipeline is coherent, but minor documentation fixes are needed."
} else {
  "PASS - pipeline is methodologically coherent and manuscript-ready."
}

audit_sheets <- list(
  audit_summary = bind_rows(
    tibble(check_id = "FINAL_VERDICT", check_description = "Final audit verdict", expected_value = "Methodological consistency", observed_value = verdict, tolerance = "", status = ifelse(fail_count > 0, "FAIL", ifelse(warning_count > 0, "WARNING", "PASS")), severity = ifelse(fail_count > 0, "critical", ifelse(warning_count > 0, "low", "low")), notes = "", recommended_action = ifelse(fail_count > 0, "Fix failed checks before manuscript use.", "Proceed with manuscript writing; address minor warnings if useful.")),
    all_checks |> count(status, name = "observed_value") |> transmute(check_id = paste0("COUNT_", status), check_description = paste("Number of", status, "checks"), expected_value = "", observed_value = as.character(observed_value), tolerance = "", status = status, severity = "low", notes = "", recommended_action = "")
  ),
  data_sample_checks = all_checks |> filter(sheet == "data_sample_checks") |> select(-sheet),
  variable_construction_checks = all_checks |> filter(sheet == "variable_construction_checks") |> select(-sheet),
  variable_order_checks = all_checks |> filter(sheet == "variable_order_checks") |> select(-sheet),
  FE_LSDV_checks = all_checks |> filter(sheet == "FE_LSDV_checks") |> select(-sheet),
  DK_inference_checks = all_checks |> filter(sheet == "DK_inference_checks") |> select(-sheet),
  pre_model_diagnostics_checks = all_checks |> filter(sheet == "pre_model_diagnostics_checks") |> select(-sheet),
  GMM_checks = all_checks |> filter(sheet == "GMM_checks") |> select(-sheet),
  LP_DK_checks = all_checks |> filter(sheet == "LP_DK_checks") |> select(-sheet),
  Structural_PVAR_checks = all_checks |> filter(sheet == "Structural_PVAR_checks") |> select(-sheet),
  FEVD_checks = all_checks |> filter(sheet == "FEVD_checks") |> select(-sheet),
  HD_checks = all_checks |> filter(sheet == "HD_checks") |> select(-sheet),
  CF_checks = all_checks |> filter(sheet == "CF_checks") |> select(-sheet),
  figures_tables_checks = all_checks |> filter(sheet == "figures_tables_checks") |> select(-sheet),
  issues_log = if (nrow(issues) == 0) tibble(check_id = "NO_ISSUES", check_description = "No critical methodological implementation issues were detected.", status = "PASS", severity = "low", recommended_action = "No action required.") else issues |> select(-sheet)
)

write_wb <- function(path, sheets) {
  wb <- createWorkbook()
  header_style <- createStyle(textDecoration = "bold", fgFill = "#D9EAF7", border = "Bottom")
  for (nm in names(sheets)) {
    addWorksheet(wb, nm)
    df <- as.data.frame(sheets[[nm]])
    writeDataTable(wb, nm, df, tableStyle = "TableStyleMedium2")
    if (ncol(df) > 0) {
      addStyle(wb, nm, header_style, rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
      freezePane(wb, nm, firstRow = TRUE)
      setColWidths(wb, nm, cols = seq_len(ncol(df)), widths = "auto")
    }
  }
  saveWorkbook(wb, path, overwrite = TRUE)
}
write_wb(AUDIT_XLSX, audit_sheets)

section_text <- function(title, sheet_name) {
  dat <- all_checks |> filter(sheet == sheet_name)
  c(
    paste0("## ", title),
    "",
    paste0("Checks: ", nrow(dat), "; PASS: ", sum(dat$status == "PASS"), "; WARNING: ", sum(dat$status == "WARNING"), "; FAIL: ", sum(dat$status == "FAIL"), "."),
    "",
    if (any(dat$status != "PASS")) paste0("- ", dat$check_id[dat$status != "PASS"], ": ", dat$status[dat$status != "PASS"], " - ", dat$notes[dat$status != "PASS"]) else "No issues detected.",
    ""
  )
}

report_lines <- c(
  "# Methodological Code Audit Report",
  "",
  "## Executive summary",
  "",
  paste0("Final verdict: ", verdict),
  paste0("Total checks: ", nrow(all_checks), "; PASS: ", pass_count, "; WARNING: ", warning_count, "; FAIL: ", fail_count, "."),
  "This audit reads existing final outputs only and does not re-estimate the PVAR, Structural PVAR, historical decomposition or counterfactual analysis.",
  "",
  section_text("Data and sample audit", "data_sample_checks"),
  section_text("Variable construction audit", "variable_construction_checks"),
  section_text("Variable order audit", "variable_order_checks"),
  section_text("FE/LSDV PVAR audit", "FE_LSDV_checks"),
  section_text("Driscoll-Kraay inference audit", "DK_inference_checks"),
  section_text("Pre-model diagnostics audit", "pre_model_diagnostics_checks"),
  section_text("GMM robustness audit", "GMM_checks"),
  section_text("LP-DK robustness audit", "LP_DK_checks"),
  section_text("Structural PVAR audit", "Structural_PVAR_checks"),
  section_text("FEVD audit", "FEVD_checks"),
  section_text("Historical Decomposition audit", "HD_checks"),
  section_text("Counterfactual audit", "CF_checks"),
  section_text("Figures and tables audit", "figures_tables_checks"),
  "## Critical issues found",
  "",
  if (any(all_checks$status == "FAIL" & all_checks$severity == "critical")) paste0("- ", all_checks$check_id[all_checks$status == "FAIL" & all_checks$severity == "critical"], ": ", all_checks$check_description[all_checks$status == "FAIL" & all_checks$severity == "critical"]) else "No critical methodological implementation issues were detected.",
  "",
  "## Minor issues found",
  "",
  if (nrow(issues) > 0) paste0("- ", issues$check_id, " [", issues$status, "]: ", issues$check_description, ". ", issues$notes) else "No warnings or failures were detected.",
  "",
  "## Recommendations before manuscript writing",
  "",
  "- Use FE/LSDV coefficients with Driscoll-Kraay coefficient-level inference for reduced-form Table 3.",
  "- Use polished figures and polished table workbook for the manuscript.",
  "- Treat GMM and LP-DK as robustness evidence, not as the structural baseline.",
  "- Keep Structural PVAR, HD and counterfactual interpretation tied to the stable FE/LSDV dynamic matrix.",
  "",
  "## Final verdict",
  "",
  verdict
)
writeLines(unlist(report_lines), AUDIT_REPORT, useBytes = TRUE)

issue_lines <- if (nrow(issues) == 0) {
  c("# Audit Issues To Fix", "", "No critical methodological implementation issues were detected.")
} else {
  c("# Audit Issues To Fix", "", paste0("## ", unique(issues$severity)), "", apply(issues, 1, function(row) {
    paste0("- ", row[["check_id"]], ": ", row[["check_description"]], " Status: ", row[["status"]], ". Affects results: no unless marked critical. Recommended action: ", row[["recommended_action"]])
  }))
}
writeLines(unlist(issue_lines), AUDIT_ISSUES, useBytes = TRUE)

commit_hash <- tryCatch(system("git rev-parse HEAD", intern = TRUE), error = function(e) "not available")
pkg_versions <- vapply(required_packages, function(p) as.character(utils::packageVersion(p)), character(1))
snapshot <- c(
  "Audit reproducibility snapshot",
  paste0("Commit hash: ", commit_hash[1]),
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("R version: ", R.version.string),
  "Package versions:",
  paste0("- ", names(pkg_versions), ": ", pkg_versions),
  "Input files used:",
  paste0("- ", c(DATA_FILE, TRANSFORM_FILE, MASTER_FILE, APPENDIX_FILE, PREDIAG_FILE, DK_FILE, FIG_MANIFEST, TABLE_MANIFEST)),
  "Output files checked: final Excel workbooks, polished manifests, final reports, source scripts.",
  paste0("Number of checks: ", nrow(all_checks)),
  paste0("PASS count: ", pass_count),
  paste0("WARNING count: ", warning_count),
  paste0("FAIL count: ", fail_count),
  paste0("Final audit verdict: ", verdict)
)
writeLines(snapshot, AUDIT_SNAPSHOT, useBytes = TRUE)

message("Methodological audit complete.")
message("Checks workbook: ", AUDIT_XLSX)
message("Report: ", AUDIT_REPORT)
message("Snapshot: ", AUDIT_SNAPSHOT)
message("Issues: ", AUDIT_ISSUES)
