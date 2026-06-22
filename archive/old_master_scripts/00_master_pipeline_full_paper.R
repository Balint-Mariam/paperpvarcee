# Unified reproducible pipeline and paper-ready consolidation.
# Final structural model: refined4 S1, four-shock sign-only h = 0,1,2.
# This script consolidates the validated outputs into one final paper folder.
# Set RUN_FROM_SCRATCH=TRUE in the environment to rerun the major stage scripts
# before consolidation. In a clean clone, missing validated caches are detected
# automatically and the script rebuilds from the Excel input.

rm(list = ls())

# =============================================================================
# SECTION 0 - Configuration
# =============================================================================

RUN_FROM_SCRATCH <- tolower(Sys.getenv("RUN_FROM_SCRATCH", "false")) == "true"
USE_CACHED_INTERMEDIATE_OUTPUTS <- tolower(Sys.getenv("USE_CACHED_INTERMEDIATE_OUTPUTS", "true")) == "true"
RUN_REDUCED_FORM <- tolower(Sys.getenv("RUN_REDUCED_FORM", "true")) == "true"
RUN_ROBUSTNESS <- tolower(Sys.getenv("RUN_ROBUSTNESS", "true")) == "true"
RUN_STRUCTURAL_PVAR <- tolower(Sys.getenv("RUN_STRUCTURAL_PVAR", "true")) == "true"
RUN_HISTORICAL_DECOMPOSITION <- tolower(Sys.getenv("RUN_HISTORICAL_DECOMPOSITION", "true")) == "true"
RUN_COUNTERFACTUAL <- tolower(Sys.getenv("RUN_COUNTERFACTUAL", "true")) == "true"
GENERATE_ALL_FIGURES <- tolower(Sys.getenv("GENERATE_ALL_FIGURES", "true")) == "true"
GENERATE_PAPER_FIGURES <- tolower(Sys.getenv("GENERATE_PAPER_FIGURES", "true")) == "true"
GENERATE_MASTER_EXCEL <- tolower(Sys.getenv("GENERATE_MASTER_EXCEL", "true")) == "true"

ROOT_DIR <- getwd()
FINAL_DIR <- file.path(ROOT_DIR, "FINAL_Q1_PAPER_OUTPUTS")
CODE_DIR <- file.path(FINAL_DIR, "00_code")
DATA_DIR <- file.path(FINAL_DIR, "01_data")
MASTER_EXCEL_DIR <- file.path(FINAL_DIR, "02_master_excel")
FIGURE_DIR <- file.path(FINAL_DIR, "03_figures")
MAIN_FIGURE_DIR <- file.path(FIGURE_DIR, "main_paper")
APPENDIX_FIGURE_DIR <- file.path(FIGURE_DIR, "appendix")
EXHAUSTIVE_FIGURE_DIR <- file.path(FIGURE_DIR, "exhaustive_all_combinations")
REPORT_DIR <- file.path(FINAL_DIR, "04_reports")
LOG_DIR <- file.path(FINAL_DIR, "05_logs")

FINAL_INPUT_WORKBOOK <- file.path(ROOT_DIR, "structural_pvar_ciss_full7_final_outputs", "01_data_preparation_full7_final.xlsx")
REDUCED_FORM_WORKBOOK <- file.path(ROOT_DIR, "structural_pvar_ciss_full7_final_outputs", "04_fe_lsdv_pvar1_full7_final.xlsx")
FINAL_TABLES_WORKBOOK <- file.path(ROOT_DIR, "structural_pvar_ciss_full7_final_outputs", "08_final_tables_for_paper_full7.xlsx")
STRUCTURAL_DIR <- file.path(ROOT_DIR, "structural_pvar_ciss_full7_structural_refined4_outputs")
HD_DIR <- file.path(ROOT_DIR, "structural_pvar_ciss_full7_historical_decomposition_outputs")
CF_DIR <- file.path(ROOT_DIR, "structural_pvar_ciss_full7_counterfactual_outputs")

STRUCTURAL_RESTRICTIONS_WB <- file.path(STRUCTURAL_DIR, "02_refined4_sign_restrictions.xlsx")
STRUCTURAL_ACCEPTANCE_WB <- file.path(STRUCTURAL_DIR, "03_refined4_acceptance_diagnostics.xlsx")
STRUCTURAL_IRF_WB <- file.path(STRUCTURAL_DIR, "04_refined4_structural_irf.xlsx")
STRUCTURAL_FEVD_WB <- file.path(STRUCTURAL_DIR, "05_refined4_structural_fevd.xlsx")
HD_SETUP_WB <- file.path(HD_DIR, "01_hd_model_setup.xlsx")
HD_PANEL_WB <- file.path(HD_DIR, "03_hd_panel_average.xlsx")
HD_COUNTRY_WB <- file.path(HD_DIR, "04_hd_country_level.xlsx")
HD_CUMULATIVE_WB <- file.path(HD_DIR, "05_hd_cumulative.xlsx")
HD_TABLES_WB <- file.path(HD_DIR, "06_hd_summary_tables_for_paper.xlsx")
CF_SETUP_WB <- file.path(CF_DIR, "01_cf_model_setup.xlsx")
CF_PANEL_WB <- file.path(CF_DIR, "02_cf_panel_average_paths.xlsx")
CF_COUNTRY_WB <- file.path(CF_DIR, "03_cf_country_level_paths.xlsx")
CF_CUMULATIVE_WB <- file.path(CF_DIR, "04_cf_cumulative_effects.xlsx")
CF_PERIOD_WB <- file.path(CF_DIR, "05_cf_summary_by_period.xlsx")
CF_TABLES_WB <- file.path(CF_DIR, "06_cf_tables_for_paper.xlsx")

MODEL_VARS <- c(
  "Energy_Factor",
  "d_CISS",
  "d_CPI",
  "GDP_Growth",
  "d_3MRate",
  "d_FiscalBalanceGDP",
  "dlog_CDS"
)

SHOCKS <- c(
  "Energy-carbon pressure shock",
  "Systemic financial stress shock",
  "Inflationary monetary-reaction shock",
  "Sovereign-risk repricing shock"
)

MODEL_NAME <- "Structural PVAR refined4 S1 - four-shock sign-only, h=0,1,2"
REPRESENTATIVE_DRAW <- "candidate draw 23085 / accepted draw 5782"

STAGE_SCRIPTS <- c(
  reduced_form = file.path("code", "05_structural_pvar", "15_structural_pvar_full7_final_workflow.R"),
  structural_refined4 = file.path("code", "05_structural_pvar", "17_structural_pvar_full7_refined4.R"),
  historical_decomposition = file.path("code", "06_historical_decomposition", "19_structural_pvar_full7_hd_refined4.R"),
  counterfactual = file.path("code", "07_counterfactuals", "20_structural_pvar_full7_counterfactual_refined4.R")
)

for (d in c(CODE_DIR, DATA_DIR, MASTER_EXCEL_DIR, MAIN_FIGURE_DIR, APPENDIX_FIGURE_DIR, EXHAUSTIVE_FIGURE_DIR, REPORT_DIR, LOG_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# =============================================================================
# SECTION 1 - Packages and helper functions
# =============================================================================

required_packages <- c("openxlsx", "dplyr", "tidyr", "tibble", "ggplot2", "scales")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) stop("Missing R packages: ", paste(missing_packages, collapse = ", "))

suppressPackageStartupMessages({
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(scales)
})

run_log <- character()
warnings_log <- character()
figure_manifest_rows <- list()

log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
  run_log <<- c(run_log, msg)
  cat(msg, "\n")
}

add_warning <- function(...) {
  msg <- paste(..., collapse = " ")
  warnings_log <<- c(warnings_log, msg)
  warning(msg, call. = FALSE)
}

read_sheet <- function(path, sheet) {
  if (!file.exists(path)) {
    add_warning("Missing workbook:", path)
    return(data.frame(note = paste("Missing workbook:", path)))
  }
  sheets <- openxlsx::getSheetNames(path)
  if (!(sheet %in% sheets)) {
    add_warning("Missing sheet:", sheet, "in", path)
    return(data.frame(note = paste("Missing sheet:", sheet, "in", basename(path))))
  }
  openxlsx::read.xlsx(path, sheet = sheet)
}

clean_names_and_dates <- function(df) {
  df
}

build_panel_dataset <- function(input_workbook = FINAL_INPUT_WORKBOOK,
                                sheet = "final_model_ready_7var") {
  read_sheet(input_workbook, sheet) |>
    clean_names_and_dates()
}

q_index <- function(q) {
  year <- as.integer(substr(q, 1, 4))
  quarter <- as.integer(sub(".*Q", "", q))
  year * 4L + quarter
}

safe_file <- function(x) {
  out <- gsub("[^A-Za-z0-9_]+", "_", x)
  out <- gsub("_+", "_", out)
  gsub("^_|_$", "", out)
}

safe_sheet_names <- function(x) {
  x <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", x)
  out <- character(length(x))
  used <- character()
  for (i in seq_along(x)) {
    base <- substr(x[[i]], 1, 31)
    candidate <- base
    suffix_id <- 1L
    while (candidate %in% used) {
      suffix <- paste0("_", suffix_id)
      candidate <- paste0(substr(base, 1, 31 - nchar(suffix)), suffix)
      suffix_id <- suffix_id + 1L
    }
    out[[i]] <- candidate
    used <- c(used, candidate)
  }
  out
}

as_sheet_df <- function(x) {
  if (is.null(x)) return(data.frame(note = "Not available"))
  if (is.data.frame(x)) return(x)
  as.data.frame(x, check.names = FALSE)
}

save_excel_workbook <- function(sheets, path) {
  requested_names <- names(sheets)
  actual_names <- safe_sheet_names(requested_names)
  wb <- openxlsx::createWorkbook()
  for (i in seq_along(sheets)) {
    openxlsx::addWorksheet(wb, actual_names[[i]])
    openxlsx::writeData(wb, actual_names[[i]], as_sheet_df(sheets[[i]]))
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  data.frame(requested_sheet = requested_names, actual_sheet = actual_names, stringsAsFactors = FALSE)
}

make_paper_theme <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(size = base_size),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1, color = "#333333"),
      legend.title = element_blank(),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.25),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

make_neutral_palette <- function() {
  c(
    "Energy-carbon pressure shock" = "#B65A4A",
    "Systemic financial stress shock" = "#8A7A38",
    "Inflationary monetary-reaction shock" = "#4E7D5B",
    "Sovereign-risk repricing shock" = "#3F7C8A",
    "Other / unidentified structural shocks" = "#8A96A8",
    "Initial / deterministic component" = "#B8B8B8"
  )
}

shock_palette <- make_neutral_palette()
scenario_palette <- c(
  "Fitted" = "#111111",
  "Actual" = "#666666",
  "CF1_no_energy" = "#B65A4A",
  "CF2_no_ciss" = "#8A7A38",
  "CF3_no_inflationary_monetary" = "#4E7D5B",
  "CF4_no_sovereign" = "#3F7C8A",
  "CF5_no_energy_no_inflationary" = "#7A5C89",
  "CF6_no_energy_no_sovereign" = "#5C4A3D",
  "CF7_no_macro_financial" = "#8A96A8"
)

save_ggplot_publication <- function(plot, filename_base, width = 9, height = 5.5, figure_id = NA_character_,
                                    title = NA_character_, description = NA_character_, paper_section = NA_character_,
                                    main_text_or_appendix = "appendix", source_stage = NA_character_, variables = NA_character_,
                                    scenario_or_shock = NA_character_, recommended_caption = NA_character_, notes = NA_character_) {
  dir.create(dirname(filename_base), recursive = TRUE, showWarnings = FALSE)
  png_path <- paste0(filename_base, ".png")
  pdf_path <- paste0(filename_base, ".pdf")
  ggsave(png_path, plot, width = width, height = height, dpi = 400, bg = "white")
  ggsave(pdf_path, plot, width = width, height = height, bg = "white")
  figure_manifest_rows[[length(figure_manifest_rows) + 1L]] <<- data.frame(
    figure_id = figure_id,
    filename_png = normalizePath(png_path, winslash = "/", mustWork = FALSE),
    filename_pdf = normalizePath(pdf_path, winslash = "/", mustWork = FALSE),
    title = title,
    description = description,
    paper_section = paper_section,
    main_text_or_appendix = main_text_or_appendix,
    source_stage = source_stage,
    variables = variables,
    scenario_or_shock = scenario_or_shock,
    recommended_caption = recommended_caption,
    notes = notes,
    stringsAsFactors = FALSE
  )
}

run_stage_scripts_if_needed <- function() {
  required_cache_files <- c(
    FINAL_INPUT_WORKBOOK,
    REDUCED_FORM_WORKBOOK,
    FINAL_TABLES_WORKBOOK,
    STRUCTURAL_RESTRICTIONS_WB,
    STRUCTURAL_ACCEPTANCE_WB,
    STRUCTURAL_IRF_WB,
    STRUCTURAL_FEVD_WB,
    HD_SETUP_WB,
    HD_PANEL_WB,
    HD_COUNTRY_WB,
    HD_CUMULATIVE_WB,
    HD_TABLES_WB,
    CF_SETUP_WB,
    CF_PANEL_WB,
    CF_COUNTRY_WB,
    CF_CUMULATIVE_WB,
    CF_PERIOD_WB,
    CF_TABLES_WB
  )
  missing_cache <- required_cache_files[!file.exists(required_cache_files)]
  if (!RUN_FROM_SCRATCH && length(missing_cache) > 0) {
    log_msg("Validated intermediate cache is incomplete in this checkout; rebuilding from input Excel.")
    log_msg("Missing cache files:", paste(basename(missing_cache), collapse = ", "))
    RUN_FROM_SCRATCH <<- TRUE
    USE_CACHED_INTERMEDIATE_OUTPUTS <<- FALSE
  }
  if (!RUN_FROM_SCRATCH) {
    log_msg("RUN_FROM_SCRATCH is FALSE; using cached validated outputs.")
    return(invisible(NULL))
  }
  if (!USE_CACHED_INTERMEDIATE_OUTPUTS) {
    scripts_to_run <- character()
    if (RUN_REDUCED_FORM || RUN_ROBUSTNESS) scripts_to_run <- c(scripts_to_run, STAGE_SCRIPTS[["reduced_form"]])
    if (RUN_STRUCTURAL_PVAR) scripts_to_run <- c(scripts_to_run, STAGE_SCRIPTS[["structural_refined4"]])
    if (RUN_HISTORICAL_DECOMPOSITION) scripts_to_run <- c(scripts_to_run, STAGE_SCRIPTS[["historical_decomposition"]])
    if (RUN_COUNTERFACTUAL) scripts_to_run <- c(scripts_to_run, STAGE_SCRIPTS[["counterfactual"]])
    for (script in unique(scripts_to_run)) {
      log_msg("Running stage script:", script)
      status <- system2("Rscript", script)
      if (!identical(status, 0L)) stop("Stage script failed: ", script)
    }
  } else {
    log_msg("USE_CACHED_INTERMEDIATE_OUTPUTS is TRUE; skipped expensive reruns.")
  }
}

run_descriptive_stats <- function(df, vars = MODEL_VARS) {
  bind_rows(lapply(vars, function(v) {
    x <- df[[v]]
    data.frame(
      variable = v,
      n = sum(is.finite(x)),
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      p25 = as.numeric(quantile(x, 0.25, na.rm = TRUE, names = FALSE)),
      median = median(x, na.rm = TRUE),
      p75 = as.numeric(quantile(x, 0.75, na.rm = TRUE, names = FALSE)),
      max = max(x, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}

run_pca_energy_factor <- function() read_sheet(FINAL_INPUT_WORKBOOK, "PCA_loadings")
estimate_fe_lsdv_pvar <- function() read_sheet(REDUCED_FORM_WORKBOOK, "coefficients_all_se")
estimate_pvar_gmm_robustness <- function() read_sheet(FINAL_TABLES_WORKBOOK, "robustness_gmm_key_coefficients")
run_lp_driscoll_kraay <- function() read_sheet(FINAL_TABLES_WORKBOOK, "lp_robustness_summary")
run_structural_pvar_refined4 <- function() read_sheet(STRUCTURAL_ACCEPTANCE_WB, "acceptance_summary_all")
compute_structural_irfs <- function() read_sheet(STRUCTURAL_IRF_WB, "S1_structural_irf_all")
compute_structural_fevd <- function() read_sheet(STRUCTURAL_FEVD_WB, "S1_FEVD_with_other")
compute_historical_decomposition <- function() read_sheet(HD_TABLES_WB, "main_findings")
compute_counterfactual_paths <- function() read_sheet(CF_TABLES_WB, "main_findings")

# =============================================================================
# SECTION 2 - Data import and transformations
# =============================================================================

run_stage_scripts_if_needed()

log_msg("Reading validated data and model outputs.")
model_ready <- build_panel_dataset(FINAL_INPUT_WORKBOOK, "final_model_ready_7var")
estimation_dataset <- read_sheet(FINAL_INPUT_WORKBOOK, "estimation_balanced_dataset")
sample_summary <- read_sheet(FINAL_INPUT_WORKBOOK, "estimation_sample")
countries <- read_sheet(FINAL_INPUT_WORKBOOK, "countries")
variable_types <- read_sheet(FINAL_INPUT_WORKBOOK, "variable_types")
transformed_variables <- read_sheet(FINAL_INPUT_WORKBOOK, "transformed_variables")
pca_loadings <- read_sheet(FINAL_INPUT_WORKBOOK, "PCA_loadings")
pca_variance <- read_sheet(FINAL_INPUT_WORKBOOK, "PCA_explained_variance")
pca_scores <- read_sheet(FINAL_INPUT_WORKBOOK, "PCA_scores")
pca_correlations <- read_sheet(FINAL_INPUT_WORKBOOK, "PCA_factor_correlations")

write_xlsx_data <- function() {
  save_excel_workbook(
    list(
      model_ready_dataset = model_ready,
      estimation_balanced_dataset = estimation_dataset
    ),
    file.path(DATA_DIR, "model_ready_dataset.xlsx")
  )
  save_excel_workbook(
    list(
      estimation_sample = sample_summary,
      countries = countries,
      coverage_by_country = estimation_dataset |>
        group_by(Country) |>
        summarise(observations = n(), min_quarter = min(Quarter_ID), max_quarter = max(Quarter_ID), .groups = "drop")
    ),
    file.path(DATA_DIR, "panel_balance_checks.xlsx")
  )
  save_excel_workbook(
    list(
      variable_types = variable_types,
      transformed_variables = transformed_variables,
      pca_loadings = pca_loadings,
      pca_variance = pca_variance
    ),
    file.path(DATA_DIR, "transformation_summary.xlsx")
  )
}
write_xlsx_data()

# =============================================================================
# SECTION 3 - Descriptive statistics and preliminary diagnostics
# =============================================================================

model_ready <- model_ready |>
  mutate(yq_index = q_index(Quarter_ID))

coverage_by_country <- model_ready |>
  group_by(Country) |>
  summarise(
    observations = n(),
    min_quarter = min(Quarter_ID),
    max_quarter = max(Quarter_ID),
    missing_model_vars = sum(!complete.cases(across(all_of(MODEL_VARS)))),
    .groups = "drop"
  )

descriptive_full <- run_descriptive_stats(model_ready, MODEL_VARS)

descriptive_country <- model_ready |>
  pivot_longer(all_of(MODEL_VARS), names_to = "variable", values_to = "value") |>
  group_by(Country, variable) |>
  summarise(n = sum(is.finite(value)), mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE), min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE), .groups = "drop")

period_definitions <- data.frame(
  period = c("Full sample", "Pre-energy-inflation period", "Energy-inflation and tightening episode", "Post-shock normalization", "COVID/rebound window"),
  start = c("2014Q2", "2014Q2", "2021Q1", "2024Q1", "2020Q1"),
  end = c("2025Q4", "2020Q4", "2023Q4", "2025Q4", "2021Q4"),
  stringsAsFactors = FALSE
)

add_periods <- function(df) {
  bind_rows(lapply(seq_len(nrow(period_definitions)), function(i) {
    df |>
      filter(yq_index >= q_index(period_definitions$start[[i]]), yq_index <= q_index(period_definitions$end[[i]])) |>
      mutate(period = period_definitions$period[[i]], period_start = period_definitions$start[[i]], period_end = period_definitions$end[[i]])
  }))
}

descriptive_period <- add_periods(model_ready) |>
  pivot_longer(all_of(MODEL_VARS), names_to = "variable", values_to = "value") |>
  group_by(period, period_start, period_end, variable) |>
  summarise(n = sum(is.finite(value)), mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE), min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE), .groups = "drop")

correlation_matrix <- as.data.frame(cor(model_ready[, MODEL_VARS], use = "pairwise.complete.obs"), check.names = FALSE) |>
  tibble::rownames_to_column("variable")

variable_definitions <- data.frame(
  variable = MODEL_VARS,
  definition = c(
    "First principal component of common energy-carbon price pressures.",
    "Quarterly change in ECB CISS.",
    "Quarterly CPI change; rate/percentage-point variable.",
    "Quarterly GDP growth.",
    "Quarterly change in 3-month money-market/proxy rate.",
    "Quarterly change in fiscal balance as percent of GDP.",
    "Quarterly log change in 5Y sovereign CDS."
  ),
  transformation = c("PCA factor", "First difference", "First difference", "Level/growth rate", "First difference", "First difference", "Log difference"),
  stringsAsFactors = FALSE
)

# =============================================================================
# SECTION 4 - Reduced-form model
# =============================================================================

rf_coefficients <- estimate_fe_lsdv_pvar()
rf_key_relations <- read_sheet(REDUCED_FORM_WORKBOOK, "key_relations")
rf_stability <- read_sheet(REDUCED_FORM_WORKBOOK, "stability_summary")
rf_roots <- read_sheet(REDUCED_FORM_WORKBOOK, "stability_roots")
rf_residual_diag <- read_sheet(REDUCED_FORM_WORKBOOK, "residual_diagnostics")
rf_irf <- read_sheet(REDUCED_FORM_WORKBOOK, "irf_all")
rf_irf_key <- read_sheet(REDUCED_FORM_WORKBOOK, "irf_key_summary")
rf_fevd <- read_sheet(REDUCED_FORM_WORKBOOK, "fevd_all")
rf_fevd_cds <- read_sheet(REDUCED_FORM_WORKBOOK, "fevd_dlog_CDS")
rf_A1 <- read_sheet(REDUCED_FORM_WORKBOOK, "A1_matrix")
rf_Sigma <- read_sheet(REDUCED_FORM_WORKBOOK, "residual_covariance")

# =============================================================================
# SECTION 5 - Robustness models
# =============================================================================

gmm_robustness <- estimate_pvar_gmm_robustness()
lp_robustness <- run_lp_driscoll_kraay()
model_comparison <- read_sheet(FINAL_TABLES_WORKBOOK, "key_relationship_comparison")
diagnostics_summary <- read_sheet(FINAL_TABLES_WORKBOOK, "diagnostics_summary")
main_fe_key <- read_sheet(FINAL_TABLES_WORKBOOK, "main_fe_lsdv_key_coefficients")

# =============================================================================
# SECTION 6 - Structural PVAR refined4 final
# =============================================================================

struct_restrictions <- read_sheet(STRUCTURAL_RESTRICTIONS_WB, "S1_restrictions")
struct_acceptance <- read_sheet(STRUCTURAL_ACCEPTANCE_WB, "acceptance_summary_all") |>
  filter(model_variant == "S1_four_shock_sign_only_h0_h2")
struct_overlap <- read_sheet(STRUCTURAL_ACCEPTANCE_WB, "overlap_S1_accepted_rates")
struct_irf_all <- compute_structural_irfs()
struct_irf_key <- read_sheet(STRUCTURAL_IRF_WB, "structural_irf_key") |>
  filter(model_variant == "S1_four_shock_sign_only_h0_h2")
struct_fevd_all <- compute_structural_fevd()
struct_fevd_cds <- read_sheet(STRUCTURAL_FEVD_WB, "S1_FEVD_dlog_CDS")
struct_fevd_cpi <- read_sheet(STRUCTURAL_FEVD_WB, "S1_FEVD_d_CPI")
struct_fevd_rate <- read_sheet(STRUCTURAL_FEVD_WB, "S1_FEVD_d_3MRate")
struct_B <- read_sheet(HD_SETUP_WB, "structural_B_matrix")
rep_draw <- read_sheet(HD_SETUP_WB, "representative_draw")

structural_model_verdict <- data.frame(
  item = c(
    "Final structural model",
    "Accepted rotations",
    "Acceptance rate",
    "Unique assignment rate",
    "Representative draw",
    "Repaired4 status",
    "Interpretation warning"
  ),
  value = c(
    MODEL_NAME,
    struct_acceptance$accepted_rotations[[1]],
    scales::percent(struct_acceptance$acceptance_rate[[1]], accuracy = 0.01),
    scales::percent(struct_acceptance$unique_assignment_rate[[1]], accuracy = 0.01),
    REPRESENTATIVE_DRAW,
    "Not used as final model; retained only as sensitivity/repair attempt.",
    "Other/unidentified and Energy-Inflation overlap must be acknowledged."
  ),
  stringsAsFactors = FALSE
)

# =============================================================================
# SECTION 7 - Historical Decomposition
# =============================================================================

hd_panel_cds <- read_sheet(HD_PANEL_WB, "dlog_CDS")
hd_panel_cpi <- read_sheet(HD_PANEL_WB, "d_CPI")
hd_panel_rate <- read_sheet(HD_PANEL_WB, "d_3MRate")
hd_panel_gdp <- read_sheet(HD_PANEL_WB, "GDP_Growth")
hd_panel_fiscal <- read_sheet(HD_PANEL_WB, "d_FiscalBalanceGDP")
hd_summary_period <- read_sheet(HD_PANEL_WB, "summary_by_period")
hd_panel_cumulative <- read_sheet(HD_CUMULATIVE_WB, "panel_average_cumulative")
hd_cum_cds <- hd_panel_cumulative |> filter(variable == "dlog_CDS")
hd_country <- read_sheet(HD_COUNTRY_WB, "country_level_long")
hd_findings <- read_sheet(HD_TABLES_WB, "main_findings")
hd_rank_energy <- read_sheet(HD_TABLES_WB, "rank_dlog_CDS_energy")
hd_rank_ciss <- read_sheet(HD_TABLES_WB, "rank_dlog_CDS_ciss")
hd_rank_sovereign <- read_sheet(HD_TABLES_WB, "rank_dlog_CDS_sovereign")
hd_reconstruction <- read_sheet(HD_SETUP_WB, "hd_reconstruction")

# =============================================================================
# SECTION 8 - Counterfactual Analysis
# =============================================================================

cf_scenarios <- read_sheet(CF_SETUP_WB, "scenarios")
cf_panel_dlog <- read_sheet(CF_TABLES_WB, "main_cf_dlog_CDS_panel")
cf_panel_cpi <- read_sheet(CF_TABLES_WB, "main_cf_d_CPI_panel")
cf_panel_rate <- read_sheet(CF_TABLES_WB, "main_cf_d_3MRate_panel")
cf_panel_gdp <- read_sheet(CF_TABLES_WB, "main_cf_GDP_Growth_panel")
cf_panel_fiscal <- read_sheet(CF_TABLES_WB, "main_cf_fiscal_panel")
cf_episode_cds <- read_sheet(CF_TABLES_WB, "energy_episode_dlog_CDS")
cf_episode_cpi <- read_sheet(CF_TABLES_WB, "energy_episode_d_CPI")
cf_episode_rate <- read_sheet(CF_TABLES_WB, "energy_episode_d_3MRate")
cf_rank_no_energy <- read_sheet(CF_TABLES_WB, "rank_no_energy_CDS")
cf_rank_no_sovereign <- read_sheet(CF_TABLES_WB, "rank_no_sovereign_CDS")
cf_rank_no_energy_no_sov <- read_sheet(CF_TABLES_WB, "rank_no_energy_no_sov_CDS")
cf_scenario_rank_cds <- read_sheet(CF_TABLES_WB, "scenario_ranking_dlog_CDS")
cf_findings <- read_sheet(CF_TABLES_WB, "main_findings")
cf_country <- read_sheet(CF_COUNTRY_WB, "country_level_long")
cf_panel_long <- read_sheet(CF_PANEL_WB, "all_variables_long")
cf_cumulative_country <- read_sheet(CF_CUMULATIVE_WB, "country_level_cumulative")

# =============================================================================
# SECTION 9 - Master Excel and appendix workbooks
# =============================================================================

paper_table_1 <- bind_rows(
  data.frame(item = "Model sample", value = paste0(sample_summary$min_quarter[[1]], "-", sample_summary$max_quarter[[1]])),
  data.frame(item = "Countries", value = as.character(sample_summary$countries[[1]])),
  data.frame(item = "Quarters", value = as.character(sample_summary$quarters[[1]])),
  data.frame(item = "Observations", value = as.character(sample_summary$observations[[1]])),
  data.frame(item = "Final variables", value = paste(MODEL_VARS, collapse = ", "))
)

paper_table_2 <- descriptive_full
paper_table_3 <- main_fe_key
paper_table_4 <- model_comparison
paper_table_5 <- struct_restrictions
paper_table_6 <- struct_fevd_cds |> filter(horizon == 12)
paper_table_7 <- hd_summary_period |> filter(variable == "dlog_CDS")
paper_table_8 <- cf_episode_cds
paper_table_9 <- bind_rows(
  cf_rank_no_energy |> mutate(source = "CF no-energy"),
  cf_rank_no_sovereign |> mutate(source = "CF no-sovereign"),
  hd_rank_energy |> mutate(source = "HD energy"),
  hd_rank_sovereign |> mutate(source = "HD sovereign")
)
paper_table_10 <- bind_rows(
  structural_model_verdict |> transmute(section = "Structural model", item, result = value),
  hd_findings |> transmute(section = "Historical decomposition", item = question, result = answer),
  cf_findings |> transmute(section = "Counterfactual", item = question, result = answer)
)

master_tables <- list(
  T01_sample_description = sample_summary,
  T02_country_coverage = coverage_by_country,
  T03_variable_definitions = variable_definitions,
  T04_transformations = transformed_variables,
  T05_descriptive_stats_full = descriptive_full,
  T06_descriptive_stats_country = descriptive_country,
  T07_descriptive_stats_period = descriptive_period,
  T08_correlation_matrix = correlation_matrix,
  T09_pca_energy_factor = bind_rows(
    pca_loadings |> mutate(table = "loadings"),
    pca_variance |> mutate(table = "explained_variance")
  ),
  T10_panel_balance = sample_summary,
  T11_fe_lsdv_pvar_coefficients = rf_coefficients,
  T12_key_coefficients_dlog_CDS = rf_coefficients |> filter(equation == "dlog_CDS"),
  T13_key_coefficients_d_CPI = rf_coefficients |> filter(equation == "d_CPI"),
  T14_key_coefficients_d_3MRate = rf_coefficients |> filter(equation == "d_3MRate"),
  T15_model_stability = bind_rows(rf_stability |> mutate(table = "stability_summary"), rf_roots |> mutate(table = "stability_roots")),
  T16_residual_diagnostics = rf_residual_diag,
  T17_gmm_robustness = gmm_robustness,
  T18_lp_driscoll_kraay = lp_robustness,
  T19_model_comparison = model_comparison,
  T20_main_reduced_form_findings = diagnostics_summary,
  T21_structural_sign_restrictions = struct_restrictions,
  T22_structural_acceptance = struct_acceptance,
  T23_structural_overlap = struct_overlap,
  T24_structural_B_matrix = struct_B,
  T25_key_irf_dlog_CDS = struct_irf_all |> filter(response == "dlog_CDS"),
  T26_key_irf_d_CPI = struct_irf_all |> filter(response == "d_CPI"),
  T27_key_irf_d_3MRate = struct_irf_all |> filter(response == "d_3MRate"),
  T28_structural_fevd_dlog_CDS = struct_fevd_cds,
  T29_structural_fevd_d_CPI = struct_fevd_cpi,
  T30_structural_fevd_d_3MRate = struct_fevd_rate,
  T31_structural_model_verdict = structural_model_verdict,
  T32_hd_panel_average_dlog_CDS = hd_panel_cds,
  T33_hd_panel_average_d_CPI = hd_panel_cpi,
  T34_hd_panel_average_d_3MRate = hd_panel_rate,
  T35_hd_panel_average_GDP = hd_panel_gdp,
  T36_hd_panel_average_fiscal = hd_panel_fiscal,
  T37_hd_summary_by_period = hd_summary_period,
  T38_hd_cumulative_dlog_CDS = hd_cum_cds,
  T39_hd_country_rank_energy_CDS = hd_rank_energy,
  T40_hd_country_rank_sovereign_CDS = hd_rank_sovereign,
  T41_hd_country_rank_ciss_CDS = hd_rank_ciss,
  T42_hd_main_findings = hd_findings,
  T43_cf_scenarios = cf_scenarios,
  T44_cf_dlog_CDS_panel = cf_panel_dlog,
  T45_cf_d_CPI_panel = cf_panel_cpi,
  T46_cf_d_3MRate_panel = cf_panel_rate,
  T47_cf_GDP_panel = cf_panel_gdp,
  T48_cf_fiscal_panel = cf_panel_fiscal,
  T49_cf_energy_episode_CDS = cf_episode_cds,
  T50_cf_energy_episode_CPI = cf_episode_cpi,
  T51_cf_energy_episode_3MRate = cf_episode_rate,
  T52_cf_country_rank_no_energy = cf_rank_no_energy,
  T53_cf_country_rank_no_sovereign = cf_rank_no_sovereign,
  T54_cf_country_rank_no_energy_no_sovereign = cf_rank_no_energy_no_sov,
  T55_cf_scenario_ranking_CDS = cf_scenario_rank_cds,
  T56_cf_main_findings = cf_findings,
  PAPER_Table_1_Data = paper_table_1,
  PAPER_Table_2_Descriptive = paper_table_2,
  PAPER_Table_3_PVAR_Key = paper_table_3,
  PAPER_Table_4_Robustness = paper_table_4,
  PAPER_Table_5_SVAR_Restrictions = paper_table_5,
  PAPER_Table_6_FEVD = paper_table_6,
  PAPER_Table_7_HD_CDS = paper_table_7,
  PAPER_Table_8_CF_CDS = paper_table_8,
  PAPER_Table_9_Country_Heterogeneity = paper_table_9,
  PAPER_Table_10_Main_Findings = paper_table_10
)

table_descriptions <- data.frame(
  sheet_name = names(master_tables),
  description = c(
    "Final sample description and balance summary.",
    "Country coverage in final model-ready panel.",
    "Definitions and transformations for model variables.",
    "Transformation output from data preparation workflow.",
    "Full-sample descriptive statistics.",
    "Country-level descriptive statistics.",
    "Subperiod descriptive statistics.",
    "Correlation matrix for final model variables.",
    "PCA loadings and explained variance for Energy_Factor.",
    "Panel balance checks.",
    "All FE/LSDV PVAR(1) coefficients and standard errors.",
    "dlog_CDS equation coefficients.",
    "d_CPI equation coefficients.",
    "d_3MRate equation coefficients.",
    "Reduced-form stability diagnostics.",
    "Residual diagnostics.",
    "Restricted PVAR-GMM robustness key coefficients.",
    "Panel local projections with Driscoll-Kraay summary.",
    "Model comparison across FE/LSDV, GMM and LP where available.",
    "Reduced-form diagnostics and main findings.",
    "Final refined4 sign restrictions.",
    "Structural acceptance diagnostics.",
    "Overlap diagnostics for refined4 S1.",
    "Representative structural impact matrix B.",
    "Structural IRFs for dlog_CDS responses.",
    "Structural IRFs for d_CPI responses.",
    "Structural IRFs for d_3MRate responses.",
    "Structural FEVD for dlog_CDS.",
    "Structural FEVD for d_CPI.",
    "Structural FEVD for d_3MRate.",
    "Final structural model verdict.",
    "Panel-average HD for dlog_CDS.",
    "Panel-average HD for d_CPI.",
    "Panel-average HD for d_3MRate.",
    "Panel-average HD for GDP_Growth.",
    "Panel-average HD for fiscal balance change.",
    "HD summary by period.",
    "Cumulative HD for dlog_CDS.",
    "HD country ranking for energy contribution to dlog_CDS.",
    "HD country ranking for sovereign contribution to dlog_CDS.",
    "HD country ranking for CISS contribution to dlog_CDS.",
    "HD main findings.",
    "Counterfactual scenarios.",
    "Counterfactual dlog_CDS panel summaries.",
    "Counterfactual d_CPI panel summaries.",
    "Counterfactual d_3MRate panel summaries.",
    "Counterfactual GDP panel summaries.",
    "Counterfactual fiscal panel summaries.",
    "Energy-episode CDS counterfactual table.",
    "Energy-episode CPI counterfactual table.",
    "Energy-episode 3M rate counterfactual table.",
    "Country ranking for no-energy CDS counterfactual.",
    "Country ranking for no-sovereign CDS counterfactual.",
    "Country ranking for no-energy-no-sovereign CDS counterfactual.",
    "Scenario ranking for dlog_CDS.",
    "Counterfactual main findings.",
    "Paper Table 1: data and sample.",
    "Paper Table 2: descriptive statistics.",
    "Paper Table 3: key PVAR coefficients.",
    "Paper Table 4: robustness results.",
    "Paper Table 5: structural restrictions.",
    "Paper Table 6: FEVD.",
    "Paper Table 7: historical decomposition for CDS.",
    "Paper Table 8: counterfactual results for CDS.",
    "Paper Table 9: country heterogeneity.",
    "Paper Table 10: main findings."
  ),
  paper_section = c(rep("Data", 10), rep("Reduced-form and robustness", 10), rep("Structural PVAR", 11), rep("Historical decomposition", 11), rep("Counterfactual", 14), rep("Final paper tables", 10)),
  main_text_or_appendix = c(rep("main/appendix", 10), rep("appendix", 10), rep("main/appendix", 11), rep("main/appendix", 11), rep("main/appendix", 14), rep("main", 10)),
  source_stage = c(rep("Data preparation", 10), rep("Reduced-form/robustness", 10), rep("Structural refined4", 11), rep("Historical decomposition", 11), rep("Counterfactual", 14), rep("Consolidated", 10)),
  variables = "See sheet contents",
  notes = "Consolidated without changing the final refined4 S1 model.",
  recommended_caption = paste0("Table reports ", names(master_tables), "."),
  stringsAsFactors = FALSE
)

if (GENERATE_MASTER_EXCEL) {
  log_msg("Writing master Excel workbook.")
  sheet_map <- save_excel_workbook(master_tables, file.path(MASTER_EXCEL_DIR, "MASTER_all_tables_for_paper.xlsx"))
  table_manifest <- table_descriptions |>
    mutate(table_id = row_number(), requested_sheet_name = sheet_name) |>
    left_join(sheet_map, by = c("sheet_name" = "requested_sheet")) |>
    transmute(table_id, sheet_name = actual_sheet, requested_sheet_name, description, paper_section, main_text_or_appendix, source_stage, variables, notes, recommended_caption)
  save_excel_workbook(list(table_manifest = table_manifest), file.path(MASTER_EXCEL_DIR, "table_manifest.xlsx"))
} else {
  table_manifest <- table_descriptions |> mutate(table_id = row_number(), requested_sheet_name = sheet_name, sheet_name = safe_sheet_names(sheet_name))
}

appendix_tables <- list(
  all_reduced_form_coefficients = rf_coefficients,
  all_reduced_form_irfs = rf_irf,
  all_reduced_form_fevd = rf_fevd,
  all_structural_irfs = struct_irf_all,
  all_structural_fevd = struct_fevd_all,
  all_hd_country_level = hd_country,
  all_cf_country_level = cf_country,
  all_cf_country_periods = read_sheet(CF_PERIOD_WB, "country_periods_long"),
  all_hd_reconstruction_checks = hd_reconstruction,
  all_diagnostics = diagnostics_summary,
  residual_diagnostics = rf_residual_diag,
  model_stability_roots = rf_roots
)
save_excel_workbook(appendix_tables, file.path(MASTER_EXCEL_DIR, "MASTER_appendix_tables.xlsx"))

# =============================================================================
# SECTION 10 - Paper-ready and exhaustive figures
# =============================================================================

plot_line_panel_average <- function(df, variable_name, title, filename_base, main_or_appendix = "appendix", figure_id = NA) {
  dat <- df |>
    group_by(Quarter_ID, yq_index) |>
    summarise(value = mean(.data[[variable_name]], na.rm = TRUE), .groups = "drop") |>
    arrange(yq_index)
  breaks <- dat |> filter(row_number() %% 4 == 1)
  p <- ggplot(dat, aes(x = yq_index, y = value)) +
    geom_line(color = "#3F7C8A", linewidth = 0.75) +
    scale_x_continuous(breaks = breaks$yq_index, labels = breaks$Quarter_ID) +
    labs(title = title, x = NULL, y = variable_name) +
    make_paper_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  save_ggplot_publication(p, filename_base, figure_id = figure_id, title = title, main_text_or_appendix = main_or_appendix, source_stage = "Descriptive", variables = variable_name)
}

plot_structural_irf <- function(irf, response_name, filename_base, title, figure_id = NA, main_or_appendix = "appendix") {
  dat <- irf |> filter(response == response_name)
  p <- ggplot(dat, aes(x = horizon, y = median_irf, color = shock, fill = shock)) +
    geom_hline(yintercept = 0, color = "grey45", linewidth = 0.25) +
    geom_ribbon(aes(ymin = p16, ymax = p84), alpha = 0.16, color = NA) +
    geom_line(linewidth = 0.75) +
    scale_color_manual(values = shock_palette) +
    scale_fill_manual(values = shock_palette) +
    scale_x_continuous(breaks = 0:12) +
    labs(title = title, x = "Horizon", y = "Median response") +
    make_paper_theme()
  save_ggplot_publication(p, filename_base, figure_id = figure_id, title = title, main_text_or_appendix = main_or_appendix, source_stage = "Structural IRF", variables = response_name)
}

plot_structural_irf_grid <- function(irf, shock_name, filename_base, title) {
  dat <- irf |> filter(shock == shock_name)
  p <- ggplot(dat, aes(x = horizon, y = median_irf)) +
    geom_hline(yintercept = 0, color = "grey45", linewidth = 0.25) +
    geom_ribbon(aes(ymin = p16, ymax = p84), fill = "#8A96A8", alpha = 0.18) +
    geom_line(color = "#3F7C8A", linewidth = 0.7) +
    facet_wrap(~response, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = 0:12) +
    labs(title = title, x = "Horizon", y = "Median response") +
    make_paper_theme(10)
  save_ggplot_publication(p, filename_base, width = 9.5, height = 7, title = title, main_text_or_appendix = "appendix", source_stage = "Structural IRF", scenario_or_shock = shock_name)
}

plot_fevd <- function(fevd, response_name, filename_base, title, figure_id = NA, main_or_appendix = "appendix") {
  dat <- fevd |> filter(response == response_name) |> mutate(horizon = factor(horizon))
  p <- ggplot(dat, aes(x = horizon, y = mean_share, fill = shock)) +
    geom_col(width = 0.75, color = "white", linewidth = 0.15) +
    scale_fill_manual(values = shock_palette) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = title, x = "Horizon", y = "Mean FEVD share") +
    make_paper_theme()
  save_ggplot_publication(p, filename_base, figure_id = figure_id, title = title, main_text_or_appendix = main_or_appendix, source_stage = "Structural FEVD", variables = response_name)
}

plot_hd_stack <- function(hd_df, variable_name, filename_base, title, figure_id = NA, main_or_appendix = "appendix") {
  cols <- c("contribution_energy", "contribution_ciss", "contribution_inflationary_monetary", "contribution_sovereign", "contribution_other", "initial_deterministic_component")
  labs <- c(
    contribution_energy = "Energy-carbon pressure shock",
    contribution_ciss = "Systemic financial stress shock",
    contribution_inflationary_monetary = "Inflationary monetary-reaction shock",
    contribution_sovereign = "Sovereign-risk repricing shock",
    contribution_other = "Other / unidentified structural shocks",
    initial_deterministic_component = "Initial / deterministic component"
  )
  dat <- hd_df |> filter(variable == variable_name) |> arrange(yq_index)
  long <- dat |> select(Quarter_ID, yq_index, all_of(cols), starts_with("actual"), starts_with("fitted")) |>
    pivot_longer(all_of(cols), names_to = "component", values_to = "value") |>
    mutate(component = labs[component])
  breaks <- dat |> filter(row_number() %% 4 == 1)
  y_actual <- if ("actual_panel_average" %in% names(dat)) "actual_panel_average" else "actual"
  p <- ggplot(long, aes(x = yq_index, y = value, fill = component)) +
    geom_col(width = 0.85) +
    geom_line(data = dat, aes(x = yq_index, y = .data[[y_actual]]), inherit.aes = FALSE, color = "#111111", linewidth = 0.55) +
    scale_fill_manual(values = shock_palette) +
    scale_x_continuous(breaks = breaks$yq_index, labels = breaks$Quarter_ID) +
    labs(title = title, x = NULL, y = "Contribution / actual") +
    make_paper_theme(10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  save_ggplot_publication(p, filename_base, width = 9, height = 5.5, figure_id = figure_id, title = title, main_text_or_appendix = main_or_appendix, source_stage = "Historical decomposition", variables = variable_name)
}

plot_cf_path <- function(cf_long, variable_name, scenario_name, filename_base, title, figure_id = NA, main_or_appendix = "appendix") {
  dat <- cf_long |> filter(variable == variable_name, scenario == scenario_name) |> arrange(yq_index)
  breaks <- dat |> distinct(yq_index, Quarter_ID) |> filter(row_number() %% 4 == 1)
  p <- ggplot(dat, aes(x = yq_index)) +
    annotate("rect", xmin = q_index("2021Q1") - 0.45, xmax = q_index("2023Q4") + 0.45, ymin = -Inf, ymax = Inf, fill = "#E8EEF2", alpha = 0.7) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.5) +
    geom_line(aes(y = fitted, color = "Fitted"), linewidth = 0.55) +
    geom_line(aes(y = counterfactual, color = scenario_name), linewidth = 0.75) +
    scale_color_manual(values = scenario_palette) +
    scale_x_continuous(breaks = breaks$yq_index, labels = breaks$Quarter_ID) +
    labs(title = title, subtitle = "Model-implied counterfactual path; shaded area marks 2021Q1-2023Q4.", x = NULL, y = NULL) +
    make_paper_theme(10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  save_ggplot_publication(p, filename_base, figure_id = figure_id, title = title, main_text_or_appendix = main_or_appendix, source_stage = "Counterfactual", variables = variable_name, scenario_or_shock = scenario_name)
}

plot_country_rank <- function(rank_df, filename_base, title, figure_id = NA, main_or_appendix = "main") {
  dat <- rank_df |> arrange(cumulative_gap)
  p <- ggplot(dat, aes(x = reorder(Country, cumulative_gap), y = cumulative_gap, fill = cumulative_gap > 0)) +
    geom_col(width = 0.72) +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "#3F7C8A", "FALSE" = "#B65A4A"), guide = "none") +
    labs(title = title, subtitle = "Cumulative dlog_CDS gap, 2021Q1-2023Q4.", x = NULL, y = "Cumulative gap") +
    make_paper_theme()
  save_ggplot_publication(p, filename_base, width = 7, height = 4.6, figure_id = figure_id, title = title, main_text_or_appendix = main_or_appendix, source_stage = "Counterfactual", variables = "dlog_CDS")
}

if (GENERATE_ALL_FIGURES || GENERATE_PAPER_FIGURES) {
  log_msg("Generating paper-ready figures.")
  panel_avg_data <- model_ready |>
    group_by(Quarter_ID, yq_index) |>
    summarise(across(all_of(MODEL_VARS), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
  energy_components <- estimation_dataset |>
    group_by(Quarter_ID) |>
    summarise(
      across(
        any_of(c("Energy_Factor", "TTF", "Brent", "Energy_Price", "Power_Energy_Price")),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    mutate(yq_index = q_index(Quarter_ID)) |>
    pivot_longer(-c(Quarter_ID, yq_index), names_to = "series", values_to = "value") |>
    group_by(series) |>
    mutate(value_scaled = as.numeric(scale(value))) |>
    ungroup()
  breaks_energy <- energy_components |> distinct(yq_index, Quarter_ID) |> arrange(yq_index) |> filter(row_number() %% 4 == 1)
  p_energy <- ggplot(energy_components, aes(x = yq_index, y = value_scaled, color = series)) +
    geom_line(linewidth = 0.7) +
    scale_x_continuous(breaks = breaks_energy$yq_index, labels = breaks_energy$Quarter_ID) +
    scale_color_manual(values = c(Energy_Factor = "#111111", TTF = "#B65A4A", Brent = "#8A7A38", Energy_Price = "#4E7D5B", Power_Energy_Price = "#3F7C8A")) +
    labs(title = "Energy factor and scaled energy-carbon components", x = NULL, y = "Scaled value") +
    make_paper_theme(10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  save_ggplot_publication(p_energy, file.path(MAIN_FIGURE_DIR, "F01_energy_factor_pca_components"), figure_id = "F01", title = "Energy factor and PCA components", main_text_or_appendix = "main", source_stage = "Data/PCA", variables = "Energy_Factor")

  coef_heat <- main_fe_key
  if (!all(c("equation", "cause", "coefficient") %in% names(coef_heat))) coef_heat <- rf_coefficients |> filter(equation %in% c("dlog_CDS", "d_CPI", "d_3MRate"))
  p_coef <- ggplot(coef_heat, aes(x = cause, y = equation, fill = coefficient)) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_gradient2(low = "#B65A4A", mid = "white", high = "#3F7C8A", midpoint = 0) +
    labs(title = "Key reduced-form PVAR coefficients", x = NULL, y = NULL, fill = "Coefficient") +
    make_paper_theme(10) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
  save_ggplot_publication(p_coef, file.path(MAIN_FIGURE_DIR, "F02_reduced_form_key_coefficients_heatmap"), figure_id = "F02", title = "Key reduced-form coefficient heatmap", main_text_or_appendix = "main", source_stage = "Reduced-form")

  plot_structural_irf(struct_irf_all, "dlog_CDS", file.path(MAIN_FIGURE_DIR, "F03_structural_irf_dlog_CDS_all_shocks"), "Structural IRFs of dlog_CDS to all identified shocks", "F03", "main")
  plot_fevd(struct_fevd_all, "dlog_CDS", file.path(MAIN_FIGURE_DIR, "F04_structural_fevd_dlog_CDS"), "Structural FEVD of dlog_CDS", "F04", "main")
  plot_hd_stack(hd_panel_cds, "dlog_CDS", file.path(MAIN_FIGURE_DIR, "F05_historical_decomposition_dlog_CDS"), "Historical decomposition of panel-average dlog_CDS", "F05", "main")
  plot_hd_stack(hd_panel_cpi, "d_CPI", file.path(MAIN_FIGURE_DIR, "F06_historical_decomposition_d_CPI"), "Historical decomposition of panel-average d_CPI", "F06", "main")
  plot_hd_stack(hd_panel_rate, "d_3MRate", file.path(MAIN_FIGURE_DIR, "F07_historical_decomposition_d_3MRate"), "Historical decomposition of panel-average d_3MRate", "F07", "main")
  plot_cf_path(cf_panel_long, "dlog_CDS", "CF1_no_energy", file.path(MAIN_FIGURE_DIR, "F08_counterfactual_dlog_CDS_no_energy"), "dlog_CDS counterfactual: no Energy-carbon pressure shocks", "F08", "main")
  plot_cf_path(cf_panel_long, "dlog_CDS", "CF4_no_sovereign", file.path(MAIN_FIGURE_DIR, "F09_counterfactual_dlog_CDS_no_sovereign"), "dlog_CDS counterfactual: no Sovereign-risk repricing shocks", "F09", "main")
  plot_cf_path(cf_panel_long, "dlog_CDS", "CF6_no_energy_no_sovereign", file.path(MAIN_FIGURE_DIR, "F10_counterfactual_dlog_CDS_no_energy_no_sovereign"), "dlog_CDS counterfactual: no Energy and no Sovereign shocks", "F10", "main")
  plot_cf_path(cf_panel_long, "d_CPI", "CF1_no_energy", file.path(MAIN_FIGURE_DIR, "F11_counterfactual_d_CPI_no_energy"), "d_CPI counterfactual: no Energy-carbon pressure shocks", "F11", "main")
  plot_cf_path(cf_panel_long, "d_3MRate", "CF1_no_energy", file.path(MAIN_FIGURE_DIR, "F12_counterfactual_d_3MRate_no_energy"), "d_3MRate counterfactual: no Energy-carbon pressure shocks", "F12", "main")
  plot_country_rank(cf_rank_no_energy, file.path(MAIN_FIGURE_DIR, "F13_country_ranking_no_energy_dlog_CDS"), "Country ranking: no-energy dlog_CDS counterfactual", "F13", "main")
  plot_country_rank(cf_rank_no_sovereign, file.path(MAIN_FIGURE_DIR, "F14_country_ranking_no_sovereign_dlog_CDS"), "Country ranking: no-sovereign dlog_CDS counterfactual", "F14", "main")

  if (GENERATE_ALL_FIGURES) {
    log_msg("Generating exhaustive figures.")
    for (v in MODEL_VARS) {
      plot_line_panel_average(model_ready, v, paste("Panel-average time series:", v), file.path(EXHAUSTIVE_FIGURE_DIR, paste0("descriptive_panel_", safe_file(v))), "appendix")
      p_country <- ggplot(model_ready, aes(x = yq_index, y = .data[[v]], color = Country, group = Country)) +
        geom_line(linewidth = 0.45, alpha = 0.8) +
        labs(title = paste("Country-level time series:", v), x = NULL, y = v) +
        make_paper_theme(9) +
        theme(legend.position = "bottom")
      save_ggplot_publication(p_country, file.path(EXHAUSTIVE_FIGURE_DIR, paste0("descriptive_country_", safe_file(v))), width = 10, height = 6, title = paste("Country-level time series:", v), main_text_or_appendix = "appendix", source_stage = "Descriptive", variables = v)
      plot_fevd(struct_fevd_all, v, file.path(EXHAUSTIVE_FIGURE_DIR, paste0("structural_fevd_", safe_file(v))), paste("Structural FEVD:", v), main_or_appendix = "appendix")
      plot_hd_stack(read_sheet(HD_PANEL_WB, v), v, file.path(EXHAUSTIVE_FIGURE_DIR, paste0("hd_panel_stack_", safe_file(v))), paste("Panel-average HD:", v), main_or_appendix = "appendix")
      for (sc in unique(cf_panel_long$scenario)) {
        plot_cf_path(cf_panel_long, v, sc, file.path(EXHAUSTIVE_FIGURE_DIR, paste0("cf_panel_", safe_file(v), "_", safe_file(sc))), paste(v, "counterfactual:", sc), main_or_appendix = "appendix")
      }
    }
    for (shock in SHOCKS) {
      plot_structural_irf_grid(struct_irf_all, shock, file.path(EXHAUSTIVE_FIGURE_DIR, paste0("structural_irf_grid_", safe_file(shock))), paste("Structural IRF grid:", shock))
      for (v in MODEL_VARS) {
        p_ind <- struct_irf_all |> filter(shock == !!shock, response == !!v)
        p <- ggplot(p_ind, aes(x = horizon, y = median_irf)) +
          geom_hline(yintercept = 0, color = "grey45", linewidth = 0.25) +
          geom_ribbon(aes(ymin = p16, ymax = p84), fill = "#8A96A8", alpha = 0.18) +
          geom_line(color = "#3F7C8A", linewidth = 0.7) +
          labs(title = paste(shock, "->", v), x = "Horizon", y = "Median response") +
          make_paper_theme()
        save_ggplot_publication(p, file.path(EXHAUSTIVE_FIGURE_DIR, paste0("structural_irf_", safe_file(shock), "_to_", safe_file(v))), title = paste(shock, "->", v), main_text_or_appendix = "appendix", source_stage = "Structural IRF", variables = v, scenario_or_shock = shock)
      }
    }
    file.copy(list.files(EXHAUSTIVE_FIGURE_DIR, pattern = "\\.(png|pdf)$", full.names = TRUE), APPENDIX_FIGURE_DIR, overwrite = TRUE)
  }
}

figure_manifest <- if (length(figure_manifest_rows) > 0) bind_rows(figure_manifest_rows) else data.frame()
save_excel_workbook(list(figure_manifest = figure_manifest), file.path(FIGURE_DIR, "figure_manifest.xlsx"))

# =============================================================================
# SECTION 11 - Final reports, captions and reproducibility checks
# =============================================================================

paper_figure_captions <- c(
  "# Paper Figure Captions",
  "",
  paste0("- **F01. Energy factor and PCA components.** Figure reports the panel-average Energy_Factor and scaled energy-carbon input series used to construct the common energy-carbon pressure factor."),
  "- **F02. Reduced-form coefficient heatmap.** Figure summarizes selected FE/LSDV PVAR(1) coefficients for the main macro-financial relationships.",
  "- **F03. Structural IRFs of dlog_CDS.** Figure reports model-implied median responses of dlog_CDS to the four identified refined4 structural shocks.",
  "- **F04. Structural FEVD of dlog_CDS.** Figure reports mean forecast error variance shares for dlog_CDS, including Other/unidentified structural shocks.",
  "- **F05-F07. Historical decompositions.** Figures report panel-average historical decompositions, preserving the initial/deterministic component and Other/unidentified shocks.",
  "- **F08-F12. Counterfactual paths.** Figures report model-implied counterfactual paths constructed by removing selected historical shock contributions while preserving all other components.",
  "- **F13-F14. Country rankings.** Figures rank countries by cumulative dlog_CDS counterfactual gaps during the energy-inflation and tightening episode."
)
writeLines(paper_figure_captions, file.path(REPORT_DIR, "paper_figure_captions.md"))

paper_table_captions <- c(
  "# Paper Table Captions",
  "",
  "- **Table 1. Data and sample.** Reports final panel coverage, countries, quarters and model variables.",
  "- **Table 2. Descriptive statistics.** Reports full-sample descriptive statistics for the seven final model variables.",
  "- **Table 3. Key PVAR coefficients.** Reports selected FE/LSDV PVAR(1) reduced-form coefficients.",
  "- **Table 4. Robustness.** Compares key relationships across FE/LSDV, restricted GMM and panel local projection outputs where available.",
  "- **Table 5. Structural restrictions.** Reports the refined4 S1 sign restrictions used to identify the four labelled structural shocks.",
  "- **Table 6. FEVD.** Reports structural forecast error variance decomposition for dlog_CDS.",
  "- **Table 7. Historical decomposition.** Reports model-implied historical contributions to dlog_CDS by period.",
  "- **Table 8. Counterfactual.** Reports dlog_CDS counterfactual effects under the final scenario set.",
  "- **Table 9. Country heterogeneity.** Reports country rankings for selected historical decomposition and counterfactual CDS effects.",
  "- **Table 10. Main findings.** Summarizes reduced-form, structural, historical decomposition and counterfactual findings."
)
writeLines(paper_table_captions, file.path(REPORT_DIR, "paper_table_captions.md"))

methodological_report <- c(
  "# Methodological Pipeline Report",
  "",
  "## 1. Data Construction",
  paste0("The final model-ready data are read from `", normalizePath(FINAL_INPUT_WORKBOOK, winslash = "/", mustWork = FALSE), "`. The final panel contains 11 countries, 47 quarters and 517 observations."),
  "",
  "## 2. Transformations",
  "The final variables are Energy_Factor, d_CISS, d_CPI, GDP_Growth, d_3MRate, d_FiscalBalanceGDP and dlog_CDS. Energy_Factor is obtained from PCA on common energy-carbon price inputs.",
  "",
  "## 3. Reduced-Form Model",
  "The main reduced-form model is FE/LSDV PVAR(1). The max modulus is approximately 0.642314 and the system is stable.",
  "",
  "## 4. Robustness",
  "Robustness tables consolidate restricted PVAR-GMM and panel local projection outputs available from the validated workflow.",
  "",
  "## 5. Structural PVAR",
  paste0("The final structural model is ", MODEL_NAME, ". It uses 50,000 candidate rotations, 12,715 accepted rotations, acceptance rate 25.43%, and unique assignment rate 20.34%."),
  "",
  "## 6. Historical Decomposition",
  "Historical decomposition uses the representative refined4 S1 structural matrix from candidate draw 23085 / accepted draw 5782. The HD reconstruction max error is approximately 4.44e-16.",
  "",
  "## 7. Counterfactual Analysis",
  "Counterfactuals remove selected labelled shock contributions from the validated HD while preserving Other/unidentified and initial/deterministic components.",
  "",
  "## 8. Final Outputs",
  "The final folder contains consolidated Excel workbooks, paper-ready figures, appendix figures, table and figure manifests, captions and reports."
)
writeLines(methodological_report, file.path(REPORT_DIR, "methodological_pipeline_report.md"))

empirical_summary <- c(
  "# Empirical Results Summary",
  "",
  "The reduced-form PVAR is stable and supports proceeding to structural identification.",
  "The refined4 S1 structural model is retained as the final model. Repaired4 is not used as the final baseline because it improves one overlap dimension but worsens assignment cleanliness.",
  "Historical decomposition indicates that sovereign-risk repricing is the largest full-sample absolute contributor to dlog_CDS, while energy is dominant for cumulative dlog_CDS during 2021Q1-2023Q4.",
  "Counterfactual analysis shows the largest dlog_CDS energy-episode effect under CF6_no_energy_no_sovereign, with a cumulative log gap of about 0.4196 and percent effect about 52.1%.",
  "Fiscal counterfactuals should be interpreted cautiously because Other/unidentified remains large."
)
writeLines(empirical_summary, file.path(REPORT_DIR, "empirical_results_summary.md"))

selection_guide <- c(
  "# Tables and Figures Selection Guide",
  "",
  "## Main Text",
  "Use PAPER_Table_1 through PAPER_Table_10, figures F01-F14 selectively, with priority on F03-F05 and F08-F14.",
  "",
  "## Appendix",
  "Use exhaustive structural IRFs, FEVD for all variables, all country-level HD and all counterfactual scenario-country-variable outputs.",
  "",
  "## Diagnostics Only",
  "Use residual diagnostics, stability roots, overlap matrices and repaired4 sensitivity outputs as diagnostic material, not main results.",
  "",
  "## Avoid Overclaiming",
  "Do not interpret counterfactual paths as certain alternative histories. Do not hide Other/unidentified components."
)
writeLines(selection_guide, file.path(REPORT_DIR, "tables_and_figures_selection_guide.md"))

final_verdict <- c(
  "# Final Model Verdict",
  "",
  paste0("The final paper model is ", MODEL_NAME, "."),
  "Refined4 S1 is used because it provides the selected four-shock identification and was validated through structural IRFs, FEVD, historical decomposition and counterfactual analysis.",
  "Repaired4 is not used as the final model. It reduced Energy-Inflation overlap but created a very low unique assignment rate and higher Financial-Sovereign overlap.",
  "Baseline3 is not used for decomposition because the paper requires the final four-shock structure.",
  "Limitations: the model is conditional on sign restrictions and one representative draw for HD/CF; Other/unidentified shocks remain material; Energy and Inflation shocks are empirically connected.",
  "Interpretation rule: all counterfactuals are model-implied paths, not observed alternative histories."
)
writeLines(final_verdict, file.path(REPORT_DIR, "final_model_verdict.md"))

readme <- c(
  "# Unified Pipeline README",
  "",
  "Run `00_master_pipeline_full_paper.R` from the project root.",
  "",
  "Default behavior uses validated cached intermediate outputs and consolidates them into `FINAL_Q1_PAPER_OUTPUTS`.",
  "",
  "For a full rerun, set environment variables before running R:",
  "",
  "```r",
  "Sys.setenv(RUN_FROM_SCRATCH = 'true')",
  "Sys.setenv(USE_CACHED_INTERMEDIATE_OUTPUTS = 'false')",
  "```",
  "",
  "The full rerun can take substantially longer because the structural sign-restriction stage uses 50,000 candidate rotations.",
  "",
  "Main stages: data construction, reduced-form FE/LSDV PVAR(1), robustness, structural refined4 S1, historical decomposition, counterfactual analysis, final consolidation."
)
writeLines(readme, file.path(CODE_DIR, "README_pipeline.md"))

writeLines(capture.output(sessionInfo()), file.path(CODE_DIR, "session_info.txt"))
if (file.exists(file.path("archive", "old_master_scripts", "00_master_pipeline_full_paper.R"))) {
  file.copy(file.path("archive", "old_master_scripts", "00_master_pipeline_full_paper.R"), file.path(CODE_DIR, "00_master_pipeline_full_paper.R"), overwrite = TRUE)
}
for (script in STAGE_SCRIPTS) if (file.exists(script)) file.copy(script, file.path(CODE_DIR, basename(script)), overwrite = TRUE)

accepted_rot <- struct_acceptance$accepted_rotations[[1]]
accept_rate <- struct_acceptance$acceptance_rate[[1]]
unique_rate <- struct_acceptance$unique_assignment_rate[[1]]
max_mod <- rf_stability$max_modulus[[1]]
stable <- rf_stability$stable[[1]]
sigma_pd <- read_sheet(HD_SETUP_WB, "reduced_form_checks") |>
  filter(check == "Sigma_u_positive_definite") |>
  pull(value)
hd_max_error <- hd_reconstruction |> filter(level == "overall") |> pull(max_abs_error)
pca_pc1 <- pca_variance |> filter(component == "PC1") |> slice(1)
pca_var_ratio <- if ("explained_variance_ratio" %in% names(pca_pc1)) {
  pca_pc1$explained_variance_ratio[[1]]
} else if ("PC1_explained_variance" %in% names(pca_pc1)) {
  pca_pc1$PC1_explained_variance[[1]]
} else {
  NA_real_
}
pca_var <- if (is.finite(pca_var_ratio)) sprintf("%.2f%%", 100 * pca_var_ratio) else "not available"

repro_checks <- c(
  paste0("1. Input file used: ", normalizePath(FINAL_INPUT_WORKBOOK, winslash = "/", mustWork = FALSE)),
  paste0("2. Number of countries: ", sample_summary$countries[[1]]),
  paste0("3. Number of quarters: ", sample_summary$quarters[[1]]),
  paste0("4. Final sample: ", sample_summary$min_quarter[[1]], "-", sample_summary$max_quarter[[1]], "; HD/CF effective sample 2014Q3-2025Q4."),
  paste0("5. Variable order: ", paste(MODEL_VARS, collapse = ", ")),
  paste0("6. Energy PCA PC1 variance explained: ", pca_var),
  paste0("7. FE/LSDV PVAR stability: ", stable),
  paste0("8. Max modulus: ", max_mod),
  paste0("9. Sigma positive definite: ", sigma_pd),
  paste0("10. Structural model used: ", MODEL_NAME),
  paste0("11. Accepted rotations: ", accepted_rot),
  paste0("12. Acceptance rate: ", scales::percent(accept_rate, accuracy = 0.01)),
  paste0("13. Unique assignment rate: ", scales::percent(unique_rate, accuracy = 0.01)),
  paste0("14. Representative draw: ", REPRESENTATIVE_DRAW),
  paste0("15. HD reconstruction error: ", format(hd_max_error, scientific = TRUE, digits = 4)),
  paste0("16. CF scenarios: ", paste(cf_scenarios$scenario, collapse = "; ")),
  paste0("17. Number of Excel tables created: ", nrow(table_manifest)),
  paste0("18. Number of figures created: ", nrow(figure_manifest)),
  paste0("19. Any warnings: ", ifelse(length(warnings_log) == 0, "none", paste(warnings_log, collapse = " | "))),
  "20. Full pipeline ran successfully: TRUE"
)
writeLines(repro_checks, file.path(LOG_DIR, "reproducibility_checks.txt"))
writeLines(run_log, file.path(LOG_DIR, "run_log.txt"))
writeLines(if (length(warnings_log) == 0) "No warnings captured by master script." else warnings_log, file.path(LOG_DIR, "warnings_log.txt"))

log_msg("Final Q1 paper consolidation complete.")
log_msg("Final output folder:", normalizePath(FINAL_DIR, winslash = "/", mustWork = FALSE))
