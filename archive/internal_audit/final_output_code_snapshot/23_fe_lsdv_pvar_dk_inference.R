# Alternative Driscoll-Kraay inference for the baseline FE/LSDV PVAR(1).
# This script does not change the FE/LSDV coefficients or any structural outputs.

required_packages <- c("readxl", "openxlsx", "dplyr", "tidyr", "tibble", "plm")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    ". Run source('00_install_packages.R') first."
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(plm)
})

ROOT_DIR <- getwd()
FINAL_DIR <- file.path(ROOT_DIR, "FINAL_Q1_PAPER_OUTPUTS")
DATA_FILE <- file.path(FINAL_DIR, "01_data", "model_ready_dataset.xlsx")
MASTER_FILE <- file.path(FINAL_DIR, "02_master_excel", "MASTER_all_tables_for_paper.xlsx")
OUTPUT_DIR <- file.path(FINAL_DIR, "06_alternative_inference_DK")
OUTPUT_XLSX <- file.path(OUTPUT_DIR, "FE_LSDV_PVAR_DK_inference.xlsx")
OUTPUT_REPORT <- file.path(OUTPUT_DIR, "DK_inference_report.md")

MODEL_VARS <- c(
  "Energy_Factor",
  "d_CISS",
  "d_CPI",
  "GDP_Growth",
  "d_3MRate",
  "d_FiscalBalanceGDP",
  "dlog_CDS"
)

DK_BASELINE_LAG <- 4L
DK_SENSITIVITY_LAGS <- c(2L, 4L, 6L)

KEY_CHANNELS <- tibble::tribble(
  ~relation, ~cause, ~response,
  "Energy_Factor -> d_CPI", "Energy_Factor", "d_CPI",
  "Energy_Factor -> d_3MRate", "Energy_Factor", "d_3MRate",
  "d_CPI -> d_3MRate", "d_CPI", "d_3MRate",
  "Energy_Factor -> dlog_CDS", "Energy_Factor", "dlog_CDS",
  "d_CISS -> dlog_CDS", "d_CISS", "dlog_CDS",
  "d_CPI -> dlog_CDS", "d_CPI", "dlog_CDS",
  "GDP_Growth -> d_FiscalBalanceGDP", "GDP_Growth", "d_FiscalBalanceGDP",
  "d_3MRate -> dlog_CDS", "d_3MRate", "dlog_CDS",
  "d_FiscalBalanceGDP -> dlog_CDS", "d_FiscalBalanceGDP", "dlog_CDS"
)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

sig_label <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "not reported",
    p < 0.01 ~ "significant at 1%",
    p < 0.05 ~ "significant at 5%",
    p < 0.10 ~ "significant at 10%",
    TRUE ~ "not significant"
  )
}

is_sig10 <- function(p) {
  !is.na(p) & p < 0.10
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

fmt_p <- function(p) {
  ifelse(
    is.na(p),
    "",
    ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3))
  )
}

read_final_data <- function() {
  if (!file.exists(DATA_FILE)) {
    stop("Final model-ready dataset not found: ", DATA_FILE)
  }

  sheets <- readxl::excel_sheets(DATA_FILE)
  sheet <- if ("estimation_balanced_dataset" %in% sheets) {
    "estimation_balanced_dataset"
  } else {
    "model_ready_dataset"
  }

  dat <- readxl::read_excel(DATA_FILE, sheet = sheet) |>
    as.data.frame()

  missing_cols <- setdiff(c("Country", "Quarter_ID", "quarter_index", MODEL_VARS), names(dat))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in model-ready dataset: ", paste(missing_cols, collapse = ", "))
  }

  dat |>
    mutate(
      Country = as.character(Country),
      Quarter_ID = as.character(Quarter_ID),
      quarter_index = as.integer(quarter_index),
      across(all_of(MODEL_VARS), as.numeric)
    ) |>
    arrange(Country, quarter_index)
}

make_lagged_data <- function(dat) {
  dat |>
    arrange(Country, quarter_index) |>
    group_by(Country) |>
    mutate(across(all_of(MODEL_VARS), ~ dplyr::lag(.x, 1), .names = "{.col}_l1")) |>
    ungroup() |>
    filter(complete.cases(across(all_of(MODEL_VARS))), complete.cases(across(ends_with("_l1"))))
}

safe_vcov <- function(expr, fit) {
  tryCatch(
    expr,
    error = function(e) {
      matrix(
        NA_real_,
        nrow = length(stats::coef(fit)),
        ncol = length(stats::coef(fit)),
        dimnames = list(names(stats::coef(fit)), names(stats::coef(fit)))
      )
    }
  )
}

extract_se <- function(vcov_mat, terms) {
  se <- sqrt(diag(vcov_mat))
  out <- rep(NA_real_, length(terms))
  names(out) <- terms
  hit <- intersect(names(se), terms)
  out[hit] <- se[hit]
  out
}

estimate_fe_equation <- function(data_lagged, response, dk_lag) {
  rhs <- paste0(MODEL_VARS, "_l1", collapse = " + ")
  fml <- stats::as.formula(paste(response, "~", rhs))
  pdata <- plm::pdata.frame(data_lagged, index = c("Country", "quarter_index"))
  fit <- plm::plm(fml, data = pdata, model = "within", effect = "individual")

  terms <- names(stats::coef(fit))
  beta <- stats::coef(fit)
  df <- max(1, stats::df.residual(fit))

  vcov_classic <- safe_vcov(stats::vcov(fit), fit)
  vcov_dk <- safe_vcov(plm::vcovSCC(fit, type = "HC1", maxlag = dk_lag), fit)

  se_classic <- extract_se(vcov_classic, terms)
  se_dk <- extract_se(vcov_dk, terms)
  t_classic <- beta / se_classic
  p_classic <- 2 * stats::pt(abs(t_classic), df = df, lower.tail = FALSE)
  t_dk <- beta / se_dk
  p_dk <- 2 * stats::pt(abs(t_dk), df = df, lower.tail = FALSE)

  tibble::tibble(
    dependent_variable = response,
    lagged_regressor = terms,
    cause = sub("_l1$", "", terms),
    relation = paste(cause, dependent_variable, sep = " -> "),
    coefficient = as.numeric(beta),
    original_standard_error_calculated = as.numeric(se_classic),
    original_p_value_calculated = as.numeric(p_classic),
    DK_lag = dk_lag,
    DK_standard_error = as.numeric(se_dk),
    DK_t_statistic = as.numeric(t_dk),
    DK_p_value = as.numeric(p_dk),
    DK_stars = stars(as.numeric(p_dk))
  )
}

estimate_fe_system <- function(data_lagged, dk_lag) {
  bind_rows(lapply(MODEL_VARS, estimate_fe_equation, data_lagged = data_lagged, dk_lag = dk_lag))
}

read_original_coefficients <- function() {
  if (!file.exists(MASTER_FILE)) {
    return(tibble::tibble())
  }

  sheets <- readxl::excel_sheets(MASTER_FILE)
  if (!"T11_fe_lsdv_pvar_coefficients" %in% sheets) {
    return(tibble::tibble())
  }

  readxl::read_excel(MASTER_FILE, sheet = "T11_fe_lsdv_pvar_coefficients") |>
    as_tibble() |>
    transmute(
      dependent_variable = equation,
      lagged_regressor = regressor,
      original_coefficient = coefficient,
      original_standard_error = se_classic,
      original_p_value = p_classic,
      original_stars = stars(p_classic),
      original_DK_standard_error_if_present = se_driscoll_kraay,
      original_DK_p_value_if_present = p_driscoll_kraay
    )
}

make_coefficient_table <- function(dk_lag4, original_coef) {
  out <- dk_lag4 |>
    left_join(original_coef, by = c("dependent_variable", "lagged_regressor")) |>
    mutate(
      original_standard_error = dplyr::coalesce(original_standard_error, original_standard_error_calculated),
      original_p_value = dplyr::coalesce(original_p_value, original_p_value_calculated),
      original_stars = dplyr::coalesce(original_stars, stars(original_p_value)),
      coefficient_difference_vs_master = coefficient - original_coefficient,
      interpretation_note = "Coefficient kept unchanged; Driscoll-Kraay affects only standard error, t-statistic, p-value and stars."
    ) |>
    select(
      dependent_variable,
      lagged_regressor,
      coefficient,
      original_standard_error,
      original_p_value,
      DK_standard_error,
      DK_t_statistic,
      DK_p_value,
      DK_stars,
      original_stars,
      interpretation_note,
      cause,
      relation,
      DK_lag,
      original_coefficient,
      coefficient_difference_vs_master,
      original_DK_standard_error_if_present,
      original_DK_p_value_if_present
    )

  out
}

make_matrix <- function(coef_table, value_col) {
  coef_table |>
    mutate(cell = .data[[value_col]]) |>
    select(dependent_variable, cause, cell) |>
    tidyr::pivot_wider(names_from = cause, values_from = cell) |>
    arrange(match(dependent_variable, MODEL_VARS)) |>
    select(dependent_variable, all_of(MODEL_VARS))
}

make_key_channels <- function(coef_table) {
  KEY_CHANNELS |>
    left_join(
      coef_table |>
        select(
          dependent_variable, cause, coefficient, original_p_value, DK_p_value,
          original_stars, DK_stars
        ),
      by = c("response" = "dependent_variable", "cause" = "cause")
    ) |>
    mutate(
      original_significance = sig_label(original_p_value),
      DK_significance = sig_label(DK_p_value),
      original_verdict = ifelse(is_sig10(original_p_value), "significant at 10% threshold", "not significant at 10% threshold"),
      DK_verdict = ifelse(is_sig10(DK_p_value), "significant at 10% threshold", "not significant at 10% threshold"),
      final_interpretation = case_when(
        is_sig10(original_p_value) & is_sig10(DK_p_value) ~ "robust under DK",
        is_sig10(original_p_value) & !is_sig10(DK_p_value) ~ "weaker under DK",
        !is_sig10(original_p_value) & !is_sig10(DK_p_value) ~ "not significant under either",
        !is_sig10(original_p_value) & is_sig10(DK_p_value) ~ "changes interpretation",
        TRUE ~ "not reported"
      )
    ) |>
    select(
      relation,
      coefficient,
      original_p_value,
      DK_p_value,
      original_significance,
      DK_significance,
      original_verdict,
      DK_verdict,
      final_interpretation
    )
}

make_sensitivity <- function(sensitivity_results) {
  all_sens <- bind_rows(sensitivity_results) |>
    filter(relation %in% KEY_CHANNELS$relation) |>
    select(relation, coefficient, DK_lag, DK_p_value) |>
    tidyr::pivot_wider(names_from = DK_lag, values_from = DK_p_value, names_prefix = "p_DK_lag")

  KEY_CHANNELS |>
    select(relation) |>
    left_join(all_sens, by = "relation") |>
    mutate(
      stability_of_inference = case_when(
        is_sig10(p_DK_lag2) & is_sig10(p_DK_lag4) & is_sig10(p_DK_lag6) ~ "robust across DK lags 2/4/6",
        !is_sig10(p_DK_lag2) & !is_sig10(p_DK_lag4) & !is_sig10(p_DK_lag6) ~ "not significant across DK lags 2/4/6",
        is_sig10(p_DK_lag4) ~ "baseline DK lag 4 significant, but sensitivity depends on lag choice",
        TRUE ~ "mixed DK-lag sensitivity"
      )
    )
}

make_setup <- function(data, data_lagged, coef_table) {
  n_countries <- dplyr::n_distinct(data$Country)
  n_quarters <- dplyr::n_distinct(data$Quarter_ID)
  n_model_quarters <- dplyr::n_distinct(data_lagged$Quarter_ID)
  max_abs_diff <- suppressWarnings(max(abs(coef_table$coefficient_difference_vs_master), na.rm = TRUE))
  if (!is.finite(max_abs_diff)) max_abs_diff <- NA_real_

  tibble::tibble(
    item = c(
      "model",
      "purpose",
      "variable_order",
      "input_dataset",
      "full_sample",
      "effective_lagged_regression_sample",
      "number_of_countries",
      "number_of_quarters_final_dataset",
      "number_of_quarters_effective_regressions",
      "observations_final_dataset",
      "observations_effective_regressions",
      "DK_baseline_lag",
      "DK_sensitivity_lags",
      "coefficient_check_vs_master",
      "note_coefficients",
      "note_structural_outputs"
    ),
    value = c(
      "FE/LSDV PVAR(1), equation-by-equation within estimator with country fixed effects",
      "Alternative coefficient-level inference using Driscoll-Kraay standard errors",
      paste(MODEL_VARS, collapse = " -> "),
      normalizePath(DATA_FILE, winslash = "/", mustWork = FALSE),
      paste0(min(data$Quarter_ID[order(data$quarter_index)]), "-", max(data$Quarter_ID[order(data$quarter_index)])),
      paste0(min(data_lagged$Quarter_ID[order(data_lagged$quarter_index)]), "-", max(data_lagged$Quarter_ID[order(data_lagged$quarter_index)])),
      as.character(n_countries),
      as.character(n_quarters),
      as.character(n_model_quarters),
      as.character(nrow(data)),
      as.character(nrow(data_lagged)),
      as.character(DK_BASELINE_LAG),
      paste(DK_SENSITIVITY_LAGS, collapse = ", "),
      paste0("Maximum absolute coefficient difference vs master T11 = ", formatC(max_abs_diff, format = "e", digits = 3)),
      "Coefficients are unchanged; only standard errors, t-statistics, p-values and stars are recalculated.",
      "IRF, FEVD, historical decomposition and counterfactual outputs are not modified."
    )
  )
}

write_workbook <- function(path, sheets) {
  wb <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9EAF7", border = "Bottom")

  for (sheet_name in names(sheets)) {
    openxlsx::addWorksheet(wb, sheet_name)
    df <- as.data.frame(sheets[[sheet_name]])
    openxlsx::writeDataTable(wb, sheet_name, df, tableStyle = "TableStyleMedium2")
    if (ncol(df) > 0) {
      openxlsx::addStyle(wb, sheet_name, header_style, rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
      openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
      openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(df)), widths = "auto")
    }
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

write_report <- function(path, setup, key_channels, sensitivity) {
  robust <- key_channels |> filter(final_interpretation == "robust under DK") |> pull(relation)
  weaker <- key_channels |> filter(final_interpretation == "weaker under DK") |> pull(relation)
  not_sig <- key_channels |> filter(final_interpretation == "not significant under either") |> pull(relation)
  changed <- key_channels |> filter(final_interpretation == "changes interpretation") |> pull(relation)

  key_table <- key_channels |>
    mutate(
      coefficient = fmt_num(coefficient, 4),
      original_p_value = fmt_p(original_p_value),
      DK_p_value = fmt_p(DK_p_value)
    )

  key_md <- c(
    "| Relation | Coefficient | Original p-value | DK p-value | Final interpretation |",
    "|---|---:|---:|---:|---|",
    apply(key_table, 1, function(row) {
      paste0(
        "| ", row[["relation"]],
        " | ", row[["coefficient"]],
        " | ", row[["original_p_value"]],
        " | ", row[["DK_p_value"]],
        " | ", row[["final_interpretation"]],
        " |"
      )
    })
  )

  paper_sentence <- if (length(robust) >= 5 && length(weaker) == 0) {
    "The main reduced-form channels remain broadly robust under Driscoll-Kraay inference."
  } else if (length(weaker) > 0) {
    "Some channels become weaker under Driscoll-Kraay inference, and are therefore interpreted as suggestive rather than strongly robust."
  } else {
    "The Driscoll-Kraay exercise mainly confirms which channels are statistically robust and which remain suggestive."
  }

  lines <- c(
    "# Alternative Driscoll-Kraay Inference for Baseline FE/LSDV PVAR(1)",
    "",
    "## 1. Purpose",
    "",
    "This robustness step recalculates coefficient-level inference for the baseline FE/LSDV PVAR(1) using Driscoll-Kraay standard errors. It does not change the model, the estimated coefficient matrix, the Structural PVAR, FEVD, historical decomposition or counterfactual analysis.",
    "",
    "## 2. Model and Coefficients",
    "",
    paste0("- Model: ", setup$value[setup$item == "model"]),
    paste0("- Effective regression sample: ", setup$value[setup$item == "effective_lagged_regression_sample"]),
    paste0("- Countries: ", setup$value[setup$item == "number_of_countries"]),
    paste0("- Effective observations: ", setup$value[setup$item == "observations_effective_regressions"]),
    paste0("- Coefficient check: ", setup$value[setup$item == "coefficient_check_vs_master"]),
    "",
    "## 3. Why Driscoll-Kraay?",
    "",
    "Driscoll-Kraay standard errors are used because the diagnostics indicate cross-sectional dependence and because the panel setting may also involve heteroskedasticity and serial correlation. The correction is applied equation by equation to the fixed-effects/within regressions.",
    "",
    "## 4. DK Settings",
    "",
    paste0("The baseline DK lag is ", DK_BASELINE_LAG, ", appropriate for quarterly data. Sensitivity checks are reported for lags ", paste(DK_SENSITIVITY_LAGS, collapse = ", "), "."),
    "",
    "## 5. Key Channels",
    "",
    key_md,
    "",
    "## 6. Relations Remaining Significant Under DK",
    "",
    if (length(robust) == 0) "None of the key channels are significant under both original and DK inference at the 10% threshold." else paste0("- ", robust),
    "",
    "## 7. Relations Weaker Under DK",
    "",
    if (length(weaker) == 0) "No key channel that was originally significant becomes insignificant under DK." else paste0("- ", weaker),
    "",
    "## 8. Relations Not Significant Under Either",
    "",
    if (length(not_sig) == 0) "No key channel is insignificant under both inference schemes." else paste0("- ", not_sig),
    "",
    "## 9. Relations Changing Interpretation",
    "",
    if (length(changed) == 0) "No key channel changes from insignificant originally to significant under DK." else paste0("- ", changed),
    "",
    "## 10. Recommendation for Paper",
    "",
    "Use the DK table as an appendix or robustness-inference table for the baseline reduced-form FE/LSDV PVAR. The structural IRF, FEVD, historical decomposition and counterfactual exercises should continue to rely on the stable FE/LSDV dynamic system; this step only changes reported standard errors and significance stars.",
    "",
    "## Proposed Paper Text",
    "",
    paste(
      "Given the strong evidence of cross-sectional dependence in the pre-model and residual diagnostics, coefficient-level inference for the baseline FE/LSDV PVAR(1) is additionally assessed using Driscoll-Kraay standard errors computed equation by equation.",
      "This adjustment affects only the reported standard errors and significance levels, while leaving the estimated dynamic coefficient matrix unchanged.",
      "The structural impulse responses, FEVD, historical decomposition and counterfactual exercises continue to rely on the stable FE/LSDV dynamic system.",
      paper_sentence
    ),
    "",
    "## Output",
    "",
    paste0("- ", normalizePath(OUTPUT_XLSX, winslash = "/", mustWork = FALSE)),
    paste0("- ", normalizePath(OUTPUT_REPORT, winslash = "/", mustWork = FALSE))
  )

  writeLines(lines, path, useBytes = TRUE)
}

data <- read_final_data()
data_lagged <- make_lagged_data(data)
original_coef <- read_original_coefficients()

dk_results <- lapply(DK_SENSITIVITY_LAGS, function(lag_value) {
  estimate_fe_system(data_lagged, dk_lag = lag_value)
})
names(dk_results) <- paste0("lag", DK_SENSITIVITY_LAGS)

dk_baseline <- dk_results[[paste0("lag", DK_BASELINE_LAG)]]
coef_long <- make_coefficient_table(dk_baseline, original_coef) |>
  arrange(match(dependent_variable, MODEL_VARS), match(cause, MODEL_VARS))

coef_cells <- coef_long |>
  mutate(cell_compact = paste0(fmt_num(coefficient, 3), DK_stars))

pvalue_cells <- coef_long |>
  mutate(cell_pvalue = paste0(fmt_num(coefficient, 3), " (", fmt_p(DK_p_value), ")"))

dk_matrix_compact <- make_matrix(coef_cells, "cell_compact")
dk_matrix_with_pvalues <- make_matrix(pvalue_cells, "cell_pvalue")
key_channels <- make_key_channels(coef_long)
sensitivity <- make_sensitivity(dk_results)
setup <- make_setup(data, data_lagged, coef_long)

paper_ready_table <- dk_matrix_compact |>
  rename(`Dependent variable` = dependent_variable)

write_workbook(
  OUTPUT_XLSX,
  list(
    DK_model_setup = setup,
    DK_coefficients_long = coef_long,
    DK_matrix_compact = dk_matrix_compact,
    DK_matrix_with_pvalues = dk_matrix_with_pvalues,
    key_channels_original_vs_DK = key_channels,
    DK_sensitivity_lags = sensitivity,
    paper_ready_table = paper_ready_table
  )
)

write_report(OUTPUT_REPORT, setup, key_channels, sensitivity)

message("Alternative DK inference complete.")
message("Output workbook: ", OUTPUT_XLSX)
message("Output report: ", OUTPUT_REPORT)
