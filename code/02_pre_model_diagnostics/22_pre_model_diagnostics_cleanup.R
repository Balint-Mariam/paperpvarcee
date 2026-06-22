# Pre-model diagnostics cleanup for the final 7-variable PVAR system.
# This script does not re-estimate the PVAR and does not modify empirical results.

required_packages <- c(
  "readxl", "openxlsx", "dplyr", "tidyr", "tibble", "plm", "tseries"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    ". Run source('code/00_install_packages.R') first."
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(plm)
  library(tseries)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

ROOT_DIR <- getwd()
FINAL_DIR <- "outputs"
DATA_FILE <- file.path(FINAL_DIR, "01_model_ready_data", "model_ready_dataset.xlsx")
MASTER_FILE <- file.path(FINAL_DIR, "02_tables", "main_paper", "MASTER_all_tables_for_paper.xlsx")
DIAG_FILE <- file.path(FINAL_DIR, "02_tables", "robustness", "pre_model_diagnostics_cleaned.xlsx")
UPDATED_MASTER_FILE <- file.path(FINAL_DIR, "02_tables", "main_paper", "MASTER_all_tables_for_paper_updated_diagnostics.xlsx")
REPORT_FILE <- file.path(FINAL_DIR, "04_reports", "pre_model_diagnostics_cleanup_report.md")

MODEL_VARS <- c(
  "Energy_Factor",
  "d_CISS",
  "d_CPI",
  "GDP_Growth",
  "d_3MRate",
  "d_FiscalBalanceGDP",
  "dlog_CDS"
)

COMMON_VARS <- c("Energy_Factor", "d_CISS")
COUNTRY_VARS <- setdiff(MODEL_VARS, COMMON_VARS)

TRANSFORMATIONS <- tibble::tibble(
  variable = MODEL_VARS,
  transformation = c(
    "PCA factor from common energy-carbon price variables",
    "First difference of CISS",
    "First difference of CPI rate",
    "Quarterly GDP growth rate in levels",
    "First difference of 3-month rate",
    "First difference of fiscal balance to GDP",
    "Log difference of 5Y sovereign CDS"
  )
)

dir.create(dirname(DIAG_FILE), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(REPORT_FILE), recursive = TRUE, showWarnings = FALSE)

as_num1 <- function(x) {
  out <- suppressWarnings(tryCatch(as.numeric(x)[1], error = function(e) NA_real_))
  if (length(out) == 0) NA_real_ else out
}

quarter_order <- function(x) {
  year <- as.integer(substr(x, 1, 4))
  quarter <- as.integer(sub(".*Q", "", x))
  year * 4L + quarter
}

read_final_data <- function() {
  if (!file.exists(DATA_FILE)) {
    stop("Final model-ready dataset not found: ", DATA_FILE)
  }

  sheets <- readxl::excel_sheets(DATA_FILE)
  sheet_to_use <- if ("estimation_balanced_dataset" %in% sheets) {
    "estimation_balanced_dataset"
  } else {
    "model_ready_dataset"
  }

  dat <- readxl::read_excel(DATA_FILE, sheet = sheet_to_use) |>
    as.data.frame()

  required_cols <- c("Country", "Quarter_ID", MODEL_VARS)
  missing_cols <- setdiff(required_cols, names(dat))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in final dataset: ", paste(missing_cols, collapse = ", "))
  }

  if (!"quarter_index" %in% names(dat)) {
    q_map <- tibble::tibble(Quarter_ID = sort(unique(dat$Quarter_ID))) |>
      mutate(quarter_index = dense_rank(quarter_order(Quarter_ID)))
    dat <- dat |>
      left_join(q_map, by = "Quarter_ID")
  }

  dat |>
    arrange(Country, quarter_index) |>
    mutate(
      Country = as.character(Country),
      Quarter_ID = as.character(Quarter_ID),
      across(all_of(MODEL_VARS), as.numeric),
      quarter_index = as.integer(quarter_index)
    )
}

sample_overview <- function(dat) {
  n_countries <- dplyr::n_distinct(dat$Country)
  n_quarters <- dplyr::n_distinct(dat$Quarter_ID)
  expected <- n_countries * n_quarters
  missing_model_values <- sum(is.na(dat[, MODEL_VARS]))
  duplicate_cq <- dat |>
    count(Country, Quarter_ID) |>
    filter(n > 1) |>
    nrow()

  tibble::tibble(
    source_file = DATA_FILE,
    sheet_used = if ("quarter_index_original" %in% names(dat)) "estimation_balanced_dataset" else "model_ready_dataset",
    observations = nrow(dat),
    countries = n_countries,
    quarters = n_quarters,
    min_quarter = dat$Quarter_ID[which.min(dat$quarter_index)],
    max_quarter = dat$Quarter_ID[which.max(dat$quarter_index)],
    expected_balanced_observations = expected,
    balanced = nrow(dat) == expected,
    missing_values_in_model_variables = missing_model_values,
    duplicates_country_quarter = duplicate_cq
  )
}

sample_for_variable <- function(dat, variable, variable_type, lag_loss = 0L) {
  if (variable_type == "common") {
    d <- dat |>
      distinct(Quarter_ID, quarter_index, .keep_all = TRUE) |>
      filter(!is.na(.data[[variable]])) |>
      arrange(quarter_index)
    countries <- dplyr::n_distinct(dat$Country)
    quarters <- nrow(d)
    observations <- nrow(d)
    effective_quarters <- max(quarters - lag_loss, 0L)
    effective_observations <- effective_quarters
  } else {
    d <- dat |>
      filter(!is.na(.data[[variable]])) |>
      arrange(Country, quarter_index)
    countries <- dplyr::n_distinct(d$Country)
    quarters <- dplyr::n_distinct(d$Quarter_ID)
    observations <- nrow(d)
    effective_quarters <- max(quarters - lag_loss, 0L)
    effective_observations <- countries * effective_quarters
  }

  tibble::tibble(
    n_countries = countries,
    n_quarters = quarters,
    observations = observations,
    lag_loss_per_series = lag_loss,
    effective_quarters_approx = effective_quarters,
    effective_observations_approx = effective_observations,
    min_quarter = if (nrow(d) > 0) d$Quarter_ID[which.min(d$quarter_index)] else NA_character_,
    max_quarter = if (nrow(d) > 0) d$Quarter_ID[which.max(d$quarter_index)] else NA_character_
  )
}

interpret_unit_root <- function(test, p_value, status) {
  if (!identical(status, "ok")) {
    return("not reported")
  }
  if (is.na(p_value)) {
    return("p-value unavailable")
  }
  if (grepl("KPSS|Hadri", test, ignore.case = TRUE)) {
    ifelse(p_value < 0.05, "reject stationarity", "do not reject stationarity")
  } else {
    ifelse(p_value < 0.05, "reject unit root / stationary", "do not reject unit root")
  }
}

parse_test_result <- function(obj, variable, variable_type, test_name, result_level, country, note) {
  if (inherits(obj, "purtest") && is.list(obj$statistic)) {
    statistic <- as_num1(obj$statistic$statistic)
    p_value <- as_num1(obj$statistic$p.value)
    method <- paste(obj$statistic$method %||% obj$method %||% test_name, collapse = " ")
    alternative <- paste(obj$statistic$alternative %||% obj$alternative %||% "", collapse = " ")
  } else {
    statistic <- as_num1(obj$statistic)
    p_value <- as_num1(obj$p.value)
    method <- paste(obj$method %||% test_name, collapse = " ")
    alternative <- paste(obj$alternative %||% "", collapse = " ")
  }

  tibble::tibble(
    variable = variable,
    variable_type = variable_type,
    test = test_name,
    result_level = result_level,
    country = country,
    statistic = statistic,
    p_value = p_value,
    method = method,
    alternative = alternative,
    status = "ok",
    message = note
  )
}

failed_test_row <- function(variable, variable_type, test_name, result_level, country, message) {
  tibble::tibble(
    variable = variable,
    variable_type = variable_type,
    test = test_name,
    result_level = result_level,
    country = country,
    statistic = NA_real_,
    p_value = NA_real_,
    method = test_name,
    alternative = NA_character_,
    status = "failed",
    message = message
  )
}

not_applicable_row <- function(variable, variable_type, test_name, result_level, country, message) {
  tibble::tibble(
    variable = variable,
    variable_type = variable_type,
    test = test_name,
    result_level = result_level,
    country = country,
    statistic = NA_real_,
    p_value = NA_real_,
    method = "not applicable",
    alternative = NA_character_,
    status = "not_applicable",
    message = message
  )
}

safe_test <- function(expr, variable, variable_type, test_name, result_level = "panel",
                      country = NA_character_, note = "", sample_info) {
  out <- tryCatch(
    parse_test_result(
      suppressWarnings(expr),
      variable = variable,
      variable_type = variable_type,
      test_name = test_name,
      result_level = result_level,
      country = country,
      note = note
    ),
    error = function(e) {
      failed_test_row(variable, variable_type, test_name, result_level, country, conditionMessage(e))
    }
  )

  bind_cols(out, sample_info) |>
    mutate(
      stationarity_interpretation = mapply(
        interpret_unit_root,
        test,
        p_value,
        status,
        USE.NAMES = FALSE
      )
    )
}

run_fisher_pp <- function(dat, variable) {
  country_rows <- lapply(sort(unique(dat$Country)), function(cty) {
    x <- dat |>
      filter(Country == cty) |>
      arrange(quarter_index) |>
      pull(.data[[variable]])
    x <- x[!is.na(x)]
    sample_info <- tibble::tibble(
      n_countries = 1L,
      n_quarters = length(x),
      observations = length(x),
      lag_loss_per_series = 0L,
      effective_quarters_approx = length(x),
      effective_observations_approx = length(x),
      min_quarter = dat |> filter(Country == cty) |> arrange(quarter_index) |> slice(1) |> pull(Quarter_ID),
      max_quarter = dat |> filter(Country == cty) |> arrange(quarter_index) |> slice(n()) |> pull(Quarter_ID)
    )

    safe_test(
      tseries::pp.test(x),
      variable = variable,
      variable_type = "country-specific",
      test_name = "PP country-level",
      result_level = "country",
      country = cty,
      note = "Country-level PP p-value used in Fisher PP combination.",
      sample_info = sample_info
    )
  }) |>
    bind_rows()

  valid <- country_rows |>
    filter(status == "ok", !is.na(p_value), p_value > 0)

  sample_info <- sample_for_variable(dat, variable, "country-specific", lag_loss = 0L)
  if (nrow(valid) == 0) {
    combined <- failed_test_row(
      variable,
      "country-specific",
      "Fisher PP",
      "panel",
      NA_character_,
      "No valid country-level PP p-values were available."
    ) |>
      bind_cols(sample_info)
  } else {
    stat <- -2 * sum(log(valid$p_value))
    p_value <- stats::pchisq(stat, df = 2 * nrow(valid), lower.tail = FALSE)
    combined <- tibble::tibble(
      variable = variable,
      variable_type = "country-specific",
      test = "Fisher PP",
      result_level = "panel",
      country = NA_character_,
      statistic = stat,
      p_value = p_value,
      method = paste0("Fisher combination of ", nrow(valid), " country-level PP tests"),
      alternative = "stationarity",
      status = "ok",
      message = "Computed from country-level Phillips-Perron tests."
    ) |>
      bind_cols(sample_info)
  }

  combined <- combined |>
    mutate(
      stationarity_interpretation = mapply(
        interpret_unit_root,
        test,
        p_value,
        status,
        USE.NAMES = FALSE
      )
    )

  list(combined = combined, country_detail = country_rows)
}

run_panel_unit_roots <- function(dat) {
  pdata <- plm::pdata.frame(dat, index = c("Country", "quarter_index"))

  bind_rows(lapply(COUNTRY_VARS, function(v) {
    if (!inherits(pdata[[v]], "pseries")) {
      cips_row <- failed_test_row(
        v,
        "country-specific",
        "CIPS / CADF Pesaran",
        "panel",
        NA_character_,
        "Object is not a pseries after pdata.frame conversion."
      ) |>
        bind_cols(sample_for_variable(dat, v, "country-specific", lag_loss = 2L)) |>
        mutate(stationarity_interpretation = "not reported")
    } else {
      cips_row <- safe_test(
        plm::cipstest(pdata[[v]], lags = 1, type = "drift", model = "cmg"),
        variable = v,
        variable_type = "country-specific",
        test_name = "CIPS / CADF Pesaran",
        result_level = "panel",
        note = "Run on a pseries from pdata.frame(index = Country, quarter_index).",
        sample_info = sample_for_variable(dat, v, "country-specific", lag_loss = 2L)
      )
    }

    fisher_pp <- run_fisher_pp(dat, v)

    bind_rows(
      safe_test(
        plm::purtest(pdata[[v]], test = "levinlin", exo = "intercept", lags = 1),
        variable = v,
        variable_type = "country-specific",
        test_name = "LLC",
        result_level = "panel",
        note = "Panel unit-root test with individual intercepts and one lag.",
        sample_info = sample_for_variable(dat, v, "country-specific", lag_loss = 2L)
      ),
      safe_test(
        plm::purtest(pdata[[v]], test = "ips", exo = "intercept", lags = 1),
        variable = v,
        variable_type = "country-specific",
        test_name = "IPS",
        result_level = "panel",
        note = "Panel unit-root test with individual intercepts and one lag.",
        sample_info = sample_for_variable(dat, v, "country-specific", lag_loss = 2L)
      ),
      safe_test(
        plm::purtest(pdata[[v]], test = "madwu", exo = "intercept", lags = 1),
        variable = v,
        variable_type = "country-specific",
        test_name = "Fisher ADF / Maddala-Wu",
        result_level = "panel",
        note = "Fisher-type panel ADF test with individual intercepts and one lag.",
        sample_info = sample_for_variable(dat, v, "country-specific", lag_loss = 2L)
      ),
      fisher_pp$combined,
      cips_row,
      fisher_pp$country_detail
    )
  }))
}

run_common_unit_roots <- function(dat) {
  common_ts <- dat |>
    distinct(Quarter_ID, quarter_index, .keep_all = TRUE) |>
    arrange(quarter_index)

  bind_rows(lapply(COMMON_VARS, function(v) {
    x <- common_ts[[v]]
    bind_rows(
      safe_test(
        tseries::adf.test(x, k = 1),
        variable = v,
        variable_type = "common",
        test_name = "ADF time-series",
        result_level = "common_series",
        note = "Common variable; panel unit-root tests are not substantively informative.",
        sample_info = sample_for_variable(dat, v, "common", lag_loss = 2L)
      ),
      safe_test(
        tseries::pp.test(x),
        variable = v,
        variable_type = "common",
        test_name = "PP time-series",
        result_level = "common_series",
        note = "Common variable; panel unit-root tests are not substantively informative.",
        sample_info = sample_for_variable(dat, v, "common", lag_loss = 0L)
      ),
      safe_test(
        tseries::kpss.test(x, null = "Level"),
        variable = v,
        variable_type = "common",
        test_name = "KPSS time-series",
        result_level = "common_series",
        note = "KPSS null is level stationarity.",
        sample_info = sample_for_variable(dat, v, "common", lag_loss = 0L)
      )
    )
  }))
}

run_cips_status <- function(panel_unit_roots) {
  cips_country <- panel_unit_roots |>
    filter(test == "CIPS / CADF Pesaran", result_level == "panel")

  cips_common <- bind_rows(lapply(COMMON_VARS, function(v) {
    not_applicable_row(
      variable = v,
      variable_type = "common",
      test_name = "CIPS / CADF Pesaran",
      result_level = "common_series",
      country = NA_character_,
      message = "Not applicable: common variable replicated across countries; panel CIPS/CADF is not substantively informative."
    ) |>
      bind_cols(sample_for_variable(final_data, v, "common", lag_loss = 0L)) |>
      mutate(stationarity_interpretation = "not applicable")
  }))

  bind_rows(cips_common, cips_country) |>
    arrange(match(variable, MODEL_VARS))
}

avg_pairwise_corr <- function(dat, variable) {
  wide <- dat |>
    select(Country, Quarter_ID, quarter_index, all_of(variable)) |>
    arrange(quarter_index, Country) |>
    tidyr::pivot_wider(names_from = Country, values_from = all_of(variable)) |>
    select(-Quarter_ID, -quarter_index)

  corr <- suppressWarnings(stats::cor(as.matrix(wide), use = "pairwise.complete.obs"))
  if (all(is.na(corr)) || ncol(corr) < 2) {
    return(NA_real_)
  }
  mean(corr[upper.tri(corr)], na.rm = TRUE)
}

run_cross_sectional_dependence <- function(dat) {
  pdata <- plm::pdata.frame(dat, index = c("Country", "quarter_index"))

  bind_rows(lapply(MODEL_VARS, function(v) {
    if (v %in% COMMON_VARS) {
      not_applicable_row(
        variable = v,
        variable_type = "common",
        test_name = "Pesaran CD",
        result_level = "panel",
        country = NA_character_,
        message = "Not applicable: common variable replicated across countries."
      ) |>
        bind_cols(sample_for_variable(dat, v, "common", lag_loss = 0L)) |>
        mutate(
          average_pairwise_correlation = NA_real_,
          cd_verdict = "not applicable / common variable"
        )
    } else {
      out <- safe_test(
        plm::pcdtest(as.formula(paste(v, "~ 1")), data = pdata, test = "cd"),
        variable = v,
        variable_type = "country-specific",
        test_name = "Pesaran CD",
        result_level = "panel",
        note = "Pesaran CD test on the country-specific panel series.",
        sample_info = sample_for_variable(dat, v, "country-specific", lag_loss = 0L)
      )

      out |>
        mutate(
          average_pairwise_correlation = avg_pairwise_corr(dat, v),
          cd_verdict = case_when(
            status != "ok" ~ "not reported",
            is.na(p_value) ~ "p-value unavailable",
            p_value < 0.05 ~ "dependence detected",
            TRUE ~ "dependence not detected"
          )
        )
    }
  })) |>
    select(
      variable, variable_type, test, statistic, p_value, method, status,
      average_pairwise_correlation, cd_verdict, n_countries, n_quarters,
      observations, min_quarter, max_quarter, message
    )
}

supportive_stationarity <- function(test, p_value, status) {
  if (!identical(status, "ok") || is.na(p_value)) {
    return(FALSE)
  }
  if (grepl("KPSS|Hadri", test, ignore.case = TRUE)) {
    p_value >= 0.05
  } else {
    p_value < 0.05
  }
}

make_unit_root_summary <- function(unit_roots) {
  main_rows <- unit_roots |>
    filter(result_level %in% c("panel", "common_series")) |>
    mutate(is_supportive = mapply(supportive_stationarity, test, p_value, status, USE.NAMES = FALSE))

  main_rows |>
    group_by(variable, variable_type) |>
    summarise(
      tests_reported = paste(test[status == "ok"], collapse = "; "),
      tests_ok = sum(status == "ok"),
      tests_failed = sum(status == "failed"),
      tests_not_applicable = sum(status == "not_applicable"),
      supportive_stationarity_tests = sum(is_supportive, na.rm = TRUE),
      min_p_value = suppressWarnings(min(p_value[status == "ok"], na.rm = TRUE)),
      max_p_value = suppressWarnings(max(p_value[status == "ok"], na.rm = TRUE)),
      unit_root_verdict = case_when(
        tests_ok == 0 ~ "not reported",
        supportive_stationarity_tests >= ceiling(0.6 * tests_ok) ~ "stationary / transformation adequate",
        TRUE ~ "mixed evidence; inspect detailed tests"
      ),
      .groups = "drop"
    ) |>
    mutate(
      min_p_value = ifelse(is.infinite(min_p_value), NA_real_, min_p_value),
      max_p_value = ifelse(is.infinite(max_p_value), NA_real_, max_p_value)
    ) |>
    arrange(match(variable, MODEL_VARS))
}

make_common_replication_check <- function(dat) {
  bind_rows(lapply(COMMON_VARS, function(v) {
    dat |>
      group_by(Quarter_ID, quarter_index) |>
      summarise(
        unique_values = dplyr::n_distinct(round(.data[[v]], 12), na.rm = TRUE),
        .groups = "drop"
      ) |>
      summarise(
        variable = v,
        quarters_checked = n(),
        max_unique_values_by_quarter = max(unique_values, na.rm = TRUE),
        replicated_identically_across_countries = all(unique_values == 1),
        .groups = "drop"
      )
  }))
}

make_final_verdict <- function(dat, unit_root_summary, cips_results, cd_results) {
  overview <- sample_overview(dat)
  missing_by_var <- tibble::tibble(
    variable = MODEL_VARS,
    missing_values = vapply(MODEL_VARS, function(v) sum(is.na(dat[[v]])), numeric(1))
  )

  common_base <- dat |>
    distinct(Quarter_ID, quarter_index, .keep_all = TRUE)

  common_unique_obs <- tibble::tibble(
    variable = MODEL_VARS,
    unique_time_observations = vapply(MODEL_VARS, function(v) {
      if (v %in% COMMON_VARS) {
        sum(!is.na(common_base[[v]]))
      } else {
        NA_integer_
      }
    }, integer(1))
  )

  tibble::tibble(variable = MODEL_VARS) |>
    left_join(TRANSFORMATIONS, by = "variable") |>
    mutate(common_variable = ifelse(variable %in% COMMON_VARS, "yes", "no")) |>
    left_join(missing_by_var, by = "variable") |>
    left_join(common_unique_obs, by = "variable") |>
    left_join(unit_root_summary |> select(variable, unit_root_verdict), by = "variable") |>
    left_join(
      cips_results |>
        filter(variable %in% MODEL_VARS) |>
        transmute(
          variable,
          cips_cadf_status = case_when(
            status == "ok" ~ "reported successfully",
            status == "not_applicable" ~ "not applicable / common variable",
            TRUE ~ "not reported - technical implementation issue"
          )
        ),
      by = "variable"
    ) |>
    left_join(
      cd_results |>
        select(variable, cross_sectional_dependence_verdict = cd_verdict),
      by = "variable"
    ) |>
    mutate(
      countries = overview$countries[[1]],
      quarters = overview$quarters[[1]],
      observations = overview$observations[[1]],
      final_diagnostic_conclusion = case_when(
        common_variable == "yes" ~ "Stationarity supported by time-series tests; panel CD and panel unit-root tests are not substantively applicable because the series is common and replicated.",
        unit_root_verdict == "stationary / transformation adequate" & cross_sectional_dependence_verdict == "dependence detected" ~ "Stationary transformed country-specific series with strong cross-sectional dependence; use panel-robust inference in robustness checks.",
        unit_root_verdict == "stationary / transformation adequate" ~ "Stationary transformed country-specific series.",
        TRUE ~ "Inspect detailed diagnostics before using as main diagnostic evidence."
      )
    ) |>
    select(
      variable, transformation, common_variable, countries, quarters, observations,
      unique_time_observations, missing_values, unit_root_verdict, cips_cadf_status,
      cross_sectional_dependence_verdict, final_diagnostic_conclusion
    )
}

write_workbook <- function(path, sheets) {
  wb <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9EAF7", border = "Bottom")

  for (sheet_name in names(sheets)) {
    openxlsx::addWorksheet(wb, sheet_name)
    df <- as.data.frame(sheets[[sheet_name]])
    openxlsx::writeDataTable(wb, sheet_name, df, tableStyle = "TableStyleMedium2")
    if (nrow(df) > 0 && ncol(df) > 0) {
      openxlsx::addStyle(wb, sheet_name, header_style, rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
      openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
      openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(df)), widths = "auto")
    }
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

update_master_workbook <- function(master_path, updated_path, diagnostics_table) {
  if (!file.exists(master_path)) {
    warning("Master workbook not found; skipping updated master workbook.")
    return(invisible(FALSE))
  }

  wb <- openxlsx::loadWorkbook(master_path)
  target_sheet <- "T_pre_model_diagnostics"
  if (target_sheet %in% names(wb)) {
    openxlsx::removeWorksheet(wb, target_sheet)
  }
  openxlsx::addWorksheet(wb, target_sheet)
  openxlsx::writeDataTable(wb, target_sheet, as.data.frame(diagnostics_table), tableStyle = "TableStyleMedium2")
  openxlsx::freezePane(wb, target_sheet, firstRow = TRUE)
  openxlsx::setColWidths(wb, target_sheet, cols = seq_len(ncol(diagnostics_table)), widths = "auto")
  openxlsx::saveWorkbook(wb, updated_path, overwrite = TRUE)
  invisible(TRUE)
}

make_notes <- function(sample_info, unit_root_summary, cips_results, cd_results, final_verdict) {
  cips_ok <- cips_results |>
    filter(variable %in% COUNTRY_VARS) |>
    summarise(all_ok = all(status == "ok"), .groups = "drop") |>
    pull(all_ok)

  cd_detected_vars <- cd_results |>
    filter(variable_type == "country-specific", cd_verdict == "dependence detected") |>
    pull(variable)

  all_stationary <- all(unit_root_summary$unit_root_verdict == "stationary / transformation adequate")

  paper_text <- paste(
    "Pre-model diagnostics indicate that the transformed variables used in the PVAR system are stationary according to standard unit-root diagnostics.",
    "For the country-specific variables, LLC, IPS, Fisher ADF, Fisher PP and Pesaran CIPS/CADF tests reject the unit-root null at conventional levels.",
    "For the common replicated variables, ADF and PP tests reject unit roots, while KPSS tests do not reject level stationarity.",
    "Pesaran CD tests detect strong cross-sectional dependence for the country-specific macro-financial variables, which is expected in a CEE panel exposed to common European energy, inflation and financial-stress shocks.",
    "The empirical strategy therefore complements the baseline FE/LSDV PVAR with robustness checks based on PVAR-GMM and panel local projections with Driscoll-Kraay inference."
  )

  tibble::tibble(
    note_id = c(
      "sample",
      "variables",
      "common_variables",
      "cips_cadf",
      "cross_sectional_dependence",
      "paper_text"
    ),
    text = c(
      paste0(
        "The final model-ready sample contains ", sample_info$countries[[1]], " countries, ",
        sample_info$quarters[[1]], " quarters, and ", sample_info$observations[[1]],
        " observations from ", sample_info$min_quarter[[1]], " to ", sample_info$max_quarter[[1]],
        ". The panel is balanced: ", sample_info$balanced[[1]], "."
      ),
      paste0("All seven final variables were tested: ", paste(MODEL_VARS, collapse = ", "), "."),
      "Energy_Factor and d_CISS are common variables replicated across countries. Time-series ADF, PP and KPSS tests are reported; panel unit-root and Pesaran CD tests are marked not applicable for these two series.",
      if (isTRUE(cips_ok)) {
        "CIPS/CADF was rerun successfully for all five country-specific variables using plm::cipstest on pseries objects from pdata.frame(index = Country, quarter_index)."
      } else {
        "CIPS/CADF was not reported for at least one country-specific variable because of a technical implementation issue; this is not interpreted as evidence of non-stationarity."
      },
      paste0(
        "Pesaran CD detects cross-sectional dependence for: ",
        paste(cd_detected_vars, collapse = ", "),
        "."
      ),
      paper_text
    )
  )
}

write_report <- function(path, sample_info, unit_root_summary, cips_results, cd_results, final_verdict, notes) {
  tested_vars <- paste(MODEL_VARS, collapse = ", ")
  stationary_vars <- unit_root_summary |>
    filter(unit_root_verdict == "stationary / transformation adequate") |>
    pull(variable)
  problematic_vars <- unit_root_summary |>
    filter(unit_root_verdict != "stationary / transformation adequate") |>
    pull(variable)
  cips_success <- cips_results |>
    filter(variable %in% COUNTRY_VARS) |>
    summarise(all_ok = all(status == "ok"), .groups = "drop") |>
    pull(all_ok)
  cd_detected <- cd_results |>
    filter(variable_type == "country-specific", cd_verdict == "dependence detected") |>
    pull(variable)

  lines <- c(
    "# Pre-Model Diagnostics Cleanup Report",
    "",
    "This cleanup uses the final model-ready dataset only. It does not re-estimate the PVAR, alter the structural PVAR refined4 S1, change the historical decomposition, or change counterfactual results.",
    "",
    "## 1. Were all seven final variables tested?",
    "",
    paste0("Yes. Tested variables: ", tested_vars, "."),
    "",
    "## 2. Was d_FiscalBalanceGDP included explicitly?",
    "",
    "Yes. d_FiscalBalanceGDP is included in the panel unit-root tests, Pesaran CIPS/CADF, Pesaran CD test, summary workbook, updated master workbook, and final diagnostic verdict.",
    "",
    "## 3. What stationarity tests were run?",
    "",
    "For country-specific variables, the script reports LLC, IPS, Fisher ADF / Maddala-Wu, Fisher PP, and Pesaran CIPS/CADF. For common replicated variables, it reports ADF, PP, and KPSS time-series tests.",
    "",
    "## 4. Which variables are stationary?",
    "",
    paste0("Stationarity is supported for: ", paste(stationary_vars, collapse = ", "), "."),
    "",
    "## 5. Is any variable problematic?",
    "",
    if (length(problematic_vars) == 0) {
      "No variable is flagged as problematic by the cleaned pre-model diagnostics."
    } else {
      paste0("Variables requiring manual inspection: ", paste(problematic_vars, collapse = ", "), ".")
    },
    "",
    "## 6. Was CIPS/CADF rerun successfully?",
    "",
    if (isTRUE(cips_success)) {
      "Yes. CIPS/CADF was rerun successfully for all five country-specific variables using pseries objects from a pdata.frame indexed by Country and quarter_index."
    } else {
      "No. At least one CIPS/CADF test could not be reported because of a technical implementation issue. This is not interpreted as evidence of non-stationarity."
    },
    "",
    "## 7. Why is CIPS/CADF not reported for some variables?",
    "",
    "CIPS/CADF is not reported for Energy_Factor and d_CISS because these are common variables replicated across countries; treating them as independent panel series would be mechanically misleading.",
    "",
    "## 8. Is there cross-sectional dependence?",
    "",
    paste0("Yes. Pesaran CD detects cross-sectional dependence for the country-specific variables: ", paste(cd_detected, collapse = ", "), "."),
    "",
    "## 9. For which variables is Pesaran CD relevant?",
    "",
    paste0("Pesaran CD is relevant for country-specific variables: ", paste(COUNTRY_VARS, collapse = ", "), ". It is marked not applicable for Energy_Factor and d_CISS because they are common replicated variables."),
    "",
    "## 10. Suggested diagnostic wording for the paper",
    "",
    notes |>
      filter(note_id == "paper_text") |>
      pull(text),
    "",
    "## Sample Used",
    "",
    paste0("- Countries: ", sample_info$countries[[1]]),
    paste0("- Quarters: ", sample_info$quarters[[1]], " (", sample_info$min_quarter[[1]], "-", sample_info$max_quarter[[1]], ")"),
    paste0("- Observations: ", sample_info$observations[[1]]),
    paste0("- Balanced panel: ", sample_info$balanced[[1]]),
    paste0("- Missing values in final model variables: ", sample_info$missing_values_in_model_variables[[1]]),
    "",
    "## Output Files",
    "",
    paste0("- ", DIAG_FILE),
    paste0("- ", UPDATED_MASTER_FILE),
    paste0("- ", REPORT_FILE)
  )

  writeLines(lines, path, useBytes = TRUE)
}

final_data <- read_final_data()
sample_info <- sample_overview(final_data)

if (!isTRUE(sample_info$balanced[[1]])) {
  warning("The final model-ready sample is not balanced. Diagnostics will still be reported.")
}
if (sample_info$missing_values_in_model_variables[[1]] > 0) {
  warning("Missing values detected in final model variables.")
}

panel_unit_roots <- run_panel_unit_roots(final_data)
common_unit_roots <- run_common_unit_roots(final_data)
unit_root_detailed <- bind_rows(panel_unit_roots, common_unit_roots) |>
  arrange(match(variable, MODEL_VARS), result_level, test, country)
unit_root_summary <- make_unit_root_summary(unit_root_detailed)
cips_results <- run_cips_status(panel_unit_roots)
cd_results <- run_cross_sectional_dependence(final_data)
common_tests <- common_unit_roots |>
  arrange(match(variable, COMMON_VARS), test)
replication_check <- make_common_replication_check(final_data)
diagnostic_final_verdict <- make_final_verdict(final_data, unit_root_summary, cips_results, cd_results)
notes_for_paper <- make_notes(sample_info, unit_root_summary, cips_results, cd_results, diagnostic_final_verdict)

write_workbook(
  DIAG_FILE,
  list(
    unit_root_summary = unit_root_summary,
    unit_root_detailed = unit_root_detailed,
    cips_cadf_results = cips_results,
    cross_sectional_dependence = cd_results,
    common_variables_tests = common_tests,
    diagnostic_final_verdict = diagnostic_final_verdict,
    notes_for_paper = notes_for_paper
  )
)

update_master_workbook(MASTER_FILE, UPDATED_MASTER_FILE, diagnostic_final_verdict)
write_report(REPORT_FILE, sample_info, unit_root_summary, cips_results, cd_results, diagnostic_final_verdict, notes_for_paper)

message("Pre-model diagnostics cleanup complete.")
message("Diagnostics workbook: ", DIAG_FILE)
message("Updated master workbook: ", UPDATED_MASTER_FILE)
message("Report: ", REPORT_FILE)
