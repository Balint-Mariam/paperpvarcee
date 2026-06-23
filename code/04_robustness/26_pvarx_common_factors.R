# PVARX robustness with country-specific endogenous variables and common factors.
# The model uses country fixed effects and no time fixed effects.

options(stringsAsFactors = FALSE)

required_packages <- c(
  "openxlsx", "dplyr", "tidyr", "tibble", "plm", "lmtest",
  "sandwich", "ggplot2", "scales"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required packages: ", paste(missing_packages, collapse = ", "),
    ". Run Rscript code/00_install_packages.R first."
  )
}

suppressPackageStartupMessages({
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(plm)
  library(lmtest)
  library(sandwich)
  library(ggplot2)
  library(scales)
})

DATA_FILE <- file.path("outputs", "01_model_ready_data", "model_ready_dataset.xlsx")
REFERENCE_DK_FILE <- file.path(
  "outputs", "02_tables", "robustness", "dk_inference",
  "FE_LSDV_PVAR_DK_inference.xlsx"
)
OUTPUT_DIR <- file.path("outputs", "02_tables", "robustness", "pvarx_common_factors")
FIGURE_DIR <- file.path("outputs", "03_figures", "appendix", "pvarx_common_factors")
REPORT_FILE <- file.path("outputs", "04_reports", "PVARX_common_factors_report.md")
LOG_FILE <- file.path("outputs", "05_logs", "PVARX_common_factors_run_log.txt")
OUTPUT_FILE <- file.path(OUTPUT_DIR, "PVARX_common_factors_results.xlsx")

ENDOGENOUS <- c(
  "d_CPI",
  "GDP_Growth",
  "d_3MRate",
  "d_FiscalBalanceGDP",
  "dlog_CDS"
)
COMMON_FACTORS <- c("Energy_Factor", "d_CISS")
DK_PRIMARY_LAG <- 4L
DK_SENSITIVITY_LAGS <- c(2L, 4L, 6L)
MULTIPLIER_HORIZON <- 12L

SPECIFICATIONS <- tibble::tribble(
  ~specification, ~include_factor_lags, ~description,
  "baseline_current_factors", FALSE,
  "Five country-specific endogenous variables with one endogenous lag and contemporaneous common factors.",
  "extended_current_and_lagged_factors", TRUE,
  "Five country-specific endogenous variables with one endogenous lag and contemporaneous plus lagged common factors."
)

KEY_CHANNELS <- tibble::tribble(
  ~relation, ~response, ~regressor, ~timing, ~assessment_rule,
  "Energy_Factor -> dlog_CDS", "dlog_CDS", "Energy_Factor", "current common factor", "positive",
  "d_CISS -> dlog_CDS", "dlog_CDS", "d_CISS", "current common factor", "positive",
  "d_CPI -> dlog_CDS", "dlog_CDS", "L1_d_CPI", "one-quarter lag", "positive",
  "Energy_Factor -> d_CPI", "d_CPI", "Energy_Factor", "current common factor", "positive",
  "Energy_Factor -> d_3MRate", "d_3MRate", "Energy_Factor", "current common factor", "positive",
  "d_3MRate -> dlog_CDS", "dlog_CDS", "L1_d_3MRate", "one-quarter lag", "weak_direct",
  "d_FiscalBalanceGDP -> dlog_CDS", "dlog_CDS", "L1_d_FiscalBalanceGDP", "one-quarter lag", "weak_direct"
)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(REPORT_FILE), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(LOG_FILE), recursive = TRUE, showWarnings = FALSE)

stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

significance_label <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "not available",
    p < 0.01 ~ "significant at 1%",
    p < 0.05 ~ "significant at 5%",
    p < 0.10 ~ "significant at 10%",
    TRUE ~ "not significant at 10%"
  )
}

format_number <- function(x, digits = 4L) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

format_p <- function(x) {
  ifelse(
    is.na(x),
    "NA",
    ifelse(x < 0.001, "<0.001", formatC(x, format = "f", digits = 3L))
  )
}

matrix_to_table <- function(x, row_label = "response") {
  as.data.frame(x, check.names = FALSE) |>
    tibble::rownames_to_column(row_label)
}

read_and_prepare_data <- function() {
  if (!file.exists(DATA_FILE)) {
    stop("Model-ready dataset not found: ", DATA_FILE)
  }

  sheets <- openxlsx::getSheetNames(DATA_FILE)
  sheet <- if ("model_ready_dataset" %in% sheets) {
    "model_ready_dataset"
  } else {
    sheets[[1L]]
  }

  raw <- openxlsx::read.xlsx(DATA_FILE, sheet = sheet) |>
    as_tibble()

  required_columns <- c("Country", "Quarter_ID", ENDOGENOUS, COMMON_FACTORS)
  missing_columns <- setdiff(required_columns, names(raw))
  if (length(missing_columns) > 0L) {
    stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
  }

  dat <- raw |>
    mutate(
      Country = as.character(Country),
      Quarter_ID = as.character(Quarter_ID),
      across(all_of(c(ENDOGENOUS, COMMON_FACTORS)), as.numeric),
      quarter_index = match(Quarter_ID, sort(unique(Quarter_ID)))
    ) |>
    arrange(Country, quarter_index)

  duplicate_count <- dat |>
    count(Country, Quarter_ID, name = "n") |>
    filter(n > 1L) |>
    nrow()
  if (duplicate_count > 0L) {
    stop("Duplicate country-quarter rows detected: ", duplicate_count)
  }

  missing_count <- sum(is.na(dat[c(ENDOGENOUS, COMMON_FACTORS)]))
  if (missing_count > 0L) {
    stop("Missing values detected in required model variables: ", missing_count)
  }

  common_check <- dat |>
    group_by(Quarter_ID, quarter_index) |>
    summarise(
      countries = n_distinct(Country),
      Energy_Factor_unique = n_distinct(Energy_Factor, na.rm = TRUE),
      d_CISS_unique = n_distinct(d_CISS, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      common_factor_check = Energy_Factor_unique == 1L & d_CISS_unique == 1L,
      complete_country_count = countries == n_distinct(dat$Country)
    )

  if (!all(common_check$common_factor_check)) {
    stop("Common factors are not unique by quarter.")
  }

  common_ts <- dat |>
    distinct(Quarter_ID, quarter_index, Energy_Factor, d_CISS) |>
    arrange(quarter_index) |>
    mutate(
      L1_Energy_Factor = dplyr::lag(Energy_Factor),
      L1_d_CISS = dplyr::lag(d_CISS)
    )

  prepared <- dat |>
    select(-all_of(COMMON_FACTORS)) |>
    left_join(common_ts, by = c("Quarter_ID", "quarter_index")) |>
    arrange(Country, quarter_index) |>
    group_by(Country) |>
    mutate(across(all_of(ENDOGENOUS), dplyr::lag, .names = "L1_{.col}")) |>
    ungroup()

  baseline_required <- c(ENDOGENOUS, paste0("L1_", ENDOGENOUS), COMMON_FACTORS)
  extended_required <- c(baseline_required, paste0("L1_", COMMON_FACTORS))

  baseline_data <- prepared |>
    filter(if_all(all_of(baseline_required), ~ !is.na(.x)))
  extended_data <- prepared |>
    filter(if_all(all_of(extended_required), ~ !is.na(.x)))

  expected_rows <- n_distinct(dat$Country) * (n_distinct(dat$Quarter_ID) - 1L)
  if (nrow(baseline_data) != expected_rows || nrow(extended_data) != expected_rows) {
    stop("Unexpected effective sample size after lag construction.")
  }

  data_checks <- tibble(
    check = c(
      "Countries in source data",
      "Quarters in source data",
      "Rows in source data",
      "Duplicate country-quarter rows",
      "Missing required values",
      "Quarters with non-common Energy_Factor",
      "Quarters with non-common d_CISS",
      "Rows in baseline estimation sample",
      "Rows in extended estimation sample",
      "Time fixed effects included"
    ),
    value = as.character(c(
      n_distinct(dat$Country),
      n_distinct(dat$Quarter_ID),
      nrow(dat),
      duplicate_count,
      missing_count,
      sum(common_check$Energy_Factor_unique != 1L),
      sum(common_check$d_CISS_unique != 1L),
      nrow(baseline_data),
      nrow(extended_data),
      FALSE
    )),
    status = c(
      "pass", "pass", "pass", "pass", "pass",
      "pass", "pass", "pass", "pass", "required specification"
    )
  )

  list(
    source = dat,
    baseline = baseline_data,
    extended = extended_data,
    common_ts = common_ts,
    common_check = common_check,
    data_checks = data_checks,
    source_sheet = sheet
  )
}

formula_for <- function(response, include_factor_lags) {
  regressors <- c(paste0("L1_", ENDOGENOUS), COMMON_FACTORS)
  if (isTRUE(include_factor_lags)) {
    regressors <- c(regressors, paste0("L1_", COMMON_FACTORS))
  }
  stats::as.formula(paste(response, "~", paste(regressors, collapse = " + ")))
}

fit_specification <- function(data, specification, include_factor_lags) {
  pdata <- plm::pdata.frame(
    as.data.frame(data),
    index = c("Country", "quarter_index"),
    drop.index = FALSE,
    row.names = TRUE
  )

  fits <- lapply(ENDOGENOUS, function(response) {
    plm::plm(
      formula_for(response, include_factor_lags),
      data = pdata,
      model = "within",
      effect = "individual"
    )
  })
  names(fits) <- ENDOGENOUS

  list(
    specification = specification,
    include_factor_lags = include_factor_lags,
    data = data,
    fits = fits
  )
}

safe_vcov <- function(fit, dk_lag = NULL) {
  tryCatch(
    {
      if (is.null(dk_lag)) {
        stats::vcov(fit)
      } else {
        plm::vcovSCC(fit, type = "HC1", maxlag = dk_lag)
      }
    },
    error = function(e) {
      beta_names <- names(stats::coef(fit))
      matrix(
        NA_real_,
        nrow = length(beta_names),
        ncol = length(beta_names),
        dimnames = list(beta_names, beta_names)
      )
    }
  )
}

extract_equation <- function(fit, response, specification, dk_lag) {
  beta <- stats::coef(fit)
  terms <- names(beta)
  classic_vcov <- safe_vcov(fit)
  dk_vcov <- safe_vcov(fit, dk_lag)
  classic_se <- sqrt(pmax(diag(classic_vcov), 0))
  dk_se <- sqrt(pmax(diag(dk_vcov), 0))
  residual_df <- max(1, stats::df.residual(fit))
  critical <- stats::qt(0.975, df = residual_df)
  classic_t <- beta / classic_se
  dk_t <- beta / dk_se
  classic_p <- 2 * stats::pt(abs(classic_t), df = residual_df, lower.tail = FALSE)
  dk_p <- 2 * stats::pt(abs(dk_t), df = residual_df, lower.tail = FALSE)

  tibble(
    specification = specification,
    dependent_variable = response,
    regressor = terms,
    cause = sub("^L1_", "", terms),
    timing = case_when(
      startsWith(terms, "L1_") ~ "one-quarter lag",
      terms %in% COMMON_FACTORS ~ "current common factor",
      TRUE ~ "other"
    ),
    coefficient = as.numeric(beta),
    classic_standard_error = as.numeric(classic_se),
    classic_t_statistic = as.numeric(classic_t),
    classic_p_value = as.numeric(classic_p),
    DK_lag = as.integer(dk_lag),
    DK_standard_error = as.numeric(dk_se),
    DK_t_statistic = as.numeric(dk_t),
    DK_p_value = as.numeric(dk_p),
    DK_ci_lower_95 = as.numeric(beta - critical * dk_se),
    DK_ci_upper_95 = as.numeric(beta + critical * dk_se),
    DK_stars = stars(as.numeric(dk_p)),
    residual_df = residual_df
  )
}

extract_system <- function(fitted_specification, dk_lag) {
  bind_rows(lapply(ENDOGENOUS, function(response) {
    extract_equation(
      fitted_specification$fits[[response]],
      response,
      fitted_specification$specification,
      dk_lag
    )
  }))
}

make_model_summary <- function(fitted_specification) {
  bind_rows(lapply(ENDOGENOUS, function(response) {
    fit <- fitted_specification$fits[[response]]
    fit_summary <- summary(fit)
    tibble(
      specification = fitted_specification$specification,
      dependent_variable = response,
      observations = stats::nobs(fit),
      countries = n_distinct(fitted_specification$data$Country),
      quarters = n_distinct(fitted_specification$data$Quarter_ID),
      within_R_squared = unname(fit_summary$r.squared[["rsq"]]),
      adjusted_within_R_squared = unname(fit_summary$r.squared[["adjrsq"]]),
      country_fixed_effects = TRUE,
      time_fixed_effects = FALSE
    )
  }))
}

make_residual_cd <- function(fitted_specification) {
  bind_rows(lapply(ENDOGENOUS, function(response) {
    fit <- fitted_specification$fits[[response]]
    test <- tryCatch(
      plm::pcdtest(fit, test = "cd"),
      error = function(e) NULL
    )
    tibble(
      specification = fitted_specification$specification,
      dependent_variable = response,
      Pesaran_CD_statistic = if (is.null(test)) NA_real_ else unname(test$statistic),
      p_value = if (is.null(test)) NA_real_ else test$p.value,
      inference_response = "Driscoll-Kraay standard errors reported at coefficient level"
    )
  }))
}

make_factor_tests <- function(fitted_specification, dk_lag) {
  bind_rows(lapply(ENDOGENOUS, function(response) {
    fit <- fitted_specification$fits[[response]]
    beta <- stats::coef(fit)
    dk_vcov <- safe_vcov(fit, dk_lag)
    residual_df <- max(1, stats::df.residual(fit))

    bind_rows(lapply(COMMON_FACTORS, function(common_factor) {
      tested_terms <- common_factor
      lagged_term <- paste0("L1_", common_factor)
      if (isTRUE(fitted_specification$include_factor_lags) && lagged_term %in% names(beta)) {
        tested_terms <- c(tested_terms, lagged_term)
      }

      tested_beta <- beta[tested_terms]
      tested_vcov <- dk_vcov[tested_terms, tested_terms, drop = FALSE]
      solved <- tryCatch(
        qr.solve(tested_vcov, tested_beta),
        error = function(e) rep(NA_real_, length(tested_beta))
      )
      wald_statistic <- if (anyNA(solved)) {
        NA_real_
      } else {
        drop(t(tested_beta) %*% solved)
      }
      wald_p_value <- if (is.na(wald_statistic)) {
        NA_real_
      } else {
        stats::pchisq(wald_statistic, df = length(tested_terms), lower.tail = FALSE)
      }

      sum_weights <- rep(1, length(tested_terms))
      coefficient_sum <- sum(tested_beta)
      sum_variance <- drop(t(sum_weights) %*% tested_vcov %*% sum_weights)
      sum_standard_error <- sqrt(max(sum_variance, 0))
      sum_t_statistic <- coefficient_sum / sum_standard_error
      sum_p_value <- 2 * stats::pt(
        abs(sum_t_statistic),
        df = residual_df,
        lower.tail = FALSE
      )

      tibble(
        specification = fitted_specification$specification,
        dependent_variable = response,
        common_factor = common_factor,
        tested_terms = paste(tested_terms, collapse = " + "),
        restriction_count = length(tested_beta),
        DK_lag = dk_lag,
        Wald_chi_square = wald_statistic,
        Wald_p_value = wald_p_value,
        coefficient_sum = coefficient_sum,
        sum_DK_standard_error = sum_standard_error,
        sum_t_statistic = sum_t_statistic,
        sum_p_value = sum_p_value,
        sum_sign = case_when(
          coefficient_sum > 0 ~ "positive",
          coefficient_sum < 0 ~ "negative",
          TRUE ~ "zero"
        )
      )
    }))
  }))
}

coefficient_matrix <- function(coefficient_table, specification, matrix_type) {
  regressor_names <- switch(
    matrix_type,
    A = paste0("L1_", ENDOGENOUS),
    B = COMMON_FACTORS,
    C = paste0("L1_", COMMON_FACTORS)
  )
  column_names <- switch(
    matrix_type,
    A = ENDOGENOUS,
    B = COMMON_FACTORS,
    C = COMMON_FACTORS
  )

  output <- matrix(
    0,
    nrow = length(ENDOGENOUS),
    ncol = length(regressor_names),
    dimnames = list(ENDOGENOUS, column_names)
  )
  selected <- coefficient_table |>
    filter(
      .data$specification == .env$specification,
      DK_lag == DK_PRIMARY_LAG,
      regressor %in% regressor_names
    )

  for (i in seq_len(nrow(selected))) {
    row_name <- selected$dependent_variable[[i]]
    col_name <- sub("^L1_", "", selected$regressor[[i]])
    output[row_name, col_name] <- selected$coefficient[[i]]
  }
  output
}

make_stability <- function(A, specification) {
  roots <- eigen(A)$values
  root_table <- tibble(
    specification = specification,
    root = seq_along(roots),
    real = Re(roots),
    imaginary = Im(roots),
    modulus = Mod(roots),
    inside_unit_circle = Mod(roots) < 1
  )
  summary_table <- tibble(
    specification = specification,
    stable = all(root_table$inside_unit_circle),
    maximum_root_modulus = max(root_table$modulus),
    stability_rule = "All eigenvalue moduli must be below one"
  )
  list(roots = root_table, summary = summary_table)
}

make_residual_covariance <- function(fitted_specification) {
  residual_frames <- lapply(ENDOGENOUS, function(response) {
    fit <- fitted_specification$fits[[response]]
    fit_index <- plm::index(fit)
    out <- tibble(
      Country = as.character(fit_index[[1L]]),
      quarter_index = as.integer(as.character(fit_index[[2L]])),
      residual = as.numeric(stats::residuals(fit))
    )
    names(out)[[3L]] <- response
    out
  })

  residual_wide <- Reduce(
    function(x, y) full_join(x, y, by = c("Country", "quarter_index")),
    residual_frames
  ) |>
    arrange(Country, quarter_index)

  covariance <- stats::cov(residual_wide[ENDOGENOUS], use = "complete.obs")
  list(wide = residual_wide, covariance = covariance)
}

make_dynamic_multipliers <- function(A, B, C, shock_sd, specification, horizon) {
  output <- list()
  counter <- 1L

  for (shock in COMMON_FACTORS) {
    scale_value <- shock_sd$standard_deviation[shock_sd$factor == shock]
    current_response <- B[, shock, drop = TRUE] * scale_value

    for (h in 0:horizon) {
      if (h == 0L) {
        response_h <- current_response
      } else if (h == 1L) {
        response_h <- as.numeric(A %*% current_response)
        names(response_h) <- ENDOGENOUS
        if (!is.null(C)) {
          response_h <- response_h + C[, shock, drop = TRUE] * scale_value
        }
        current_response <- response_h
      } else {
        response_h <- as.numeric(A %*% current_response)
        names(response_h) <- ENDOGENOUS
        current_response <- response_h
      }

      output[[counter]] <- tibble(
        specification = specification,
        shock = shock,
        shock_size = scale_value,
        horizon = h,
        response = ENDOGENOUS,
        multiplier = as.numeric(response_h)
      )
      counter <- counter + 1L
    }
  }

  bind_rows(output) |>
    group_by(specification, shock, response) |>
    arrange(horizon, .by_group = TRUE) |>
    mutate(cumulative_multiplier = cumsum(multiplier)) |>
    ungroup()
}

make_key_channel_table <- function(primary_coefficients) {
  primary_coefficients |>
    inner_join(KEY_CHANNELS, by = c("dependent_variable" = "response", "regressor")) |>
    mutate(
      relation = relation,
      sign = case_when(
        coefficient > 0 ~ "positive",
        coefficient < 0 ~ "negative",
        TRUE ~ "zero"
      ),
      significance = significance_label(DK_p_value),
      requested_check = case_when(
        assessment_rule == "positive" & coefficient > 0 & DK_p_value < 0.10 ~ "positive and statistically supported",
        assessment_rule == "positive" & coefficient > 0 ~ "positive but statistically weak",
        assessment_rule == "positive" ~ "sign differs from requested positive channel",
        assessment_rule == "weak_direct" & DK_p_value >= 0.10 ~ "direct channel remains statistically weak",
        assessment_rule == "weak_direct" ~ "direct channel is statistically informative",
        TRUE ~ "not classified"
      )
    ) |>
    select(
      specification,
      relation,
      timing = timing.y,
      coefficient,
      DK_standard_error,
      DK_t_statistic,
      DK_p_value,
      DK_ci_lower_95,
      DK_ci_upper_95,
      DK_stars,
      sign,
      significance,
      requested_check,
      assessment_rule
    ) |>
    arrange(factor(specification, levels = SPECIFICATIONS$specification), match(relation, KEY_CHANNELS$relation))
}

make_sensitivity_table <- function(all_coefficients) {
  all_coefficients |>
    inner_join(KEY_CHANNELS, by = c("dependent_variable" = "response", "regressor")) |>
    select(specification, relation, coefficient, DK_lag, DK_p_value) |>
    pivot_wider(
      names_from = DK_lag,
      values_from = DK_p_value,
      names_prefix = "DK_p_lag_"
    ) |>
    mutate(
      positive_coefficient = coefficient > 0,
      significant_all_DK_lags = if_all(starts_with("DK_p_lag_"), ~ .x < 0.10),
      significant_any_DK_lag = if_any(starts_with("DK_p_lag_"), ~ .x < 0.10)
    ) |>
    arrange(factor(specification, levels = SPECIFICATIONS$specification), match(relation, KEY_CHANNELS$relation))
}

make_reference_comparison <- function(key_channels) {
  if (!file.exists(REFERENCE_DK_FILE)) {
    return(tibble(note = "Reference DK workbook was not available."))
  }

  reference <- openxlsx::read.xlsx(
    REFERENCE_DK_FILE,
    sheet = "key_channels_original_vs_DK"
  ) |>
    as_tibble() |>
    transmute(
      relation = as.character(relation),
      seven_endogenous_coefficient = as.numeric(coefficient),
      seven_endogenous_DK_p_value = as.numeric(DK_p_value)
    )

  key_channels |>
    filter(specification == "baseline_current_factors") |>
    left_join(reference, by = "relation") |>
    mutate(
      same_sign = sign(coefficient) == sign(seven_endogenous_coefficient),
      timing_comparison = if_else(
        grepl("Energy_Factor|d_CISS", relation),
        "PVARX uses a current common factor; the seven-endogenous system used its lag as an endogenous regressor.",
        "Both systems use a one-quarter lag for this country-specific regressor."
      )
    ) |>
    select(
      relation,
      timing,
      PVARX_coefficient = coefficient,
      PVARX_DK_p_value = DK_p_value,
      seven_endogenous_coefficient,
      seven_endogenous_DK_p_value,
      same_sign,
      timing_comparison
    )
}

save_plot <- function(plot, file_stem, width = 10, height = 6) {
  ggplot2::ggsave(
    filename = file.path(FIGURE_DIR, paste0(file_stem, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 320,
    bg = "white"
  )
  tryCatch(
    ggplot2::ggsave(
      filename = file.path(FIGURE_DIR, paste0(file_stem, ".pdf")),
      plot = plot,
      width = width,
      height = height,
      device = grDevices::cairo_pdf,
      bg = "white"
    ),
    error = function(e) {
      ggplot2::ggsave(
        filename = file.path(FIGURE_DIR, paste0(file_stem, ".pdf")),
        plot = plot,
        width = width,
        height = height,
        device = "pdf",
        bg = "white"
      )
    }
  )
}

make_plots <- function(key_channels, multipliers) {
  key_plot_data <- key_channels |>
    mutate(
      specification_label = recode(
        specification,
        baseline_current_factors = "Current factors",
        extended_current_and_lagged_factors = "Current + lagged factors"
      ),
      relation = factor(relation, levels = rev(KEY_CHANNELS$relation))
    )

  key_plot <- ggplot(
    key_plot_data,
    aes(x = coefficient, y = relation, color = specification_label)
  ) +
    geom_vline(xintercept = 0, color = "grey55", linewidth = 0.35) +
    geom_errorbar(
      aes(xmin = DK_ci_lower_95, xmax = DK_ci_upper_95),
      orientation = "y",
      width = 0.18,
      position = position_dodge(width = 0.5),
      linewidth = 0.55
    ) +
    geom_point(position = position_dodge(width = 0.5), size = 2.2) +
    scale_color_manual(values = c("Current factors" = "#236B8E", "Current + lagged factors" = "#B04A3A")) +
    labs(
      title = "PVARX key channels with Driscoll-Kraay inference",
      subtitle = "Points are FE/LSDV coefficients; bars are 95% DK intervals with lag 4.",
      x = "Coefficient",
      y = NULL,
      color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  save_plot(key_plot, "PVARX_key_channels_DK4", width = 10.5, height = 6.5)

  multiplier_plot_data <- multipliers |>
    mutate(
      specification_label = recode(
        specification,
        baseline_current_factors = "Current factors",
        extended_current_and_lagged_factors = "Current + lagged factors"
      ),
      shock_label = recode(
        shock,
        Energy_Factor = "+1 SD Energy Factor",
        d_CISS = "+1 SD CISS change"
      )
    )

  multiplier_plot <- ggplot(
    multiplier_plot_data,
    aes(x = horizon, y = multiplier, color = shock_label, linetype = specification_label)
  ) +
    geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~ response, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = seq(0, MULTIPLIER_HORIZON, by = 2)) +
    scale_color_manual(values = c("+1 SD Energy Factor" = "#B04A3A", "+1 SD CISS change" = "#236B8E")) +
    labs(
      title = "PVARX dynamic multipliers for common-factor shocks",
      subtitle = "One-quarter shocks scaled by the time-series standard deviation of each common factor.",
      x = "Horizon (quarters)",
      y = "Response",
      color = NULL,
      linetype = NULL
    ) +
    theme_minimal(base_size = 10.5) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  save_plot(multiplier_plot, "PVARX_dynamic_multipliers_all", width = 10.5, height = 9)

  cds_plot <- multiplier_plot_data |>
    filter(response == "dlog_CDS") |>
    ggplot(aes(x = horizon, y = multiplier, color = shock_label, linetype = specification_label)) +
    geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.7) +
    scale_x_continuous(breaks = 0:MULTIPLIER_HORIZON) +
    scale_color_manual(values = c("+1 SD Energy Factor" = "#B04A3A", "+1 SD CISS change" = "#236B8E")) +
    labs(
      title = "PVARX dynamic response of sovereign-risk repricing",
      subtitle = "Response of dlog_CDS to one-quarter common-factor shocks.",
      x = "Horizon (quarters)",
      y = "dlog_CDS response",
      color = NULL,
      linetype = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  save_plot(cds_plot, "PVARX_dynamic_multipliers_dlog_CDS", width = 9.5, height = 5.8)
}

write_report <- function(
  data_bundle,
  key_channels,
  factor_tests,
  stability_summary,
  common_stats,
  multipliers
) {
  baseline_keys <- key_channels |>
    filter(specification == "baseline_current_factors")
  extended_keys <- key_channels |>
    filter(specification == "extended_current_and_lagged_factors")

  channel_lines <- function(x) {
    paste0(
      "- ", x$relation,
      ": coefficient = ", format_number(x$coefficient),
      ", DK p-value = ", format_p(x$DK_p_value),
      ", ", x$requested_check, "."
    )
  }

  cds_multipliers <- multipliers |>
    filter(response == "dlog_CDS", horizon %in% c(0L, 1L, 4L, 12L)) |>
    mutate(
      line = paste0(
        "- ", specification, ", ", shock, ", h=", horizon,
        ": ", format_number(multiplier, 5L)
      )
    ) |>
    pull(line)

  cds_factor_tests <- factor_tests |>
    filter(dependent_variable == "dlog_CDS") |>
    mutate(
      line = paste0(
        "- ", specification, ", ", common_factor,
        ": tested terms = ", tested_terms,
        ", Wald p-value = ", format_p(Wald_p_value),
        ", coefficient sum = ", format_number(coefficient_sum),
        ", sum p-value = ", format_p(sum_p_value), "."
      )
    ) |>
    pull(line)

  report <- c(
    "# PVARX Common-Factor Robustness",
    "",
    "## Specification",
    "",
    "The endogenous block contains d_CPI, GDP_Growth, d_3MRate, d_FiscalBalanceGDP, and dlog_CDS. Energy_Factor and d_CISS enter as common factors. Country fixed effects are included. Time fixed effects are excluded because they would be collinear with the common factors.",
    "",
    "The baseline specification includes current common factors. The extended specification includes current and one-quarter-lagged common factors. Driscoll-Kraay standard errors use lag 4, with lag 2 and lag 6 sensitivity checks.",
    "",
    "## Data Checks",
    "",
    paste0("- Countries: ", n_distinct(data_bundle$source$Country), "."),
    paste0("- Source quarters: ", n_distinct(data_bundle$source$Quarter_ID), "."),
    paste0("- Baseline estimation observations: ", nrow(data_bundle$baseline), "."),
    paste0("- Energy_Factor values per quarter: exactly one in all quarters."),
    paste0("- d_CISS values per quarter: exactly one in all quarters."),
    "",
    "## Baseline Key Channels",
    "",
    channel_lines(baseline_keys),
    "",
    "## Extended-Specification Key Channels",
    "",
    channel_lines(extended_keys),
    "",
    "## Stability",
    "",
    paste0(
      "- ", stability_summary$specification,
      ": stable = ", stability_summary$stable,
      ", maximum root modulus = ", format_number(stability_summary$maximum_root_modulus), "."
    ),
    "",
    "## Joint Common-Factor Tests in the dlog_CDS Equation",
    "",
    cds_factor_tests,
    "",
    "## Common-Factor Shock Scaling",
    "",
    paste0(
      "- ", common_stats$factor,
      ": time-series standard deviation = ", format_number(common_stats$standard_deviation), "."
    ),
    "",
    "## dlog_CDS Dynamic Multipliers",
    "",
    cds_multipliers,
    "",
    "## Methodological Scope",
    "",
    "The estimates are conditional on treating the current common factors as exogenous. The extended specification allows lagged common-factor effects but does not by itself establish causal exogeneity. Dynamic multipliers are reduced-form PVARX responses, not structural shocks from a seven-equation endogenous system.",
    "",
    "## Files",
    "",
    paste0("- Workbook: `", gsub("\\\\", "/", OUTPUT_FILE), "`."),
    paste0("- Figures: `", gsub("\\\\", "/", FIGURE_DIR), "/`."),
    paste0("- Run log: `", gsub("\\\\", "/", LOG_FILE), "`.")
  )

  writeLines(report, REPORT_FILE, useBytes = TRUE)
}

main <- function() {
  start_time <- Sys.time()
  data_bundle <- read_and_prepare_data()

  fitted_baseline <- fit_specification(
    data_bundle$baseline,
    "baseline_current_factors",
    FALSE
  )
  fitted_extended <- fit_specification(
    data_bundle$extended,
    "extended_current_and_lagged_factors",
    TRUE
  )
  fitted_systems <- list(fitted_baseline, fitted_extended)

  all_coefficients <- bind_rows(lapply(fitted_systems, function(fitted_specification) {
    bind_rows(lapply(DK_SENSITIVITY_LAGS, function(dk_lag) {
      extract_system(fitted_specification, dk_lag)
    }))
  }))
  primary_coefficients <- all_coefficients |>
    filter(DK_lag == DK_PRIMARY_LAG)

  model_summary <- bind_rows(lapply(fitted_systems, make_model_summary))
  residual_cd <- bind_rows(lapply(fitted_systems, make_residual_cd))
  all_factor_tests <- bind_rows(lapply(fitted_systems, function(fitted_specification) {
    bind_rows(lapply(DK_SENSITIVITY_LAGS, function(dk_lag) {
      make_factor_tests(fitted_specification, dk_lag)
    }))
  }))
  factor_tests <- all_factor_tests |>
    filter(DK_lag == DK_PRIMARY_LAG)

  A_baseline <- coefficient_matrix(
    primary_coefficients,
    "baseline_current_factors",
    "A"
  )
  B_baseline <- coefficient_matrix(
    primary_coefficients,
    "baseline_current_factors",
    "B"
  )
  A_extended <- coefficient_matrix(
    primary_coefficients,
    "extended_current_and_lagged_factors",
    "A"
  )
  B_extended <- coefficient_matrix(
    primary_coefficients,
    "extended_current_and_lagged_factors",
    "B"
  )
  C_extended <- coefficient_matrix(
    primary_coefficients,
    "extended_current_and_lagged_factors",
    "C"
  )

  stability_baseline <- make_stability(A_baseline, "baseline_current_factors")
  stability_extended <- make_stability(A_extended, "extended_current_and_lagged_factors")
  stability_summary <- bind_rows(stability_baseline$summary, stability_extended$summary)
  stability_roots <- bind_rows(stability_baseline$roots, stability_extended$roots)

  residual_baseline <- make_residual_covariance(fitted_baseline)
  residual_extended <- make_residual_covariance(fitted_extended)

  effective_quarters <- sort(unique(data_bundle$baseline$quarter_index))
  common_stats <- data_bundle$common_ts |>
    filter(quarter_index %in% effective_quarters) |>
    summarise(
      across(
        all_of(COMMON_FACTORS),
        list(
          observations = ~ sum(!is.na(.x)),
          mean = ~ mean(.x, na.rm = TRUE),
          standard_deviation = ~ stats::sd(.x, na.rm = TRUE),
          minimum = ~ min(.x, na.rm = TRUE),
          maximum = ~ max(.x, na.rm = TRUE)
        )
      )
    ) |>
    pivot_longer(
      everything(),
      names_to = c("factor", ".value"),
      names_pattern = "(Energy_Factor|d_CISS)_(.*)"
    )

  shock_sd <- common_stats |>
    select(factor, standard_deviation)

  multipliers <- bind_rows(
    make_dynamic_multipliers(
      A_baseline,
      B_baseline,
      NULL,
      shock_sd,
      "baseline_current_factors",
      MULTIPLIER_HORIZON
    ),
    make_dynamic_multipliers(
      A_extended,
      B_extended,
      C_extended,
      shock_sd,
      "extended_current_and_lagged_factors",
      MULTIPLIER_HORIZON
    )
  )

  key_channels <- make_key_channel_table(primary_coefficients)
  sensitivity <- make_sensitivity_table(all_coefficients)
  reference_comparison <- make_reference_comparison(key_channels)

  setup <- tibble(
    item = c(
      "Model class",
      "Endogenous variables",
      "Common factors",
      "Endogenous lags",
      "Country fixed effects",
      "Time fixed effects",
      "Primary DK lag",
      "DK sensitivity lags",
      "Multiplier horizon",
      "Input workbook",
      "Input sheet"
    ),
    value = c(
      "PVARX estimated equation by equation with the within estimator",
      paste(ENDOGENOUS, collapse = ", "),
      paste(COMMON_FACTORS, collapse = ", "),
      "1",
      "yes",
      "no",
      as.character(DK_PRIMARY_LAG),
      paste(DK_SENSITIVITY_LAGS, collapse = ", "),
      as.character(MULTIPLIER_HORIZON),
      DATA_FILE,
      data_bundle$source_sheet
    )
  )

  workbook <- list(
    model_setup = setup,
    data_checks = data_bundle$data_checks,
    common_factor_check = data_bundle$common_check,
    model_summary = model_summary,
    coef_baseline_DK4 = primary_coefficients |>
      filter(specification == "baseline_current_factors"),
    coef_extended_DK4 = primary_coefficients |>
      filter(specification == "extended_current_and_lagged_factors"),
    key_channels = key_channels,
    comparison_full7 = reference_comparison,
    DK_lag_sensitivity = sensitivity,
    stability_summary = stability_summary,
    stability_roots = stability_roots,
    residual_CD = residual_cd,
    factor_joint_tests = factor_tests,
    factor_test_sensitivity = all_factor_tests,
    A_baseline = matrix_to_table(A_baseline),
    B_baseline = matrix_to_table(B_baseline),
    A_extended = matrix_to_table(A_extended),
    B_extended = matrix_to_table(B_extended),
    C_extended = matrix_to_table(C_extended),
    resid_cov_baseline = matrix_to_table(residual_baseline$covariance),
    resid_cov_extended = matrix_to_table(residual_extended$covariance),
    common_factor_stats = common_stats,
    dynamic_multipliers = multipliers,
    dynamic_CDS = multipliers |>
      filter(response == "dlog_CDS")
  )

  openxlsx::write.xlsx(workbook, OUTPUT_FILE, overwrite = TRUE)
  make_plots(key_channels, multipliers)
  write_report(
    data_bundle,
    key_channels,
    factor_tests,
    stability_summary,
    common_stats,
    multipliers
  )

  end_time <- Sys.time()
  log_lines <- c(
    "PVARX common-factor robustness run",
    paste("Started:", format(start_time, "%Y-%m-%d %H:%M:%S")),
    paste("Completed:", format(end_time, "%Y-%m-%d %H:%M:%S")),
    paste("Elapsed seconds:", round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)),
    paste("Baseline rows:", nrow(data_bundle$baseline)),
    paste("Extended rows:", nrow(data_bundle$extended)),
    paste("Baseline stable:", stability_baseline$summary$stable),
    paste("Extended stable:", stability_extended$summary$stable),
    paste("Workbook:", OUTPUT_FILE),
    paste("Report:", REPORT_FILE),
    "Status: completed"
  )
  writeLines(log_lines, LOG_FILE, useBytes = TRUE)

  message(paste(log_lines, collapse = "\n"))
}

main()
