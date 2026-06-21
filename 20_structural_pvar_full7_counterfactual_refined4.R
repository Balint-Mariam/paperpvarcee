# Structural PVAR Counterfactual Analysis - refined4 S1.
# Uses only the validated historical decomposition outputs.
# Does not reestimate the model, does not use repaired4, and does not
# select a new structural draw.

rm(list = ls())

required_packages <- c("openxlsx", "dplyr", "tidyr", "tibble", "ggplot2", "scales")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(scales)
})

HD_DIR <- "structural_pvar_ciss_full7_historical_decomposition_outputs"
STRUCTURAL_DIR <- "structural_pvar_ciss_full7_structural_refined4_outputs"
INPUT_DIR <- "structural_pvar_ciss_full7_final_outputs"
OUTPUT_DIR <- "structural_pvar_ciss_full7_counterfactual_outputs"
FIGURE_DIR <- file.path(OUTPUT_DIR, "figures")
FIGURE_SUBDIRS <- c(
  "cf_panel_average",
  "cf_country_level",
  "cf_cumulative",
  "cf_dlog_CDS_focus",
  "cf_period_summary",
  "paper_figures"
)

MODEL_NAME <- "Structural PVAR refined4 S1 - four-shock sign-only, h=0,1,2"
MODEL_VARIANT <- "S1_four_shock_sign_only_h0_h2"

MODEL_VARS <- c(
  "Energy_Factor",
  "d_CISS",
  "d_CPI",
  "GDP_Growth",
  "d_3MRate",
  "d_FiscalBalanceGDP",
  "dlog_CDS"
)

FOCUS_VARS <- c("dlog_CDS", "d_CPI", "d_3MRate", "GDP_Growth", "d_FiscalBalanceGDP")

SCENARIOS <- data.frame(
  scenario = c(
    "CF1_no_energy",
    "CF2_no_ciss",
    "CF3_no_inflationary_monetary",
    "CF4_no_sovereign",
    "CF5_no_energy_no_inflationary",
    "CF6_no_energy_no_sovereign",
    "CF7_no_macro_financial"
  ),
  scenario_label = c(
    "CF1 - No Energy-carbon pressure shocks",
    "CF2 - No Systemic financial stress shocks",
    "CF3 - No Inflationary monetary-reaction shocks",
    "CF4 - No Sovereign-risk repricing shocks",
    "CF5 - No Energy-carbon and no Inflationary monetary-reaction shocks",
    "CF6 - No Energy-carbon and no Sovereign-risk repricing shocks",
    "CF7 - No identified macro-financial shocks"
  ),
  remove_energy = c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
  remove_ciss = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE),
  remove_inflationary = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
  remove_sovereign = c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE),
  paper_priority = c("main", "main", "secondary", "main", "secondary", "main", "appendix"),
  stringsAsFactors = FALSE
)

PERIODS <- data.frame(
  period = c(
    "Full sample",
    "Pre-energy-inflation period",
    "Energy-inflation and tightening episode",
    "Post-shock normalization",
    "COVID/rebound window"
  ),
  sheet = c(
    "full_sample",
    "pre_energy_inflation",
    "energy_inflation_tightening",
    "post_shock_normalization",
    "covid_rebound_secondary"
  ),
  start = c("2014Q3", "2014Q3", "2021Q1", "2024Q1", "2020Q1"),
  end = c("2025Q4", "2020Q4", "2023Q4", "2025Q4", "2021Q4"),
  stringsAsFactors = FALSE
)

CONTRIBUTION_COLS <- c(
  energy = "contribution_energy",
  ciss = "contribution_ciss",
  inflationary = "contribution_inflationary_monetary",
  sovereign = "contribution_sovereign"
)

CUMULATIVE_VARS <- c("d_CISS", "d_CPI", "d_3MRate", "d_FiscalBalanceGDP", "dlog_CDS")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)
for (subdir in FIGURE_SUBDIRS) {
  dir.create(file.path(FIGURE_DIR, subdir), recursive = TRUE, showWarnings = FALSE)
}

safe_name <- function(x) {
  out <- gsub("[^A-Za-z0-9_]+", "_", x)
  out <- gsub("_+", "_", out)
  gsub("^_|_$", "", out)
}

q_index <- function(q) {
  year <- as.integer(substr(q, 1, 4))
  quarter <- as.integer(sub(".*Q", "", q))
  year * 4L + quarter
}

write_workbook <- function(sheets, path) {
  openxlsx::write.xlsx(sheets, path, overwrite = TRUE)
}

matrix_to_sheet <- function(mat, row_name = "row") {
  as.data.frame(mat, check.names = FALSE) |>
    tibble::rownames_to_column(row_name)
}

load_inputs <- function() {
  setup <- list(
    model_info = openxlsx::read.xlsx(file.path(HD_DIR, "01_hd_model_setup.xlsx"), "model_info"),
    sample_summary = openxlsx::read.xlsx(file.path(HD_DIR, "01_hd_model_setup.xlsx"), "sample_summary"),
    countries = openxlsx::read.xlsx(file.path(HD_DIR, "01_hd_model_setup.xlsx"), "countries"),
    variable_order = openxlsx::read.xlsx(file.path(HD_DIR, "01_hd_model_setup.xlsx"), "variable_order"),
    structural_B_matrix = openxlsx::read.xlsx(file.path(HD_DIR, "01_hd_model_setup.xlsx"), "structural_B_matrix"),
    representative_draw = openxlsx::read.xlsx(file.path(HD_DIR, "01_hd_model_setup.xlsx"), "representative_draw"),
    acceptance_reproduction = openxlsx::read.xlsx(file.path(HD_DIR, "01_hd_model_setup.xlsx"), "acceptance_reproduction"),
    reduced_form_checks = openxlsx::read.xlsx(file.path(HD_DIR, "01_hd_model_setup.xlsx"), "reduced_form_checks"),
    residual_reconstruction = openxlsx::read.xlsx(file.path(HD_DIR, "01_hd_model_setup.xlsx"), "residual_reconstruction"),
    hd_reconstruction = openxlsx::read.xlsx(file.path(HD_DIR, "01_hd_model_setup.xlsx"), "hd_reconstruction")
  )
  country_hd <- openxlsx::read.xlsx(file.path(HD_DIR, "04_hd_country_level.xlsx"), "country_level_long")
  panel_periods_hd <- openxlsx::read.xlsx(file.path(HD_DIR, "03_hd_panel_average.xlsx"), "summary_by_period")
  list(setup = setup, country_hd = country_hd, panel_periods_hd = panel_periods_hd)
}

make_panel_hd <- function(country_hd) {
  country_hd |>
    group_by(quarter_index, Date, Year, Quarter, Quarter_ID, yq_index, variable) |>
    summarise(
      actual = mean(actual, na.rm = TRUE),
      fitted = mean(fitted_reconstructed, na.rm = TRUE),
      contribution_energy = mean(contribution_energy, na.rm = TRUE),
      contribution_ciss = mean(contribution_ciss, na.rm = TRUE),
      contribution_inflationary_monetary = mean(contribution_inflationary_monetary, na.rm = TRUE),
      contribution_sovereign = mean(contribution_sovereign, na.rm = TRUE),
      contribution_other = mean(contribution_other, na.rm = TRUE),
      initial_deterministic_component = mean(initial_deterministic_component, na.rm = TRUE),
      reconstruction_residual = mean(reconstruction_residual, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(variable, yq_index)
}

removed_columns_for_scenario <- function(scenario_row) {
  cols <- character()
  if (isTRUE(scenario_row$remove_energy)) cols <- c(cols, "contribution_energy")
  if (isTRUE(scenario_row$remove_ciss)) cols <- c(cols, "contribution_ciss")
  if (isTRUE(scenario_row$remove_inflationary)) cols <- c(cols, "contribution_inflationary_monetary")
  if (isTRUE(scenario_row$remove_sovereign)) cols <- c(cols, "contribution_sovereign")
  cols
}

make_counterfactual_long <- function(base_df, level) {
  out <- vector("list", nrow(SCENARIOS))
  for (i in seq_len(nrow(SCENARIOS))) {
    sc <- SCENARIOS[i, ]
    removed_energy <- if (isTRUE(sc$remove_energy)) base_df$contribution_energy else 0
    removed_ciss <- if (isTRUE(sc$remove_ciss)) base_df$contribution_ciss else 0
    removed_inflationary <- if (isTRUE(sc$remove_inflationary)) base_df$contribution_inflationary_monetary else 0
    removed_sovereign <- if (isTRUE(sc$remove_sovereign)) base_df$contribution_sovereign else 0
    removed_total <- removed_energy + removed_ciss + removed_inflationary + removed_sovereign
    fitted <- if ("fitted" %in% names(base_df)) base_df$fitted else base_df$fitted_reconstructed
    out[[i]] <- base_df |>
      mutate(
        level = level,
        scenario = sc$scenario,
        scenario_label = sc$scenario_label,
        paper_priority = sc$paper_priority,
        counterfactual = fitted - removed_total,
        gap = fitted - counterfactual,
        removed_total_contribution = removed_total,
        removed_energy_contribution = removed_energy,
        removed_ciss_contribution = removed_ciss,
        removed_inflationary_contribution = removed_inflationary,
        removed_sovereign_contribution = removed_sovereign,
        gap_percent_of_fitted = ifelse(abs(fitted) > 1e-8, gap / fitted, NA_real_)
      )
  }
  bind_rows(out)
}

make_panel_wide <- function(panel_cf_long) {
  panel_cf_long |>
    select(quarter_index, Date, Year, Quarter, Quarter_ID, yq_index, variable, actual, fitted, scenario, counterfactual, gap, gap_percent_of_fitted, removed_total_contribution) |>
    pivot_wider(
      id_cols = c(quarter_index, Date, Year, Quarter, Quarter_ID, yq_index, variable, actual, fitted),
      names_from = scenario,
      values_from = c(counterfactual, gap, gap_percent_of_fitted, removed_total_contribution),
      names_glue = "{.value}_{scenario}"
    ) |>
    rename_with(~ sub("^counterfactual_", "", .x), starts_with("counterfactual_")) |>
    rename_with(~ sub("^gap_", "gap_", .x), starts_with("gap_")) |>
    rename_with(~ sub("^removed_total_contribution_", "removed_", .x), starts_with("removed_total_contribution_")) |>
    arrange(variable, yq_index)
}

make_cumulative <- function(cf_long, group_cols) {
  if (!("fitted" %in% names(cf_long))) {
    cf_long <- cf_long |> mutate(fitted = fitted_reconstructed)
  }
  cf_long |>
    arrange(across(all_of(group_cols)), variable, scenario, yq_index) |>
    group_by(across(all_of(c(group_cols, "variable", "scenario", "scenario_label")))) |>
    mutate(
      cumulative_actual = cumsum(actual),
      cumulative_fitted = cumsum(fitted),
      cumulative_counterfactual = cumsum(counterfactual),
      cumulative_gap = cumsum(gap),
      cumulative_removed_total = cumsum(removed_total_contribution),
      cumulative_removed_energy = cumsum(removed_energy_contribution),
      cumulative_removed_ciss = cumsum(removed_ciss_contribution),
      cumulative_removed_inflationary = cumsum(removed_inflationary_contribution),
      cumulative_removed_sovereign = cumsum(removed_sovereign_contribution),
      dlog_CDS_percent_effect = ifelse(variable == "dlog_CDS", exp(cumulative_gap) - 1, NA_real_)
    ) |>
    ungroup()
}

add_periods <- function(df) {
  bind_rows(lapply(seq_len(nrow(PERIODS)), function(i) {
    start_i <- q_index(PERIODS$start[[i]])
    end_i <- q_index(PERIODS$end[[i]])
    df |>
      filter(yq_index >= start_i, yq_index <= end_i) |>
      mutate(
        period = PERIODS$period[[i]],
        period_sheet = PERIODS$sheet[[i]],
        period_start = PERIODS$start[[i]],
        period_end = PERIODS$end[[i]]
      )
  }))
}

interpretation_flag <- function(variable, cumulative_gap, mean_gap, percent_effect) {
  abs_cum <- abs(cumulative_gap)
  if (variable == "dlog_CDS") {
    if (is.finite(percent_effect) && abs(percent_effect) >= 0.10) return("large")
    if (is.finite(percent_effect) && abs(percent_effect) >= 0.03) return("moderate")
    return("small")
  }
  if (variable %in% c("d_CPI", "d_3MRate", "GDP_Growth", "d_FiscalBalanceGDP")) {
    if (abs_cum >= 1.0 || abs(mean_gap) >= 0.15) return("large")
    if (abs_cum >= 0.25 || abs(mean_gap) >= 0.05) return("moderate")
    return("small")
  }
  if (abs_cum >= 0.5 || abs(mean_gap) >= 0.10) return("moderate")
  "small"
}

make_period_summary <- function(panel_cf_long, country_cf_long) {
  panel_period <- add_periods(panel_cf_long)
  panel_summary <- panel_period |>
    group_by(period, period_sheet, period_start, period_end, variable, scenario, scenario_label, paper_priority) |>
    summarise(
      observations = n(),
      mean_actual = mean(actual, na.rm = TRUE),
      mean_fitted = mean(fitted, na.rm = TRUE),
      mean_counterfactual = mean(counterfactual, na.rm = TRUE),
      mean_gap = mean(gap, na.rm = TRUE),
      cumulative_fitted = sum(fitted, na.rm = TRUE),
      cumulative_counterfactual = sum(counterfactual, na.rm = TRUE),
      cumulative_gap = sum(gap, na.rm = TRUE),
      cumulative_percent_effect_dlog_CDS = ifelse(variable[[1]] == "dlog_CDS", exp(cumulative_gap) - 1, NA_real_),
      .groups = "drop"
    ) |>
    group_by(period, variable) |>
    mutate(
      rank_by_abs_cumulative_effect = rank(-abs(cumulative_gap), ties.method = "first"),
      dominant_scenario_effect = rank_by_abs_cumulative_effect == 1,
      effect_sign = case_when(
        cumulative_gap > 1e-10 ~ "positive",
        cumulative_gap < -1e-10 ~ "negative",
        TRUE ~ "near_zero"
      ),
      interpretation_flag = mapply(interpretation_flag, variable, cumulative_gap, mean_gap, cumulative_percent_effect_dlog_CDS)
    ) |>
    ungroup()

  country_period <- add_periods(country_cf_long)
  country_summary <- country_period |>
    group_by(period, period_sheet, period_start, period_end, Country, variable, scenario, scenario_label, paper_priority) |>
    summarise(
      observations = n(),
      mean_actual = mean(actual, na.rm = TRUE),
      mean_fitted = mean(fitted_reconstructed, na.rm = TRUE),
      mean_counterfactual = mean(counterfactual, na.rm = TRUE),
      mean_gap = mean(gap, na.rm = TRUE),
      cumulative_fitted = sum(fitted_reconstructed, na.rm = TRUE),
      cumulative_counterfactual = sum(counterfactual, na.rm = TRUE),
      cumulative_gap = sum(gap, na.rm = TRUE),
      cumulative_percent_effect_dlog_CDS = ifelse(variable[[1]] == "dlog_CDS", exp(cumulative_gap) - 1, NA_real_),
      .groups = "drop"
    ) |>
    group_by(period, variable, scenario) |>
    mutate(rank_by_abs_cumulative_effect = rank(-abs(cumulative_gap), ties.method = "first")) |>
    ungroup()

  list(panel_summary = panel_summary, country_summary = country_summary)
}

make_country_rankings <- function(country_summary) {
  target <- country_summary |>
    filter(variable == "dlog_CDS", period == "Energy-inflation and tightening episode")
  bind_rows(
    target |> filter(scenario == "CF1_no_energy") |> mutate(ranking_type = "no_energy_dlog_CDS"),
    target |> filter(scenario == "CF4_no_sovereign") |> mutate(ranking_type = "no_sovereign_dlog_CDS"),
    target |> filter(scenario == "CF6_no_energy_no_sovereign") |> mutate(ranking_type = "no_energy_no_sovereign_dlog_CDS")
  ) |>
    group_by(ranking_type) |>
    arrange(desc(abs(cumulative_gap)), .by_group = TRUE) |>
    mutate(rank_abs_gap = row_number()) |>
    ungroup()
}

make_scenario_ranking_dlog_CDS <- function(panel_summary) {
  panel_summary |>
    filter(variable == "dlog_CDS") |>
    group_by(period) |>
    arrange(desc(abs(cumulative_gap)), .by_group = TRUE) |>
    mutate(rank_abs_gap = row_number()) |>
    ungroup()
}

scenario_answer <- function(summary_df, period_name, variable_name) {
  x <- summary_df |>
    filter(period == period_name, variable == variable_name) |>
    arrange(desc(abs(cumulative_gap)))
  if (nrow(x) == 0L) return(NA_character_)
  paste0(x$scenario[[1]], " (", round(x$cumulative_gap[[1]], 4), "; percent effect ", ifelse(is.na(x$cumulative_percent_effect_dlog_CDS[[1]]), "NA", scales::percent(x$cumulative_percent_effect_dlog_CDS[[1]], accuracy = 0.1)), ")")
}

top_countries <- function(country_rankings, ranking_type) {
  x <- country_rankings |>
    filter(ranking_type == !!ranking_type) |>
    arrange(rank_abs_gap) |>
    slice_head(n = 3)
  paste(x$Country, collapse = ", ")
}

make_main_findings <- function(panel_summary, country_summary, country_rankings, hd_panel_periods) {
  full_cds <- scenario_answer(panel_summary, "Full sample", "dlog_CDS")
  episode_cds <- scenario_answer(panel_summary, "Energy-inflation and tightening episode", "dlog_CDS")
  no_energy_cpi <- panel_summary |> filter(period == "Energy-inflation and tightening episode", variable == "d_CPI", scenario == "CF1_no_energy")
  no_energy_rate <- panel_summary |> filter(period == "Energy-inflation and tightening episode", variable == "d_3MRate", scenario == "CF1_no_energy")
  no_ciss_gdp <- panel_summary |> filter(period == "Energy-inflation and tightening episode", variable == "GDP_Growth", scenario == "CF2_no_ciss")
  fiscal_other <- hd_panel_periods |>
    filter(period == "Full sample", variable == "d_FiscalBalanceGDP") |>
    pull(share_abs_other)
  if (length(fiscal_other) == 0L) fiscal_other <- NA_real_
  no_energy_cds <- panel_summary |> filter(period == "Energy-inflation and tightening episode", variable == "dlog_CDS", scenario == "CF1_no_energy")
  no_sov_cds <- panel_summary |> filter(period == "Energy-inflation and tightening episode", variable == "dlog_CDS", scenario == "CF4_no_sovereign")
  cf6_cds <- panel_summary |> filter(period == "Energy-inflation and tightening episode", variable == "dlog_CDS", scenario == "CF6_no_energy_no_sovereign")
  no_infl_cds <- panel_summary |> filter(period == "Energy-inflation and tightening episode", variable == "dlog_CDS", scenario == "CF3_no_inflationary_monetary")

  data.frame(
    question = c(
      "Model used",
      "Same HD and representative matrix",
      "Scenarios run",
      "Largest cumulative dlog_CDS effect, full sample",
      "Largest cumulative dlog_CDS effect, 2021Q1-2023Q4",
      "No-energy effect on d_CPI, 2021Q1-2023Q4",
      "No-energy effect on d_3MRate, 2021Q1-2023Q4",
      "No-CISS effect on GDP_Growth, 2021Q1-2023Q4",
      "Fiscal interpretability",
      "Top CDS countries, no-energy",
      "Top CDS countries, no-sovereign",
      "Top CDS countries, no-energy-no-sovereign",
      "No-energy supports energy-carbon to sovereign-risk narrative",
      "No-sovereign shows own CDS repricing component",
      "Combined scenarios interpretable",
      "Results that complicate narrative",
      "Best figures for paper",
      "Appendix results",
      "Counterfactual stability for paper",
      "Writing recommendation"
    ),
    answer = c(
      MODEL_NAME,
      "Yes. It uses the validated HD and representative draw candidate 23085 / accepted 5782.",
      paste(SCENARIOS$scenario, collapse = "; "),
      full_cds,
      episode_cds,
      paste0("cumulative gap = ", round(no_energy_cpi$cumulative_gap[[1]], 4), "; mean gap = ", round(no_energy_cpi$mean_gap[[1]], 4), "; flag = ", no_energy_cpi$interpretation_flag[[1]]),
      paste0("cumulative gap = ", round(no_energy_rate$cumulative_gap[[1]], 4), "; mean gap = ", round(no_energy_rate$mean_gap[[1]], 4), "; flag = ", no_energy_rate$interpretation_flag[[1]]),
      paste0("cumulative gap = ", round(no_ciss_gdp$cumulative_gap[[1]], 4), "; mean gap = ", round(no_ciss_gdp$mean_gap[[1]], 4), "; flag = ", no_ciss_gdp$interpretation_flag[[1]]),
      paste0("Full-sample HD Other share for fiscal = ", ifelse(is.na(fiscal_other), "NA", scales::percent(fiscal_other, accuracy = 0.1)), "; interpret fiscal cautiously if Other is high."),
      top_countries(country_rankings, "no_energy_dlog_CDS"),
      top_countries(country_rankings, "no_sovereign_dlog_CDS"),
      top_countries(country_rankings, "no_energy_no_sovereign_dlog_CDS"),
      ifelse(no_energy_cds$cumulative_gap[[1]] > 0, "Yes, model-implied cumulative dlog_CDS gap is positive in the energy episode.", "Weak or mixed: cumulative dlog_CDS gap is not positive in the energy episode."),
      ifelse(abs(no_sov_cds$cumulative_gap[[1]]) >= 0.05, "Yes, own CDS repricing component is economically visible.", "Small in this period; report cautiously."),
      ifelse(abs(cf6_cds$cumulative_gap[[1]]) >= max(abs(no_energy_cds$cumulative_gap[[1]]), abs(no_sov_cds$cumulative_gap[[1]])), "Yes, but interpret as additive model-implied removal of two labelled components.", "Interpretable, but not stronger than the single-shock scenarios."),
      paste0("No-inflationary dlog_CDS gap in energy episode = ", round(no_infl_cds$cumulative_gap[[1]], 4), "; fiscal and some rate paths may be influenced by Other."),
      "paper_cf_dlog_CDS_no_energy; paper_cf_dlog_CDS_no_sovereign; paper_cf_dlog_CDS_no_energy_no_sovereign; paper_cf_dlog_CDS_scenario_comparison; d_CPI and d_3MRate no-energy figures.",
      "Full country-level paths, CF7 no macro-financial, non-focus variables, and fiscal detail tables.",
      "Yes, conditional on the validated HD reconstruction and representative-draw approximation.",
      "Use CF1, CF4 and CF6 in the main text; keep CF2/CF3/CF5/CF7 and detailed country paths mostly in appendix."
    ),
    stringsAsFactors = FALSE
  )
}

plot_cf_path <- function(panel_cf_long, variable_name, scenario_name, file_path, title = NULL) {
  dat <- panel_cf_long |>
    filter(variable == variable_name, scenario == scenario_name) |>
    arrange(yq_index)
  if (nrow(dat) == 0L) return(invisible(NULL))
  breaks <- dat |> distinct(yq_index, Quarter_ID) |> filter(row_number() %% 4 == 1)
  p <- ggplot(dat, aes(x = yq_index)) +
    annotate("rect", xmin = q_index("2021Q1") - 0.45, xmax = q_index("2023Q4") + 0.45, ymin = -Inf, ymax = Inf, fill = "#d9e6ef", alpha = 0.35) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.55) +
    geom_line(aes(y = fitted, color = "Fitted / reconstructed"), linewidth = 0.55, linetype = "dashed") +
    geom_line(aes(y = counterfactual, color = "Counterfactual"), linewidth = 0.75) +
    scale_x_continuous(breaks = breaks$yq_index, labels = breaks$Quarter_ID) +
    scale_color_manual(values = c("Actual" = "black", "Fitted / reconstructed" = "#777777", "Counterfactual" = "#b23a2e")) +
    labs(
      title = ifelse(is.null(title), paste(variable_name, scenario_name), title),
      subtitle = "Model-implied counterfactual path; shaded area marks 2021Q1-2023Q4.",
      x = NULL,
      y = NULL,
      color = NULL
    ) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
  ggsave(file_path, p, width = 10.5, height = 5.6, dpi = 170)
}

plot_cumulative_gap <- function(panel_cum, variable_name, file_path, scenarios = SCENARIOS$scenario, title = NULL) {
  dat <- panel_cum |>
    filter(variable == variable_name, scenario %in% scenarios) |>
    arrange(scenario, yq_index)
  if (nrow(dat) == 0L) return(invisible(NULL))
  breaks <- dat |> distinct(yq_index, Quarter_ID) |> arrange(yq_index) |> filter(row_number() %% 4 == 1)
  p <- ggplot(dat, aes(x = yq_index, y = cumulative_gap, color = scenario, group = scenario)) +
    annotate("rect", xmin = q_index("2021Q1") - 0.45, xmax = q_index("2023Q4") + 0.45, ymin = -Inf, ymax = Inf, fill = "#d9e6ef", alpha = 0.30) +
    geom_hline(yintercept = 0, color = "grey45", linewidth = 0.25) +
    geom_line(linewidth = 0.7) +
    scale_x_continuous(breaks = breaks$yq_index, labels = breaks$Quarter_ID) +
    labs(
      title = ifelse(is.null(title), paste("Cumulative counterfactual gap:", variable_name), title),
      subtitle = "Gap = fitted minus counterfactual. Shaded area marks 2021Q1-2023Q4.",
      x = NULL,
      y = "Cumulative gap",
      color = "Scenario"
    ) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
  ggsave(file_path, p, width = 10.8, height = 5.8, dpi = 170)
}

plot_country_ranking <- function(country_rankings, ranking_type, file_path, title) {
  dat <- country_rankings |>
    filter(ranking_type == !!ranking_type) |>
    arrange(cumulative_gap)
  if (nrow(dat) == 0L) return(invisible(NULL))
  p <- ggplot(dat, aes(x = reorder(Country, cumulative_gap), y = cumulative_gap, fill = cumulative_gap > 0)) +
    geom_col(width = 0.72) +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "#2f6f88", "FALSE" = "#b45b4b"), guide = "none") +
    labs(title = title, subtitle = "Energy-inflation and tightening episode, 2021Q1-2023Q4.", x = NULL, y = "Cumulative dlog_CDS gap") +
    theme_minimal(base_size = 10)
  ggsave(file_path, p, width = 8.6, height = 5.6, dpi = 180)
}

plot_period_summary_figures <- function(panel_summary) {
  cds <- panel_summary |>
    filter(variable == "dlog_CDS") |>
    mutate(period = factor(period, levels = PERIODS$period))
  p_cds <- ggplot(cds, aes(x = scenario, y = cumulative_gap, fill = scenario)) +
    geom_col(width = 0.72) +
    geom_hline(yintercept = 0, color = "grey40", linewidth = 0.25) +
    facet_wrap(~period, scales = "free_y") +
    labs(title = "dlog_CDS cumulative counterfactual gap by period", x = NULL, y = "Cumulative gap", fill = "Scenario") +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "none")
  ggsave(file.path(FIGURE_DIR, "cf_dlog_CDS_focus", "cf_dlog_CDS_period_scenario_gap.png"), p_cds, width = 12, height = 7, dpi = 170)
  ggsave(file.path(FIGURE_DIR, "cf_period_summary", "cf_dlog_CDS_period_scenario_gap.png"), p_cds, width = 12, height = 7, dpi = 170)

  focus <- panel_summary |>
    filter(variable %in% FOCUS_VARS, period == "Energy-inflation and tightening episode") |>
    mutate(variable = factor(variable, levels = FOCUS_VARS))
  p_focus <- ggplot(focus, aes(x = scenario, y = cumulative_gap, fill = scenario)) +
    geom_col(width = 0.72) +
    geom_hline(yintercept = 0, color = "grey40", linewidth = 0.25) +
    facet_wrap(~variable, scales = "free_y") +
    labs(title = "Energy-inflation episode cumulative counterfactual gaps", x = NULL, y = "Cumulative gap", fill = "Scenario") +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "none")
  ggsave(file.path(FIGURE_DIR, "cf_period_summary", "cf_focus_variables_energy_episode_gaps.png"), p_focus, width = 12, height = 7, dpi = 170)
}

create_figures <- function(panel_cf_long, panel_cum, country_cf_long, country_rankings) {
  main_scenarios <- c("CF1_no_energy", "CF4_no_sovereign", "CF6_no_energy_no_sovereign")
  scenario_titles <- c(
    CF1_no_energy = "No Energy-carbon pressure shocks",
    CF4_no_sovereign = "No Sovereign-risk repricing shocks",
    CF6_no_energy_no_sovereign = "No Energy-carbon and no Sovereign-risk repricing shocks"
  )
  for (v in MODEL_VARS) {
    for (sc in main_scenarios) {
      plot_cf_path(
        panel_cf_long,
        v,
        sc,
        file.path(FIGURE_DIR, "cf_panel_average", paste0("cf_panel_", safe_name(v), "_", safe_name(sc), ".png")),
        paste(v, "-", scenario_titles[[sc]])
      )
    }
    plot_cumulative_gap(
      panel_cum,
      v,
      file.path(FIGURE_DIR, "cf_cumulative", paste0("cf_cumulative_gap_", safe_name(v), ".png")),
      title = paste("Cumulative counterfactual gap:", v)
    )
  }

  for (country in unique(country_cf_long$Country)) {
    dat <- country_cf_long |> filter(Country == !!country, variable == "dlog_CDS", scenario %in% main_scenarios)
    if (nrow(dat) == 0L) next
    breaks <- dat |> distinct(yq_index, Quarter_ID) |> arrange(yq_index) |> filter(row_number() %% 4 == 1)
    p <- ggplot(dat, aes(x = yq_index, y = counterfactual, color = scenario, group = scenario)) +
      annotate("rect", xmin = q_index("2021Q1") - 0.45, xmax = q_index("2023Q4") + 0.45, ymin = -Inf, ymax = Inf, fill = "#d9e6ef", alpha = 0.28) +
      geom_line(aes(y = fitted_reconstructed, color = "Fitted / reconstructed"), linewidth = 0.5, linetype = "dashed") +
      geom_line(linewidth = 0.65) +
      scale_x_continuous(breaks = breaks$yq_index, labels = breaks$Quarter_ID) +
      labs(title = paste(country, "dlog_CDS counterfactual paths"), x = NULL, y = NULL, color = NULL) +
      theme_minimal(base_size = 9) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    ggsave(file.path(FIGURE_DIR, "cf_country_level", paste0("cf_country_dlog_CDS_", safe_name(country), ".png")), p, width = 10.3, height = 5.5, dpi = 160)
  }

  paper_map <- list(
    list(v = "dlog_CDS", sc = "CF1_no_energy", file = "paper_cf_dlog_CDS_no_energy.png", title = "dlog_CDS: no Energy-carbon pressure shocks"),
    list(v = "dlog_CDS", sc = "CF4_no_sovereign", file = "paper_cf_dlog_CDS_no_sovereign.png", title = "dlog_CDS: no Sovereign-risk repricing shocks"),
    list(v = "dlog_CDS", sc = "CF6_no_energy_no_sovereign", file = "paper_cf_dlog_CDS_no_energy_no_sovereign.png", title = "dlog_CDS: no Energy-carbon and no Sovereign-risk shocks"),
    list(v = "d_CPI", sc = "CF1_no_energy", file = "paper_cf_d_CPI_no_energy.png", title = "d_CPI: no Energy-carbon pressure shocks"),
    list(v = "d_3MRate", sc = "CF1_no_energy", file = "paper_cf_d_3MRate_no_energy.png", title = "d_3MRate: no Energy-carbon pressure shocks"),
    list(v = "GDP_Growth", sc = "CF2_no_ciss", file = "paper_cf_GDP_Growth_no_ciss.png", title = "GDP_Growth: no Systemic financial stress shocks")
  )
  for (item in paper_map) {
    plot_cf_path(panel_cf_long, item$v, item$sc, file.path(FIGURE_DIR, "paper_figures", item$file), item$title)
  }
  plot_cumulative_gap(
    panel_cum,
    "dlog_CDS",
    file.path(FIGURE_DIR, "paper_figures", "paper_cf_dlog_CDS_scenario_comparison.png"),
    title = "dlog_CDS cumulative counterfactual gap by scenario"
  )
  plot_cumulative_gap(
    panel_cum,
    "dlog_CDS",
    file.path(FIGURE_DIR, "cf_dlog_CDS_focus", "cf_dlog_CDS_scenario_comparison.png"),
    title = "dlog_CDS cumulative counterfactual gap by scenario"
  )
  plot_country_ranking(
    country_rankings,
    "no_energy_dlog_CDS",
    file.path(FIGURE_DIR, "paper_figures", "paper_cf_country_ranking_no_energy_dlog_CDS.png"),
    "Country ranking: no-energy dlog_CDS gap"
  )
  plot_country_ranking(
    country_rankings,
    "no_sovereign_dlog_CDS",
    file.path(FIGURE_DIR, "paper_figures", "paper_cf_country_ranking_no_sovereign_dlog_CDS.png"),
    "Country ranking: no-sovereign dlog_CDS gap"
  )
  plot_country_ranking(
    country_rankings,
    "no_energy_no_sovereign_dlog_CDS",
    file.path(FIGURE_DIR, "paper_figures", "paper_cf_country_ranking_no_energy_no_sovereign_dlog_CDS.png"),
    "Country ranking: no-energy-no-sovereign dlog_CDS gap"
  )
}

write_reports <- function(inputs, period_summary, country_rankings, scenario_ranking, main_findings) {
  setup <- inputs$setup
  hd_max <- setup$hd_reconstruction$max_abs_error[setup$hd_reconstruction$level == "overall"][[1]]
  residual_max <- setup$residual_reconstruction$value[setup$residual_reconstruction$check == "max_abs_error"][[1]]
  rep_draw <- setup$representative_draw

  full_cds <- period_summary$panel_summary |> filter(period == "Full sample", variable == "dlog_CDS") |> arrange(desc(abs(cumulative_gap))) |> slice(1)
  episode_cds <- period_summary$panel_summary |> filter(period == "Energy-inflation and tightening episode", variable == "dlog_CDS") |> arrange(desc(abs(cumulative_gap))) |> slice(1)
  episode_cpi_no_energy <- period_summary$panel_summary |> filter(period == "Energy-inflation and tightening episode", variable == "d_CPI", scenario == "CF1_no_energy")
  episode_rate_no_energy <- period_summary$panel_summary |> filter(period == "Energy-inflation and tightening episode", variable == "d_3MRate", scenario == "CF1_no_energy")

  lines <- c(
    "# Counterfactual Analysis Report - refined4 S1",
    "",
    "## Model And Inputs",
    paste0("Model used: ", MODEL_NAME, "."),
    paste0("Input HD folder: `", HD_DIR, "`. Structural folder: `", STRUCTURAL_DIR, "`. Reduced-form folder: `", INPUT_DIR, "`."),
    "This stage uses the historical decomposition output only. It does not rerun structural identification and does not use repaired4.",
    paste0("Representative draw: candidate draw ", rep_draw$candidate_draw[[1]], ", accepted draw ", rep_draw$accepted_draw[[1]], "."),
    "",
    "## Methodology",
    "For each scenario, counterfactual = fitted reconstructed path minus the historical-decomposition contribution of the removed shock(s). Other/unidentified shocks and the initial/deterministic component are kept.",
    "Paths are computed on the full available HD sample, 2014Q3-2025Q4, then summarized by subperiod.",
    "",
    "## Reconstruction Checks",
    paste0("Residual reconstruction max error from HD: ", format(residual_max, scientific = TRUE, digits = 4), "."),
    paste0("Historical-decomposition max error from HD: ", format(hd_max, scientific = TRUE, digits = 4), "."),
    "",
    "## Scenarios",
    paste(capture.output(print(SCENARIOS[, c("scenario", "scenario_label", "paper_priority")])), collapse = "\n"),
    "",
    "## Main Findings",
    paste(capture.output(print(main_findings)), collapse = "\n"),
    "",
    "## Panel Average Results",
    paste0("Largest full-sample dlog_CDS effect: ", full_cds$scenario[[1]], ", cumulative gap ", round(full_cds$cumulative_gap[[1]], 4), ", percent effect ", scales::percent(full_cds$cumulative_percent_effect_dlog_CDS[[1]], accuracy = 0.1), "."),
    paste0("Largest 2021Q1-2023Q4 dlog_CDS effect: ", episode_cds$scenario[[1]], ", cumulative gap ", round(episode_cds$cumulative_gap[[1]], 4), ", percent effect ", scales::percent(episode_cds$cumulative_percent_effect_dlog_CDS[[1]], accuracy = 0.1), "."),
    paste0("No-energy d_CPI cumulative gap in 2021Q1-2023Q4: ", round(episode_cpi_no_energy$cumulative_gap[[1]], 4), "."),
    paste0("No-energy d_3MRate cumulative gap in 2021Q1-2023Q4: ", round(episode_rate_no_energy$cumulative_gap[[1]], 4), "."),
    "",
    "## Limitations",
    "These are model-implied counterfactual paths, not observed alternative histories.",
    "The sovereign-risk repricing shock is an own CDS repricing component not labelled by the energy, stress or inflation-rate shocks; it is not an observed external shock.",
    "Fiscal results should be interpreted cautiously when Other/unidentified remains important.",
    "",
    "## Final Answers",
    paste0("1. Model structural folosit: ", MODEL_NAME, "."),
    "2. Counterfactual-ul foloseste aceeasi HD si aceeasi matrice structurala reprezentativa? TRUE.",
    paste0("3. Scenarii rulate: ", paste(SCENARIOS$scenario, collapse = "; "), "."),
    paste0("4. Pentru dlog_CDS, cel mai mare efect cumulativ full sample: ", full_cds$scenario[[1]], "."),
    paste0("5. Pentru dlog_CDS, cel mai mare efect cumulativ in 2021Q1-2023Q4: ", episode_cds$scenario[[1]], "."),
    paste0("6. Pentru d_CPI, no-energy in 2021Q1-2023Q4: cumulative gap ", round(episode_cpi_no_energy$cumulative_gap[[1]], 4), "."),
    paste0("7. Pentru d_3MRate, no-energy in 2021Q1-2023Q4: cumulative gap ", round(episode_rate_no_energy$cumulative_gap[[1]], 4), "."),
    paste0("8. Pentru GDP_Growth, no-CISS: ", main_findings$answer[main_findings$question == "No-CISS effect on GDP_Growth, 2021Q1-2023Q4"], "."),
    paste0("9. Pentru fiscal: ", main_findings$answer[main_findings$question == "Fiscal interpretability"], "."),
    paste0("10. Tari cu cel mai mare gap CDS no-energy: ", main_findings$answer[main_findings$question == "Top CDS countries, no-energy"], "."),
    paste0("11. Tari cu cel mai mare gap CDS no-sovereign: ", main_findings$answer[main_findings$question == "Top CDS countries, no-sovereign"], "."),
    paste0("12. Tari cu cel mai mare gap CDS no-energy-no-sovereign: ", main_findings$answer[main_findings$question == "Top CDS countries, no-energy-no-sovereign"], "."),
    paste0("13. No-energy sustine narativul energy-carbon -> sovereign risk? ", main_findings$answer[main_findings$question == "No-energy supports energy-carbon to sovereign-risk narrative"], "."),
    paste0("14. No-sovereign arata componenta proprie de CDS repricing? ", main_findings$answer[main_findings$question == "No-sovereign shows own CDS repricing component"], "."),
    paste0("15. Scenariile combinate sunt interpretabile? ", main_findings$answer[main_findings$question == "Combined scenarios interpretable"], "."),
    paste0("16. Rezultate care contrazic/complica narativul: ", main_findings$answer[main_findings$question == "Results that complicate narrative"], "."),
    paste0("17. Cele mai bune figuri pentru paper: ", main_findings$answer[main_findings$question == "Best figures for paper"], "."),
    paste0("18. Rezultate pentru appendix: ", main_findings$answer[main_findings$question == "Appendix results"], "."),
    paste0("19. Counterfactual analysis este suficient de stabila pentru paper? ", main_findings$answer[main_findings$question == "Counterfactual stability for paper"], "."),
    paste0("20. Recomandare finala: ", main_findings$answer[main_findings$question == "Writing recommendation"], ".")
  )
  writeLines(lines, file.path(OUTPUT_DIR, "summary_report_counterfactual.md"))

  paper_lines <- c(
    "# Counterfactual Interpretation For Paper",
    "",
    "## 1. Counterfactual Design",
    "The counterfactual exercises use the refined4 S1 historical decomposition and remove selected labelled structural contributions while retaining Other/unidentified shocks and the initial/deterministic component.",
    "",
    "## 2. Aggregate CEE Counterfactual Results",
    paste0("The largest model-implied dlog_CDS counterfactual effect in the full sample is ", full_cds$scenario[[1]], ". During 2021Q1-2023Q4 it is ", episode_cds$scenario[[1]], "."),
    "",
    "## 3. No-Energy Shock Scenario",
    paste0("In the energy-inflation episode, the no-energy scenario implies a dlog_CDS cumulative gap of ", round((period_summary$panel_summary |> filter(period == "Energy-inflation and tightening episode", variable == "dlog_CDS", scenario == "CF1_no_energy"))$cumulative_gap[[1]], 4), ". This is a model-implied contribution of energy-carbon pressure, not a literal observed alternative history."),
    "",
    "## 4. No-Systemic Financial Stress Scenario",
    "The no-CISS scenario is most relevant for GDP_Growth and dlog_CDS. Interpret it as removing the labelled systemic financial stress contribution only.",
    "",
    "## 5. No-Inflationary Monetary-Reaction Scenario",
    "The no-inflationary monetary-reaction scenario may have smaller or mixed effects for CDS because its contribution is identified jointly through inflation and short-rate responses in the refined4 decomposition.",
    "",
    "## 6. No-Sovereign-Risk Repricing Scenario",
    "The no-sovereign scenario removes the own CDS repricing component. It should not be read as removing an observed external shock.",
    "",
    "## 7. Combined No-Energy-No-Sovereign Scenario",
    "The combined scenario is useful for CDS interpretation because it removes both energy-carbon pressure and the own sovereign-risk repricing component.",
    "",
    "## 8. Cross-Country Heterogeneity",
    paste0("Largest no-energy dlog_CDS gaps in 2021Q1-2023Q4: ", top_countries(country_rankings, "no_energy_dlog_CDS"), "."),
    paste0("Largest no-sovereign dlog_CDS gaps in 2021Q1-2023Q4: ", top_countries(country_rankings, "no_sovereign_dlog_CDS"), "."),
    "",
    "## 9. Implications For Sovereign-Risk Repricing In CEE",
    "The counterfactuals support a cautious energy-inflation-sovereign-risk narrative where labelled energy and sovereign-risk components matter for CDS, while Other/unidentified variation remains part of the decomposition.",
    "",
    "## 10. Limitations",
    "These are deterministic, representative-draw counterfactuals conditional on the refined4 identification. They do not provide uncertainty bands across all accepted rotations."
  )
  writeLines(paper_lines, file.path(OUTPUT_DIR, "counterfactual_interpretation_for_paper.md"))
}

main <- function() {
  cat("Reading validated HD inputs...\n")
  inputs <- load_inputs()
  country_hd <- inputs$country_hd
  panel_hd <- make_panel_hd(country_hd)

  cat("Constructing counterfactual paths...\n")
  panel_cf_long <- make_counterfactual_long(panel_hd, "panel_average")
  country_cf_long <- make_counterfactual_long(country_hd, "country")
  panel_cf_wide <- make_panel_wide(panel_cf_long)

  cat("Computing cumulative effects and period summaries...\n")
  panel_cumulative <- make_cumulative(panel_cf_long, character())
  panel_cumulative <- panel_cumulative |> mutate(Country = "Panel average", .before = 1)
  country_cumulative <- make_cumulative(country_cf_long, "Country")
  dlog_CDS_percent_effects <- bind_rows(
    panel_cumulative |> filter(variable == "dlog_CDS"),
    country_cumulative |> filter(variable == "dlog_CDS")
  ) |>
    select(Country, quarter_index, Date, Year, Quarter, Quarter_ID, variable, scenario, scenario_label, cumulative_fitted, cumulative_counterfactual, cumulative_gap, dlog_CDS_percent_effect)

  period_summary <- make_period_summary(panel_cf_long, country_cf_long)
  country_rankings <- make_country_rankings(period_summary$country_summary)
  scenario_ranking <- make_scenario_ranking_dlog_CDS(period_summary$panel_summary)
  main_findings <- make_main_findings(period_summary$panel_summary, period_summary$country_summary, country_rankings, inputs$panel_periods_hd)

  cat("Creating figures...\n")
  create_figures(panel_cf_long, panel_cumulative, country_cf_long, country_rankings)
  plot_period_summary_figures(period_summary$panel_summary)

  cat("Writing Excel outputs...\n")
  formula_notes <- data.frame(
    item = c(
      "formula",
      "Other shocks",
      "Initial deterministic component",
      "full_sample_paths",
      "dlog_CDS_percent_effect"
    ),
    value = c(
      "counterfactual = fitted_reconstructed - sum(removed labelled shock contributions)",
      "Always retained.",
      "Always retained.",
      "All paths are built on 2014Q3-2025Q4 before subperiod summaries.",
      "exp(cumulative_gap)-1"
    ),
    stringsAsFactors = FALSE
  )

  write_workbook(
    list(
      model_used = data.frame(item = c("model", "model_variant", "HD_input", "structural_input", "reduced_form_input"), value = c(MODEL_NAME, MODEL_VARIANT, HD_DIR, STRUCTURAL_DIR, INPUT_DIR)),
      representative_draw = inputs$setup$representative_draw,
      structural_B_matrix = inputs$setup$structural_B_matrix,
      variable_order = inputs$setup$variable_order,
      shock_names = data.frame(shock = c("Energy-carbon pressure shock", "Systemic financial stress shock", "Inflationary monetary-reaction shock", "Sovereign-risk repricing shock", "Other / unidentified structural shocks", "Initial / deterministic component")),
      scenarios = SCENARIOS,
      periods = PERIODS,
      hd_residual_reconstruction = inputs$setup$residual_reconstruction,
      hd_reconstruction = inputs$setup$hd_reconstruction,
      counterfactual_formula = formula_notes,
      interpretation_notes = formula_notes
    ),
    file.path(OUTPUT_DIR, "01_cf_model_setup.xlsx")
  )

  panel_sheets <- setNames(lapply(MODEL_VARS, function(v) panel_cf_wide |> filter(variable == v)), MODEL_VARS)
  panel_sheets$all_variables_long <- panel_cf_long
  write_workbook(panel_sheets, file.path(OUTPUT_DIR, "02_cf_panel_average_paths.xlsx"))

  write_workbook(
    list(country_level_long = country_cumulative),
    file.path(OUTPUT_DIR, "03_cf_country_level_paths.xlsx")
  )

  scenario_comp <- panel_cumulative |>
    group_by(variable, scenario, scenario_label) |>
    filter(yq_index == max(yq_index)) |>
    ungroup() |>
    arrange(variable, desc(abs(cumulative_gap)))
  write_workbook(
    list(
      panel_average_cumulative = panel_cumulative,
      country_level_cumulative = country_cumulative,
      dlog_CDS_percent_effects = dlog_CDS_percent_effects,
      scenario_comparison_cumulative = scenario_comp,
      country_rankings = country_rankings
    ),
    file.path(OUTPUT_DIR, "04_cf_cumulative_effects.xlsx")
  )

  period_sheets <- list(
    full_sample = period_summary$panel_summary |> filter(period_sheet == "full_sample"),
    pre_energy_inflation = period_summary$panel_summary |> filter(period_sheet == "pre_energy_inflation"),
    energy_inflation_tightening = period_summary$panel_summary |> filter(period_sheet == "energy_inflation_tightening"),
    post_shock_normalization = period_summary$panel_summary |> filter(period_sheet == "post_shock_normalization"),
    covid_rebound_secondary = period_summary$panel_summary |> filter(period_sheet == "covid_rebound_secondary"),
    all_periods_long = period_summary$panel_summary,
    country_periods_long = period_summary$country_summary
  )
  write_workbook(period_sheets, file.path(OUTPUT_DIR, "05_cf_summary_by_period.xlsx"))

  sheet_name_mapping <- data.frame(
    requested = c(
      "country_ranking_no_energy_dlog_CDS",
      "country_ranking_no_sovereign_dlog_CDS",
      "country_ranking_no_energy_no_sovereign_dlog_CDS"
    ),
    actual_sheet = c(
      "rank_no_energy_CDS",
      "rank_no_sovereign_CDS",
      "rank_no_energy_no_sov_CDS"
    ),
    note = "Excel sheet names are limited to 31 characters.",
    stringsAsFactors = FALSE
  )
  write_workbook(
    list(
      sheet_name_mapping = sheet_name_mapping,
      main_cf_dlog_CDS_panel = period_summary$panel_summary |> filter(variable == "dlog_CDS"),
      main_cf_d_CPI_panel = period_summary$panel_summary |> filter(variable == "d_CPI"),
      main_cf_d_3MRate_panel = period_summary$panel_summary |> filter(variable == "d_3MRate"),
      main_cf_GDP_Growth_panel = period_summary$panel_summary |> filter(variable == "GDP_Growth"),
      main_cf_fiscal_panel = period_summary$panel_summary |> filter(variable == "d_FiscalBalanceGDP"),
      energy_episode_dlog_CDS = period_summary$panel_summary |> filter(variable == "dlog_CDS", period_sheet == "energy_inflation_tightening"),
      energy_episode_d_CPI = period_summary$panel_summary |> filter(variable == "d_CPI", period_sheet == "energy_inflation_tightening"),
      energy_episode_d_3MRate = period_summary$panel_summary |> filter(variable == "d_3MRate", period_sheet == "energy_inflation_tightening"),
      rank_no_energy_CDS = country_rankings |> filter(ranking_type == "no_energy_dlog_CDS"),
      rank_no_sovereign_CDS = country_rankings |> filter(ranking_type == "no_sovereign_dlog_CDS"),
      rank_no_energy_no_sov_CDS = country_rankings |> filter(ranking_type == "no_energy_no_sovereign_dlog_CDS"),
      scenario_ranking_dlog_CDS = scenario_ranking,
      main_findings = main_findings
    ),
    file.path(OUTPUT_DIR, "06_cf_tables_for_paper.xlsx")
  )

  cat("Writing Markdown reports...\n")
  write_reports(inputs, period_summary, country_rankings, scenario_ranking, main_findings)

  cat("Counterfactual workflow complete.\n")
  cat("Output directory:", normalizePath(OUTPUT_DIR, winslash = "/"), "\n")
  print(main_findings)
}

main()
