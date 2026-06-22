# Q1 paper-ready visual and table polishing.
# This script does not rerun or modify any econometric model. It only reads
# final consolidated outputs and creates polished manuscript figures/tables.

required_packages <- c("openxlsx", "dplyr", "tidyr", "tibble", "ggplot2", "patchwork", "scales", "ragg")
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
  library(patchwork)
  library(scales)
})

ROOT_DIR <- getwd()
FINAL_DIR <- "outputs"
DATA_DIR <- file.path(FINAL_DIR, "01_model_ready_data")
MASTER_DIR <- file.path(FINAL_DIR, "02_tables", "main_paper")
APPENDIX_TABLE_DIR <- file.path(FINAL_DIR, "02_tables", "appendix")
TABLE_MANIFEST_DIR <- file.path(FINAL_DIR, "02_tables", "manifests")
FIGURE_DIR <- file.path(FINAL_DIR, "03_figures")
REPORT_DIR <- file.path(FINAL_DIR, "04_reports")

MAIN_POLISHED_DIR <- file.path(FIGURE_DIR, "main_paper")
APPENDIX_POLISHED_DIR <- file.path(FIGURE_DIR, "appendix")
dir.create(MAIN_POLISHED_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(APPENDIX_POLISHED_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_MANIFEST_DIR, recursive = TRUE, showWarnings = FALSE)

MASTER_WB <- file.path(MASTER_DIR, "MASTER_all_tables_for_paper.xlsx")
APPENDIX_WB <- file.path(APPENDIX_TABLE_DIR, "MASTER_appendix_tables.xlsx")
MODEL_READY_WB <- file.path(DATA_DIR, "model_ready_dataset.xlsx")

read_sheet <- function(path, sheet) {
  if (!file.exists(path)) stop("Missing file: ", path)
  sheets <- openxlsx::getSheetNames(path)
  if (!(sheet %in% sheets)) stop("Missing sheet `", sheet, "` in ", path)
  openxlsx::read.xlsx(path, sheet = sheet)
}

quarter_date <- function(x) {
  year <- as.integer(substr(x, 1, 4))
  quarter <- as.integer(sub(".*Q", "", x))
  month <- (quarter - 1L) * 3L + 1L
  as.Date(sprintf("%04d-%02d-01", year, month))
}

theme_q1 <- function(base_size = 10.5) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 1.5, color = "#222222"),
      plot.subtitle = element_text(size = base_size, color = "#555555"),
      plot.caption = element_text(size = base_size - 1.5, color = "#666666", hjust = 0),
      axis.title = element_text(size = base_size, color = "#333333"),
      axis.text = element_text(size = base_size - 1, color = "#333333"),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = base_size - 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.25),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(face = "bold", color = "#333333"),
      strip.background = element_rect(fill = "#F3F3F3", color = NA)
    )
}

shock_palette <- c(
  "Energy" = "#B65A4A",
  "CISS" = "#8A7A38",
  "Inflation/Rate" = "#4E7D5B",
  "Sovereign" = "#3F7C8A",
  "Other" = "#8A96A8",
  "Initial" = "#B8B8B8"
)

scenario_palette <- c(
  "Actual" = "#6E6E6E",
  "Fitted" = "#111111",
  "No Energy" = "#B65A4A",
  "No Sovereign" = "#3F7C8A",
  "No Energy + No Sovereign" = "#5C4A3D"
)

shock_levels <- names(shock_palette)
scenario_levels <- names(scenario_palette)

short_shock <- function(x) {
  dplyr::case_when(
    x == "Energy-carbon pressure shock" ~ "Energy",
    x == "Systemic financial stress shock" ~ "CISS",
    x == "Inflationary monetary-reaction shock" ~ "Inflation/Rate",
    x == "Sovereign-risk repricing shock" ~ "Sovereign",
    x == "Other / unidentified structural shocks" ~ "Other",
    x == "Initial / deterministic component" ~ "Initial",
    x == "contribution_energy" ~ "Energy",
    x == "contribution_ciss" ~ "CISS",
    x == "contribution_inflationary_monetary" ~ "Inflation/Rate",
    x == "contribution_sovereign" ~ "Sovereign",
    x == "contribution_other" ~ "Other",
    x == "initial_deterministic_component" ~ "Initial",
    TRUE ~ x
  )
}

short_scenario <- function(x) {
  dplyr::case_when(
    x == "CF1_no_energy" ~ "No Energy",
    x == "CF4_no_sovereign" ~ "No Sovereign",
    x == "CF6_no_energy_no_sovereign" ~ "No Energy + No Sovereign",
    TRUE ~ x
  )
}

var_label <- function(x) {
  dplyr::case_when(
    x == "d_CPI" ~ "CPI",
    x == "GDP_Growth" ~ "GDP growth",
    x == "d_3MRate" ~ "3M rate",
    x == "d_FiscalBalanceGDP" ~ "Fiscal balance",
    x == "dlog_CDS" ~ "CDS",
    x == "Energy_Factor" ~ "Energy factor",
    x == "d_CISS" ~ "CISS",
    TRUE ~ x
  )
}

save_q1_plot <- function(plot, path, width = 9, height = 5.5) {
  ragg::agg_png(paste0(path, ".png"), width = width, height = height, units = "in", res = 400, background = "white")
  print(plot)
  dev.off()
  tryCatch(
    ggplot2::ggsave(paste0(path, ".pdf"), plot, width = width, height = height, device = grDevices::cairo_pdf, bg = "white"),
    error = function(e) ggplot2::ggsave(paste0(path, ".pdf"), plot, width = width, height = height, device = "pdf", bg = "white")
  )
}

figure_manifest <- list()
add_figure_manifest <- function(figure_id, filename_base, title, manuscript_section,
                                main_text_or_appendix, variables, scenario_or_shock,
                                recommended_caption, notes, status = "recommended") {
  figure_manifest[[length(figure_manifest) + 1L]] <<- tibble(
    figure_id = figure_id,
    filename_png = paste0(normalizePath(filename_base, winslash = "/", mustWork = FALSE), ".png"),
    filename_pdf = paste0(normalizePath(filename_base, winslash = "/", mustWork = FALSE), ".pdf"),
    title = title,
    manuscript_section = manuscript_section,
    main_text_or_appendix = main_text_or_appendix,
    variables = variables,
    scenario_or_shock = scenario_or_shock,
    recommended_caption = recommended_caption,
    notes = notes,
    status = status
  )
}

shade_start <- as.Date("2021-01-01")
shade_end <- as.Date("2023-12-31")

# ---------------------------------------------------------------------------
# Data reads
# ---------------------------------------------------------------------------

model_ready <- read_sheet(MODEL_READY_WB, "model_ready_dataset")
pca_energy <- read_sheet(MASTER_WB, "T09_pca_energy_factor")
struct_irf <- read_sheet(APPENDIX_WB, "all_structural_irfs")
struct_fevd <- read_sheet(APPENDIX_WB, "all_structural_fevd")
cf_country <- read_sheet(APPENDIX_WB, "all_cf_country_level")

hd_panel <- bind_rows(
  read_sheet(MASTER_WB, "T32_hd_panel_average_dlog_CDS"),
  read_sheet(MASTER_WB, "T33_hd_panel_average_d_CPI"),
  read_sheet(MASTER_WB, "T34_hd_panel_average_d_3MRate")
)
hd_panel_appendix <- bind_rows(
  read_sheet(MASTER_WB, "T35_hd_panel_average_GDP"),
  read_sheet(MASTER_WB, "T36_hd_panel_average_fiscal")
)

rank_no_energy <- read_sheet(MASTER_WB, "T52_cf_country_rank_no_energy")
rank_no_sovereign <- read_sheet(MASTER_WB, "T53_cf_country_rank_no_sovereig")
rank_no_energy_no_sovereign <- read_sheet(MASTER_WB, "T54_cf_country_rank_no_energy_n")

# ---------------------------------------------------------------------------
# Figure 1 - Energy factor and PCA loadings
# ---------------------------------------------------------------------------

energy_series <- model_ready |>
  mutate(date_q = quarter_date(Quarter_ID)) |>
  group_by(Quarter_ID, date_q) |>
  summarise(Energy_Factor = mean(Energy_Factor, na.rm = TRUE), .groups = "drop")

pca_loadings <- pca_energy |>
  filter(table == "loadings") |>
  transmute(
    component = case_when(
      variable == "dlog_TTF" ~ "TTF",
      variable == "dlog_Brent" ~ "Brent",
      variable == "dlog_Energy_Price" ~ "Energy Price",
      variable == "dlog_Power_Energy_Price" ~ "Power/Energy Price",
      TRUE ~ variable
    ),
    loading = PC1
  ) |>
  arrange(loading) |>
  mutate(component = factor(component, levels = component))

p1a <- ggplot(energy_series, aes(date_q, Energy_Factor)) +
  annotate("rect", xmin = shade_start, xmax = shade_end, ymin = -Inf, ymax = Inf, fill = "#B65A4A", alpha = 0.08) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "#666666") +
  geom_line(color = "#B65A4A", linewidth = 0.75) +
  labs(title = "Panel A. Energy-carbon factor", x = NULL, y = "PCA factor") +
  theme_q1()

p1b <- ggplot(pca_loadings, aes(component, loading)) +
  geom_col(fill = "#B65A4A", width = 0.68) +
  geom_text(aes(label = sprintf("%.2f", loading)), hjust = -0.12, size = 3.2, color = "#333333") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.16))) +
  labs(title = "Panel B. PC1 loadings", x = NULL, y = "Loading") +
  theme_q1()

fig1 <- p1a / p1b +
  plot_annotation(
    title = "Energy-carbon pressure factor",
    subtitle = "Panel A reports the PCA-based energy-carbon pressure factor. Panel B reports PC1 loadings."
  )

fig1_base <- file.path(MAIN_POLISHED_DIR, "F01_energy_factor_pca_components_polished")
save_q1_plot(fig1, fig1_base, width = 9, height = 6.2)
add_figure_manifest(
  "F01", fig1_base, "Energy-carbon pressure factor", "Data and measurement", "main",
  "Energy_Factor; PCA loadings", "Energy", "Figure 1. Energy-carbon pressure factor. Panel A reports the PCA-based energy-carbon pressure factor used in the Structural PVAR. Panel B reports PC1 loadings for the four underlying energy-carbon price components.",
  "Replaces original F01; no econometric result changed."
)

# ---------------------------------------------------------------------------
# Figure 2 - Selected structural IRFs
# ---------------------------------------------------------------------------

irf_selection <- tribble(
  ~shock, ~response, ~panel,
  "Energy-carbon pressure shock", "d_CPI", "Energy -> CPI",
  "Energy-carbon pressure shock", "d_3MRate", "Energy -> 3M rate",
  "Energy-carbon pressure shock", "dlog_CDS", "Energy -> CDS",
  "Systemic financial stress shock", "GDP_Growth", "CISS -> GDP growth",
  "Systemic financial stress shock", "dlog_CDS", "CISS -> CDS",
  "Sovereign-risk repricing shock", "dlog_CDS", "Sovereign -> CDS"
)

irf_selected <- struct_irf |>
  inner_join(irf_selection, by = c("shock", "response")) |>
  mutate(panel = factor(panel, levels = irf_selection$panel))

stopifnot(n_distinct(irf_selected$panel) == 6)

fig2 <- ggplot(irf_selected, aes(horizon, median_irf)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "#555555") +
  geom_ribbon(aes(ymin = p2_5, ymax = p97_5), fill = "#8A96A8", alpha = 0.18) +
  geom_ribbon(aes(ymin = p16, ymax = p84), fill = "#3F7C8A", alpha = 0.26) +
  geom_line(color = "#111111", linewidth = 0.6) +
  facet_wrap(~panel, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +
  labs(
    title = "Selected structural impulse responses",
    subtitle = "Responses are based on the sign-restricted Structural PVAR refined4 S1.\nShaded bands report posterior/rotation uncertainty intervals, where available.",
    x = "Horizon",
    y = "Median response"
  ) +
  theme_q1()

fig2_base <- file.path(MAIN_POLISHED_DIR, "F02_selected_structural_irfs_polished")
save_q1_plot(fig2, fig2_base, width = 9, height = 5.5)
add_figure_manifest(
  "F02", fig2_base, "Selected structural impulse responses", "Structural transmission", "main",
  "d_CPI; d_3MRate; dlog_CDS; GDP_Growth", "Energy; CISS; Sovereign",
  "Figure 2. Selected structural impulse responses. The figure reports median responses and 68%/95% rotation-uncertainty bands for the main transmission channels in the Structural PVAR refined4 S1.",
  "Combines selected IRFs only; exhaustive IRFs remain in appendix/replication package."
)

# ---------------------------------------------------------------------------
# Figure 3 - Structural FEVD at h=12
# ---------------------------------------------------------------------------

main_fevd_vars <- c("d_CPI", "GDP_Growth", "d_3MRate", "d_FiscalBalanceGDP", "dlog_CDS")

fevd_h12_main <- struct_fevd |>
  filter(horizon == 12, response %in% main_fevd_vars) |>
  mutate(
    shock_short = factor(short_shock(shock), levels = shock_levels),
    response_label = factor(var_label(response), levels = var_label(main_fevd_vars))
  ) |>
  filter(shock_short %in% c("Energy", "CISS", "Inflation/Rate", "Sovereign", "Other")) |>
  group_by(response_label) |>
  mutate(share_pct = 100 * mean_share / sum(mean_share, na.rm = TRUE)) |>
  ungroup()

fig3 <- ggplot(fevd_h12_main, aes(response_label, share_pct, fill = shock_short)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.15) +
  coord_flip() +
  scale_fill_manual(values = shock_palette, breaks = shock_levels[1:5], drop = TRUE) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100), expand = expansion(mult = c(0, 0.01))) +
  labs(
    title = "Structural forecast-error variance decomposition at horizon 12",
    subtitle = "Shares of forecast-error variance explained by identified and unidentified structural components.",
    x = NULL,
    y = "Share of forecast-error variance"
  ) +
  theme_q1()

fig3_base <- file.path(MAIN_POLISHED_DIR, "F03_structural_fevd_h12_main_variables_polished")
save_q1_plot(fig3, fig3_base, width = 9, height = 5.5)
add_figure_manifest(
  "F03", fig3_base, "Structural forecast-error variance decomposition at horizon 12", "Variance decomposition", "main",
  paste(main_fevd_vars, collapse = "; "), "Energy; CISS; Inflation/Rate; Sovereign; Other",
  "Figure 3. Structural forecast-error variance decomposition at horizon 12. Bars report the share of forecast-error variance attributable to identified shocks and the residual unidentified component for the main macro-financial variables.",
  "Replaces the narrow dlog_CDS-only FEVD figure."
)

# ---------------------------------------------------------------------------
# Figure 4 - Historical decomposition, main variables
# ---------------------------------------------------------------------------

hd_long <- hd_panel |>
  mutate(date_q = quarter_date(Quarter_ID), variable_label = factor(var_label(variable), levels = var_label(c("dlog_CDS", "d_CPI", "d_3MRate")))) |>
  pivot_longer(
    cols = c(contribution_energy, contribution_ciss, contribution_inflationary_monetary,
             contribution_sovereign, contribution_other, initial_deterministic_component),
    names_to = "component",
    values_to = "contribution"
  ) |>
  mutate(component_short = factor(short_shock(component), levels = shock_levels))

fig4 <- ggplot(hd_long, aes(date_q)) +
  annotate("rect", xmin = shade_start, xmax = shade_end, ymin = -Inf, ymax = Inf, fill = "#B65A4A", alpha = 0.07) +
  geom_col(aes(y = contribution, fill = component_short), width = 70, alpha = 0.95) +
  geom_line(aes(y = actual_panel_average), color = "#111111", linewidth = 0.35) +
  geom_hline(yintercept = 0, color = "#555555", linewidth = 0.25) +
  facet_wrap(~variable_label, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = shock_palette, breaks = shock_levels, drop = FALSE) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Historical decomposition of key macro-financial variables",
    subtitle = "Panel-average contributions from identified structural shocks.\nShaded area denotes the 2021Q1-2023Q4 energy-inflation and tightening episode.",
    x = NULL,
    y = "Contribution / actual",
    caption = "Black line reports the panel-average actual series."
  ) +
  theme_q1()

fig4_base <- file.path(MAIN_POLISHED_DIR, "F04_historical_decomposition_main_variables_polished")
save_q1_plot(fig4, fig4_base, width = 9, height = 7.2)
add_figure_manifest(
  "F04", fig4_base, "Historical decomposition of key macro-financial variables", "Historical decomposition", "main",
  "dlog_CDS; d_CPI; d_3MRate", "Energy; CISS; Inflation/Rate; Sovereign; Other; Initial",
  "Figure 4. Historical decomposition of key macro-financial variables. The figure reports panel-average contributions from identified structural shocks, the unidentified component and the initial/deterministic component. The shaded area denotes 2021Q1-2023Q4.",
  "Combines separate HD figures for CDS, CPI and 3M rate."
)

# ---------------------------------------------------------------------------
# Figure 5 - Counterfactual CDS paths
# ---------------------------------------------------------------------------

main_cf_scenarios <- c("CF1_no_energy", "CF4_no_sovereign", "CF6_no_energy_no_sovereign")

cf_panel <- cf_country |>
  filter(variable == "dlog_CDS", scenario %in% main_cf_scenarios) |>
  mutate(date_q = quarter_date(Quarter_ID), scenario_short = short_scenario(scenario)) |>
  group_by(scenario, scenario_short, Quarter_ID, date_q) |>
  summarise(
    Actual = mean(actual, na.rm = TRUE),
    Fitted = mean(fitted, na.rm = TRUE),
    Counterfactual = mean(counterfactual, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(cols = c(Actual, Fitted, Counterfactual), names_to = "series", values_to = "value") |>
  mutate(
    series_label = if_else(series == "Counterfactual", scenario_short, series),
    series_label = factor(series_label, levels = scenario_levels),
    scenario_short = factor(scenario_short, levels = scenario_levels[3:5])
  )

fig5 <- ggplot(cf_panel, aes(date_q, value, color = series_label, linewidth = series_label)) +
  annotate("rect", xmin = shade_start, xmax = shade_end, ymin = -Inf, ymax = Inf, fill = "#B65A4A", alpha = 0.07) +
  geom_hline(yintercept = 0, color = "#555555", linewidth = 0.25) +
  geom_line() +
  facet_wrap(~scenario_short, ncol = 3, scales = "free_y") +
  scale_color_manual(values = scenario_palette, breaks = scenario_levels, drop = FALSE) +
  scale_linewidth_manual(values = c("Actual" = 0.45, "Fitted" = 0.5, "No Energy" = 0.8, "No Sovereign" = 0.8, "No Energy + No Sovereign" = 0.8), guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Counterfactual sovereign-risk repricing under selected shock-removal scenarios",
    subtitle = "Panel-average dlog_CDS paths. Counterfactual paths remove the historical contribution of the indicated structural shocks.",
    x = NULL,
    y = "dlog_CDS"
  ) +
  theme_q1()

fig5_base <- file.path(MAIN_POLISHED_DIR, "F05_counterfactual_dlog_CDS_selected_scenarios_polished")
save_q1_plot(fig5, fig5_base, width = 9, height = 5.5)
add_figure_manifest(
  "F05", fig5_base, "Counterfactual sovereign-risk repricing under selected shock-removal scenarios", "Counterfactual analysis", "main",
  "dlog_CDS", "No Energy; No Sovereign; No Energy + No Sovereign",
  "Figure 5. Counterfactual sovereign-risk repricing under selected shock-removal scenarios. The figure reports panel-average fitted and counterfactual dlog_CDS paths for the three main model-implied scenarios. The shaded area denotes 2021Q1-2023Q4.",
  "Combines original separate CF1, CF4 and CF6 figures."
)

# ---------------------------------------------------------------------------
# Figure 6 - Country heterogeneity in counterfactual CDS effects
# ---------------------------------------------------------------------------

rank_combined <- bind_rows(rank_no_energy, rank_no_sovereign, rank_no_energy_no_sovereign) |>
  mutate(
    scenario_short = short_scenario(scenario),
    scenario_short = factor(scenario_short, levels = scenario_levels[3:5]),
    effect_pct = 100 * cumulative_percent_effect_dlog_CDS
  ) |>
  group_by(scenario_short) |>
  arrange(effect_pct, .by_group = TRUE) |>
  mutate(country_panel = factor(paste(Country, scenario_short, sep = "___"), levels = paste(Country, scenario_short, sep = "___"))) |>
  ungroup()

rank_x_limits <- c(
  min(-20, floor(min(rank_combined$effect_pct, na.rm = TRUE) / 10) * 10),
  max(rank_combined$effect_pct, na.rm = TRUE) * 1.35
)

fig6 <- ggplot(rank_combined, aes(effect_pct, country_panel, fill = scenario_short)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", effect_pct)), hjust = -0.08, size = 2.8, color = "#333333") +
  facet_wrap(~scenario_short, nrow = 1, scales = "free_y") +
  scale_y_discrete(labels = function(x) sub("___.*$", "", x)) +
  scale_x_continuous(labels = label_percent(scale = 1), breaks = c(0, 100, 200), limits = rank_x_limits, expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = scenario_palette, breaks = scenario_levels, drop = FALSE, guide = "none") +
  coord_cartesian(clip = "off") +
  labs(
    title = "Cross-country heterogeneity in counterfactual CDS effects",
    subtitle = "Cumulative percent effects during 2021Q1-2023Q4.",
    x = "Cumulative percent effect",
    y = NULL
  ) +
  theme_q1(base_size = 10)

fig6_base <- file.path(MAIN_POLISHED_DIR, "F06_country_heterogeneity_counterfactual_CDS_polished")
save_q1_plot(fig6, fig6_base, width = 9, height = 5.5)
add_figure_manifest(
  "F06", fig6_base, "Cross-country heterogeneity in counterfactual CDS effects", "Country heterogeneity", "main",
  "dlog_CDS", "No Energy; No Sovereign; No Energy + No Sovereign",
  "Figure 6. Cross-country heterogeneity in counterfactual CDS effects. Bars report cumulative percent effects during 2021Q1-2023Q4, computed as exp(cumulative gap)-1.",
  "Combines separate country rankings into one manuscript figure."
)

# ---------------------------------------------------------------------------
# Appendix polished figures
# ---------------------------------------------------------------------------

plot_irf_grid <- function(shock_name, fig_id) {
  d <- struct_irf |>
    filter(shock == shock_name) |>
    mutate(response_label = factor(var_label(response), levels = var_label(c("Energy_Factor", "d_CISS", "d_CPI", "GDP_Growth", "d_3MRate", "d_FiscalBalanceGDP", "dlog_CDS"))))
  p <- ggplot(d, aes(horizon, median_irf)) +
    geom_hline(yintercept = 0, linewidth = 0.25, color = "#555555") +
    geom_ribbon(aes(ymin = p2_5, ymax = p97_5), fill = "#8A96A8", alpha = 0.16) +
    geom_ribbon(aes(ymin = p16, ymax = p84), fill = "#3F7C8A", alpha = 0.24) +
    geom_line(color = "#111111", linewidth = 0.55) +
    facet_wrap(~response_label, ncol = 3, scales = "free_y") +
    labs(
      title = paste("Appendix structural IRFs:", short_shock(shock_name)),
      subtitle = "All model responses for the selected identified shock.",
      x = "Horizon",
      y = "Median response"
    ) +
    theme_q1(base_size = 10)
  base <- file.path(APPENDIX_POLISHED_DIR, paste0(fig_id, "_structural_irf_", gsub("[^A-Za-z0-9]+", "_", short_shock(shock_name)), "_polished"))
  save_q1_plot(p, base, width = 11, height = 6.5)
  add_figure_manifest(
    fig_id, base, paste("Appendix structural IRFs:", short_shock(shock_name)), "Appendix structural IRFs", "appendix",
    paste(unique(d$response), collapse = "; "), short_shock(shock_name),
    paste0("Appendix Figure ", fig_id, ". Structural impulse responses to the ", short_shock(shock_name), " shock."),
    "Supplementary polished IRF grid.", "appendix"
  )
}

plot_irf_grid("Energy-carbon pressure shock", "A01")
plot_irf_grid("Systemic financial stress shock", "A02")
plot_irf_grid("Inflationary monetary-reaction shock", "A03")
plot_irf_grid("Sovereign-risk repricing shock", "A04")

fevd_h12_all <- struct_fevd |>
  filter(horizon == 12) |>
  mutate(
    shock_short = factor(short_shock(shock), levels = shock_levels),
    response_label = factor(var_label(response), levels = var_label(c("Energy_Factor", "d_CISS", "d_CPI", "GDP_Growth", "d_3MRate", "d_FiscalBalanceGDP", "dlog_CDS")))
  ) |>
  group_by(response_label) |>
  mutate(share_pct = 100 * mean_share / sum(mean_share, na.rm = TRUE)) |>
  ungroup()

fig_a05 <- ggplot(fevd_h12_all, aes(response_label, share_pct, fill = shock_short)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.15) +
  coord_flip() +
  scale_fill_manual(values = shock_palette, breaks = shock_levels, drop = FALSE) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100), expand = expansion(mult = c(0, 0.01))) +
  labs(
    title = "Appendix structural FEVD at horizon 12",
    subtitle = "All model variables.",
    x = NULL,
    y = "Share of forecast-error variance"
  ) +
  theme_q1()

a05_base <- file.path(APPENDIX_POLISHED_DIR, "A05_structural_fevd_h12_all_variables_polished")
save_q1_plot(fig_a05, a05_base, width = 11, height = 6.5)
add_figure_manifest("A05", a05_base, "Appendix structural FEVD at horizon 12", "Appendix FEVD", "appendix",
                    "All variables", "All shocks", "Appendix Figure A05. Structural FEVD at horizon 12 for all model variables.",
                    "Supplementary all-variable FEVD.", "appendix")

hd_appendix_long <- hd_panel_appendix |>
  mutate(date_q = quarter_date(Quarter_ID), variable_label = factor(var_label(variable), levels = var_label(c("GDP_Growth", "d_FiscalBalanceGDP")))) |>
  pivot_longer(
    cols = c(contribution_energy, contribution_ciss, contribution_inflationary_monetary,
             contribution_sovereign, contribution_other, initial_deterministic_component),
    names_to = "component",
    values_to = "contribution"
  ) |>
  mutate(component_short = factor(short_shock(component), levels = shock_levels))

fig_a06 <- ggplot(hd_appendix_long, aes(date_q)) +
  annotate("rect", xmin = shade_start, xmax = shade_end, ymin = -Inf, ymax = Inf, fill = "#B65A4A", alpha = 0.07) +
  geom_col(aes(y = contribution, fill = component_short), width = 70, alpha = 0.95) +
  geom_line(aes(y = actual_panel_average), color = "#111111", linewidth = 0.35) +
  geom_hline(yintercept = 0, color = "#555555", linewidth = 0.25) +
  facet_wrap(~variable_label, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = shock_palette, breaks = shock_levels, drop = FALSE) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Appendix historical decomposition of GDP growth and fiscal balance",
    subtitle = "Panel-average contributions from identified structural shocks.",
    x = NULL,
    y = "Contribution / actual"
  ) +
  theme_q1()

a06_base <- file.path(APPENDIX_POLISHED_DIR, "A06_historical_decomposition_gdp_fiscal_polished")
save_q1_plot(fig_a06, a06_base, width = 11, height = 6.5)
add_figure_manifest("A06", a06_base, "Appendix historical decomposition of GDP growth and fiscal balance", "Appendix HD", "appendix",
                    "GDP_Growth; d_FiscalBalanceGDP", "All shocks", "Appendix Figure A06. Historical decomposition of GDP growth and fiscal balance.",
                    "Supplementary HD for macro/fiscal variables.", "appendix")

cf_appendix <- cf_country |>
  filter(variable %in% c("d_CPI", "d_3MRate", "GDP_Growth"), scenario == "CF1_no_energy") |>
  mutate(date_q = quarter_date(Quarter_ID), variable_label = factor(var_label(variable), levels = var_label(c("d_CPI", "d_3MRate", "GDP_Growth")))) |>
  group_by(variable_label, Quarter_ID, date_q) |>
  summarise(
    Actual = mean(actual, na.rm = TRUE),
    Fitted = mean(fitted, na.rm = TRUE),
    `No Energy` = mean(counterfactual, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(cols = c(Actual, Fitted, `No Energy`), names_to = "series", values_to = "value") |>
  mutate(series = factor(series, levels = scenario_levels))

fig_a07 <- ggplot(cf_appendix, aes(date_q, value, color = series, linewidth = series)) +
  annotate("rect", xmin = shade_start, xmax = shade_end, ymin = -Inf, ymax = Inf, fill = "#B65A4A", alpha = 0.07) +
  geom_hline(yintercept = 0, color = "#555555", linewidth = 0.25) +
  geom_line() +
  facet_wrap(~variable_label, ncol = 1, scales = "free_y") +
  scale_color_manual(values = scenario_palette, breaks = scenario_levels, drop = FALSE) +
  scale_linewidth_manual(values = c("Actual" = 0.45, "Fitted" = 0.5, "No Energy" = 0.8, "No Sovereign" = 0.8, "No Energy + No Sovereign" = 0.8), guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Appendix no-energy counterfactuals for macro-financial variables",
    subtitle = "Panel-average paths under the no-energy-carbon shock-removal scenario.",
    x = NULL,
    y = "Variable value"
  ) +
  theme_q1()

a07_base <- file.path(APPENDIX_POLISHED_DIR, "A07_no_energy_counterfactual_macro_variables_polished")
save_q1_plot(fig_a07, a07_base, width = 11, height = 6.5)
add_figure_manifest("A07", a07_base, "Appendix no-energy counterfactuals for macro-financial variables", "Appendix counterfactuals", "appendix",
                    "d_CPI; d_3MRate; GDP_Growth", "No Energy", "Appendix Figure A07. No-energy counterfactual paths for selected macro-financial variables.",
                    "Supplementary counterfactual panel.", "appendix")

# ---------------------------------------------------------------------------
# Polished manuscript tables
# ---------------------------------------------------------------------------

table_1 <- read_sheet(MASTER_WB, "T03_variable_definitions")
table_2 <- bind_rows(
  read_sheet(MASTER_WB, "T05_descriptive_stats_full") |> mutate(table_block = "Descriptive statistics"),
  pca_energy |> mutate(table_block = "PCA loadings / variance")
)
table_3 <- read_sheet(MASTER_WB, "T11_fe_lsdv_pvar_coefficients")
table_4 <- read_sheet(MASTER_WB, "T19_model_comparison")
table_5 <- read_sheet(MASTER_WB, "T21_structural_sign_restriction")
table_6 <- fevd_h12_main |>
  transmute(
    response = as.character(response_label),
    shock = as.character(shock_short),
    mean_share_pct = round(share_pct, 2),
    median_share_pct = round(median_share_pct, 2),
    horizon = horizon
  ) |>
  arrange(response, factor(shock, levels = shock_levels))
table_7 <- read_sheet(MASTER_WB, "T37_hd_summary_by_period") |>
  filter(variable %in% c("dlog_CDS", "d_CPI", "d_3MRate", "GDP_Growth", "d_FiscalBalanceGDP")) |>
  select(period, period_start, period_end, variable, starts_with("cumulative_"), starts_with("share_abs_"), dominant_abs_contributor)
table_8 <- read_sheet(MASTER_WB, "T44_cf_dlog_CDS_panel") |>
  filter(scenario %in% main_cf_scenarios) |>
  mutate(scenario_short = short_scenario(scenario)) |>
  select(period, period_start, period_end, variable, scenario_short, observations, mean_actual, mean_fitted,
         mean_counterfactual, cumulative_gap, cumulative_percent_effect_dlog_CDS,
         rank_by_abs_cumulative_effect, effect_sign, interpretation_flag)
table_9 <- rank_combined |>
  transmute(
    period, period_start, period_end, Country, variable,
    scenario = as.character(scenario_short),
    observations,
    cumulative_gap,
    cumulative_percent_effect_dlog_CDS,
    percent_effect = effect_pct,
    rank_by_abs_cumulative_effect
  ) |>
  arrange(scenario, rank_by_abs_cumulative_effect)

polished_tables <- list(
  Table_1_variables = table_1,
  Table_2_descriptive_pca = table_2,
  Table_3_full_pvar_matrix = table_3,
  Table_4_robustness_summary = table_4,
  Table_5_sign_restrictions = table_5,
  Table_6_fevd_h12_main = table_6,
  Table_7_hd_summary = table_7,
  Table_8_cf_CDS_effects = table_8,
  Table_9_country_heterogeneity = table_9
)

openxlsx::write.xlsx(polished_tables, file.path(MASTER_DIR, "MASTER_polished_tables_for_manuscript.xlsx"), overwrite = TRUE)

table_manifest <- tibble(
  table_id = paste0("Table_", 1:9),
  sheet_name = names(polished_tables),
  title = c(
    "Variables, definitions and transformations",
    "Descriptive statistics and PCA loadings",
    "Full FE/LSDV PVAR(1) coefficient matrix",
    "Robustness comparison",
    "Structural sign restrictions",
    "Structural FEVD at h=12 for main variables",
    "Historical decomposition summary",
    "Counterfactual CDS effects",
    "Country heterogeneity in CDS counterfactuals"
  ),
  manuscript_section = c("Data", "Data", "Reduced-form model", "Robustness", "Structural identification", "FEVD", "Historical decomposition", "Counterfactual analysis", "Country heterogeneity"),
  main_text_or_appendix = c("main", "main", "main", "main", "main", "main", "main", "main", "main/optional"),
  source = c(
    "MASTER_all_tables_for_paper.xlsx:T03",
    "MASTER_all_tables_for_paper.xlsx:T05/T09",
    "MASTER_all_tables_for_paper.xlsx:T11",
    "MASTER_all_tables_for_paper.xlsx:T19",
    "MASTER_all_tables_for_paper.xlsx:T21",
    "MASTER_appendix_tables.xlsx:all_structural_fevd",
    "MASTER_all_tables_for_paper.xlsx:T37",
    "MASTER_all_tables_for_paper.xlsx:T44",
    "MASTER_all_tables_for_paper.xlsx:T52/T53/T54"
  ),
  notes = c(
    "Use as Table 1.",
    "Use as Table 2; can split if manuscript layout requires.",
    "Full matrix; consider appendix if journal asks for compact main text.",
    "FE/LSDV vs GMM vs LP-DK comparison.",
    "No repaired4 or baseline3 included.",
    "Main variables only; h=12.",
    "Full sample and subperiods.",
    "CF1, CF4 and CF6 only.",
    "Optional main text if Figure 6 is already included."
  ),
  status = c("recommended", "recommended", "recommended", "recommended", "recommended", "recommended", "recommended", "recommended", "optional")
)
openxlsx::write.xlsx(list(table_manifest = table_manifest), file.path(TABLE_MANIFEST_DIR, "table_manifest_polished.xlsx"), overwrite = TRUE)

# ---------------------------------------------------------------------------
# Captions and reports
# ---------------------------------------------------------------------------

figure_manifest_df <- bind_rows(figure_manifest)
openxlsx::write.xlsx(list(figure_manifest = figure_manifest_df), file.path(FIGURE_DIR, "figure_manifest_polished.xlsx"), overwrite = TRUE)

figure_captions <- c(
  "# Polished Figure Captions",
  "",
  figure_manifest_df |>
    filter(main_text_or_appendix == "main") |>
    transmute(line = paste0("**", figure_id, ". ", title, ".** ", recommended_caption)) |>
    pull(line)
)
writeLines(figure_captions, file.path(REPORT_DIR, "paper_figure_captions_polished.md"))

table_captions <- c(
  "# Polished Table Captions",
  "",
  "**Table 1. Variables, definitions and transformations.** The table reports the variables used in the final seven-variable Structural PVAR and their transformations.",
  "**Table 2. Descriptive statistics and PCA loadings.** The table reports descriptive statistics for the final estimation sample and the PC1 loadings used to construct the energy-carbon pressure factor.",
  "**Table 3. Full FE/LSDV PVAR(1) coefficient matrix.** The table reports the reduced-form FE/LSDV PVAR(1) coefficient estimates for the final model.",
  "**Table 4. Robustness comparison.** The table compares selected FE/LSDV, PVAR-GMM and LP-DK results for the main macro-financial relationships.",
  "**Table 5. Structural sign restrictions.** The table reports the sign-restriction scheme for the final Structural PVAR refined4 S1.",
  "**Table 6. Structural FEVD at h=12 for main variables.** The table reports the h=12 forecast-error variance shares for CPI, GDP growth, the 3M rate, fiscal balance and CDS.",
  "**Table 7. Historical decomposition summary.** The table reports cumulative historical-decomposition contributions over the full sample and key subperiods.",
  "**Table 8. Counterfactual CDS effects.** The table reports panel-average dlog_CDS effects for the three main shock-removal scenarios.",
  "**Table 9. Country heterogeneity in CDS counterfactuals.** The table reports cross-country cumulative percent effects for dlog_CDS during 2021Q1-2023Q4."
)
writeLines(table_captions, file.path(REPORT_DIR, "paper_table_captions_polished.md"))

report_lines <- c(
  "# Final Visual Polishing Report",
  "",
  "## 1. Figures Redone",
  "- F01 energy-carbon factor was rebuilt as a two-panel factor/loadings figure.",
  "- F02 was rebuilt as a selected 2x3 structural IRF figure.",
  "- F03 was rebuilt as a horizon-12 FEVD stacked bar chart for the main macro-financial variables.",
  "- F04 combines historical decompositions for dlog_CDS, d_CPI and d_3MRate.",
  "- F05 combines the three main dlog_CDS counterfactual scenarios.",
  "- F06 combines country heterogeneity rankings for the three main counterfactual scenarios.",
  "",
  "## 2. Figures Combined",
  "- Separate HD figures for dlog_CDS, d_CPI and d_3MRate were combined into F04.",
  "- Separate CF1, CF4 and CF6 counterfactual figures were combined into F05.",
  "- Separate country ranking figures were combined into F06.",
  "",
  "## 3. Figures Removed From Main Paper",
  "- The original reduced-form heatmap is not recommended for the main paper in its previous form.",
  "- The original dlog_CDS-only FEVD figure is replaced by F03.",
  "- Separate counterfactual, HD and ranking figures are replaced by combined panels.",
  "",
  "## 4. Recommended Main Manuscript Tables",
  "- Table 1: Variables, definitions and transformations.",
  "- Table 2: Descriptive statistics and PCA loadings.",
  "- Table 3: Full FE/LSDV PVAR(1) coefficient matrix.",
  "- Table 4: Robustness comparison.",
  "- Table 5: Structural sign restrictions.",
  "- Table 6: Structural FEVD at h=12 for main variables.",
  "- Table 7: Historical decomposition summary.",
  "- Table 8: Counterfactual CDS effects.",
  "- Table 9: Country heterogeneity in CDS counterfactuals, optional if Figure 6 is retained.",
  "",
  "## 5. Appendix / Replication Package",
  "- Exhaustive structural IRFs, all-variable FEVD, macro/fiscal HD and additional counterfactuals remain appendix or replication material.",
  "- Diagnostics and full exhaustive outputs should remain outside the main manuscript unless requested by reviewers.",
  "",
  "## 6. Visual Style",
  "- All polished figures use a neutral colorblind-friendly palette.",
  "- Shock labels are shortened consistently: Energy, CISS, Inflation/Rate, Sovereign, Other and Initial.",
  "- Counterfactual labels are shortened consistently: Actual, Fitted, No Energy, No Sovereign and No Energy + No Sovereign.",
  "- Figures use ggplot2 with the custom theme_q1 and are exported through ragg/cairo.",
  "",
  "## 7. Export Check",
  paste0("- Main polished figures exported: ", sum(figure_manifest_df$main_text_or_appendix == "main"), " PNG and PDF pairs."),
  paste0("- Appendix polished figures exported: ", sum(figure_manifest_df$main_text_or_appendix == "appendix"), " PNG and PDF pairs."),
  "",
  "## 8. Remaining Visual Issues",
  "- No model-result changes were made.",
  "- Manual manuscript placement should check journal column width and whether Table 3 is too large for the main text.",
  "",
  "## 9. Final Recommended Main Paper Figures",
  "1. F01_energy_factor_pca_components_polished",
  "2. F02_selected_structural_irfs_polished",
  "3. F03_structural_fevd_h12_main_variables_polished",
  "4. F04_historical_decomposition_main_variables_polished",
  "5. F05_counterfactual_dlog_CDS_selected_scenarios_polished",
  "6. F06_country_heterogeneity_counterfactual_CDS_polished",
  "",
  "## 10. Manual Checks Before Submit",
  "- Verify figure sizing after insertion into the manuscript template.",
  "- Check whether Table 3 should move to the appendix if the journal imposes strict table-length limits.",
  "- Confirm that captions match final manuscript numbering.",
  "- Confirm that the shaded 2021Q1-2023Q4 episode is described consistently in the text."
)
writeLines(report_lines, file.path(REPORT_DIR, "final_visual_polishing_report.md"))

message("Q1 polishing complete.")
message("Main polished figures: ", MAIN_POLISHED_DIR)
message("Appendix polished figures: ", APPENDIX_POLISHED_DIR)
message("Polished tables: ", file.path(MASTER_DIR, "MASTER_polished_tables_for_manuscript.xlsx"))
