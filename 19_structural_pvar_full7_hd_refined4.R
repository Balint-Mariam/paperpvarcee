# Structural PVAR Historical Decomposition - refined4 final model.
# This script runs only historical decomposition. It does not run
# counterfactual analysis, repaired4, or sensitivity structural models.

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

INPUT_DIR <- "structural_pvar_ciss_full7_final_outputs"
STRUCTURAL_DIR <- "structural_pvar_ciss_full7_structural_refined4_outputs"
FE_WORKBOOK <- file.path(INPUT_DIR, "04_fe_lsdv_pvar1_full7_final.xlsx")
DATA_WORKBOOK <- file.path(INPUT_DIR, "01_data_preparation_full7_final.xlsx")
OUTPUT_DIR <- "structural_pvar_ciss_full7_historical_decomposition_outputs"
FIGURE_DIR <- file.path(OUTPUT_DIR, "figures")
FIGURE_SUBDIRS <- c(
  "hd_panel_average",
  "hd_country_level",
  "hd_cumulative",
  "hd_dlog_CDS_focus",
  "hd_summary_periods",
  "paper_figures"
)

MODEL_VARS <- c(
  "Energy_Factor",
  "d_CISS",
  "d_CPI",
  "GDP_Growth",
  "d_3MRate",
  "d_FiscalBalanceGDP",
  "dlog_CDS"
)

SHOCKS <- data.frame(
  shock_key = c("energy", "financial_stress", "inflationary_reaction", "sovereign_repricing"),
  shock = c(
    "Energy-carbon pressure shock",
    "Systemic financial stress shock",
    "Inflationary monetary-reaction shock",
    "Sovereign-risk repricing shock"
  ),
  stringsAsFactors = FALSE
)

OTHER_SHOCKS <- c("Other shock 1", "Other shock 2", "Other shock 3")
STRUCTURAL_COLUMNS <- c(SHOCKS$shock, OTHER_SHOCKS)

SIGN_RESTRICTIONS <- data.frame(
  shock_key = c(
    "energy", "energy", "energy",
    "financial_stress", "financial_stress",
    "inflationary_reaction", "inflationary_reaction",
    "sovereign_repricing"
  ),
  variable = c(
    "Energy_Factor", "d_CPI", "d_3MRate",
    "d_CISS", "GDP_Growth",
    "d_CPI", "d_3MRate",
    "dlog_CDS"
  ),
  restriction = c(
    "positive", "positive", "positive",
    "positive", "negative",
    "positive", "positive",
    "positive"
  ),
  stringsAsFactors = FALSE
) |>
  left_join(SHOCKS, by = "shock_key") |>
  select(shock_key, shock, variable, restriction)

HORIZON <- 12L
RESTRICTION_HORIZONS <- 0:2
SIGN_TOL <- 1e-10
DEFAULT_DRAWS <- as.integer(Sys.getenv("HD_REFINED4_DRAWS", "50000"))
S1_SEED <- as.integer(Sys.getenv("HD_REFINED4_SEED", "20260631"))
MODEL_VARIANT <- "S1_four_shock_sign_only_h0_h2"
MODEL_LABEL <- "Structural PVAR refined4 S1 - four-shock sign-only h=0..2"

CONTRIB_COLS <- c(
  "contribution_energy",
  "contribution_ciss",
  "contribution_inflationary_monetary",
  "contribution_sovereign",
  "contribution_other"
)
CONTRIB_LABELS <- c(
  contribution_energy = "Energy-carbon pressure shock",
  contribution_ciss = "Systemic financial stress shock",
  contribution_inflationary_monetary = "Inflationary monetary-reaction shock",
  contribution_sovereign = "Sovereign-risk repricing shock",
  contribution_other = "Other / unidentified structural shocks",
  initial_deterministic_component = "Initial / deterministic component"
)

PERIODS <- data.frame(
  period = c(
    "Full sample",
    "Pre-energy-crisis",
    "Energy-inflation shock period",
    "Post-shock normalization"
  ),
  start = c("2014Q2", "2014Q2", "2021Q1", "2024Q1"),
  end = c("2025Q4", "2020Q4", "2023Q4", "2025Q4"),
  stringsAsFactors = FALSE
)

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

matrix_to_sheet <- function(mat, row_name = "row") {
  as.data.frame(mat, check.names = FALSE) |>
    tibble::rownames_to_column(row_name)
}

write_workbook <- function(sheets, path) {
  openxlsx::write.xlsx(sheets, path, overwrite = TRUE)
}

skewness_simple <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3L || sd(x) == 0) return(NA_real_)
  mean(((x - mean(x)) / sd(x))^3)
}

kurtosis_simple <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 4L || sd(x) == 0) return(NA_real_)
  mean(((x - mean(x)) / sd(x))^4)
}

read_reduced_form <- function() {
  A_df <- openxlsx::read.xlsx(FE_WORKBOOK, sheet = "A1_matrix")
  Sigma_df <- openxlsx::read.xlsx(FE_WORKBOOK, sheet = "residual_covariance")
  stability <- openxlsx::read.xlsx(FE_WORKBOOK, sheet = "stability_summary")
  sample_summary <- openxlsx::read.xlsx(DATA_WORKBOOK, sheet = "estimation_sample")
  countries <- openxlsx::read.xlsx(DATA_WORKBOOK, sheet = "countries")

  A <- as.matrix(A_df[, MODEL_VARS])
  Sigma <- as.matrix(Sigma_df[, MODEL_VARS])
  storage.mode(A) <- "double"
  storage.mode(Sigma) <- "double"
  rownames(A) <- colnames(A) <- MODEL_VARS
  rownames(Sigma) <- colnames(Sigma) <- MODEL_VARS
  Sigma <- (Sigma + t(Sigma)) / 2

  eigen_A <- eigen(A)$values
  eigen_Sigma <- eigen(Sigma, symmetric = TRUE)$values
  P <- t(chol(Sigma))
  rownames(P) <- MODEL_VARS
  colnames(P) <- paste0("cholesky_col_", seq_len(ncol(P)))

  numerical_checks <- data.frame(
    check = c(
      "reduced_form_max_modulus_from_workbook",
      "reduced_form_stable_from_workbook",
      "max_modulus_recomputed",
      "Sigma_u_positive_definite",
      "Sigma_u_min_eigenvalue",
      "Sigma_u_max_eigenvalue",
      "Cholesky_reconstruction_max_abs_error"
    ),
    value = c(
      as.character(stability$max_modulus[[1]]),
      as.character(stability$stable[[1]]),
      as.character(max(Mod(eigen_A))),
      as.character(all(eigen_Sigma > SIGN_TOL)),
      as.character(min(eigen_Sigma)),
      as.character(max(eigen_Sigma)),
      as.character(max(abs(Sigma - P %*% t(P))))
    ),
    stringsAsFactors = FALSE
  )

  list(
    A = A,
    Sigma = Sigma,
    P = P,
    eigen_A = eigen_A,
    eigen_Sigma = eigen_Sigma,
    stability = stability,
    sample_summary = sample_summary,
    countries = countries,
    numerical_checks = numerical_checks
  )
}

random_orthogonal <- function(k) {
  z <- matrix(rnorm(k * k), nrow = k, ncol = k)
  qr_z <- qr(z)
  q <- qr.Q(qr_z)
  r <- qr.R(qr_z)
  d <- sign(diag(r))
  d[d == 0] <- 1
  sweep(q, 2, d, `*`)
}

precompute_powers <- function(A, horizon) {
  k <- nrow(A)
  powers <- vector("list", horizon + 1L)
  powers[[1]] <- diag(k)
  if (horizon >= 1L) {
    for (h in 1:horizon) powers[[h + 1L]] <- powers[[h]] %*% A
  }
  powers
}

compute_irf_array <- function(A_powers, B, horizon) {
  k <- nrow(B)
  arr <- array(
    NA_real_,
    dim = c(horizon + 1L, k, k),
    dimnames = list(horizon = as.character(0:horizon), response = MODEL_VARS, column = paste0("col", seq_len(k)))
  )
  for (h in 0:horizon) arr[h + 1L, , ] <- A_powers[[h + 1L]] %*% B
  arr
}

make_assignment_grid <- function(k, n_shocks) {
  grid <- do.call(expand.grid, c(rep(list(seq_len(k)), n_shocks), list(KEEP.OUT.ATTRS = FALSE)))
  grid <- as.matrix(grid)
  grid[apply(grid, 1, function(x) length(unique(as.integer(x))) == length(x)), , drop = FALSE]
}

ASSIGNMENT_GRID <- make_assignment_grid(length(MODEL_VARS), nrow(SHOCKS))

sign_restriction_passes <- function(irf_arr, shock_key, col_id, sign_flip) {
  rules <- SIGN_RESTRICTIONS[SIGN_RESTRICTIONS$shock_key == shock_key, , drop = FALSE]
  for (i in seq_len(nrow(rules))) {
    vals <- sign_flip * irf_arr[RESTRICTION_HORIZONS + 1L, rules$variable[[i]], col_id]
    if (rules$restriction[[i]] == "positive" && !all(vals > SIGN_TOL, na.rm = TRUE)) return(FALSE)
    if (rules$restriction[[i]] == "negative" && !all(vals < -SIGN_TOL, na.rm = TRUE)) return(FALSE)
  }
  TRUE
}

restriction_score <- function(irf_arr, shock_key, col_id, sign_flip) {
  rules <- SIGN_RESTRICTIONS[SIGN_RESTRICTIONS$shock_key == shock_key, , drop = FALSE]
  vals <- sign_flip * irf_arr[1L, rules$variable, col_id]
  sum(abs(vals), na.rm = TRUE)
}

build_valid_matrix <- function(irf_arr) {
  n_shocks <- nrow(SHOCKS)
  k <- length(MODEL_VARS)
  valid <- matrix(0L, nrow = n_shocks, ncol = k, dimnames = list(SHOCKS$shock_key, paste0("col", seq_len(k))))
  scores <- matrix(NA_real_, nrow = n_shocks, ncol = k, dimnames = list(SHOCKS$shock_key, paste0("col", seq_len(k))))

  for (s in seq_len(n_shocks)) {
    shock_key <- SHOCKS$shock_key[[s]]
    for (j in seq_len(k)) {
      pass_pos <- sign_restriction_passes(irf_arr, shock_key, j, 1)
      pass_neg <- sign_restriction_passes(irf_arr, shock_key, j, -1)
      if (pass_pos || pass_neg) {
        if (pass_pos && pass_neg) {
          score_pos <- restriction_score(irf_arr, shock_key, j, 1)
          score_neg <- restriction_score(irf_arr, shock_key, j, -1)
          valid[s, j] <- ifelse(score_pos >= score_neg, 1L, -1L)
          scores[s, j] <- max(score_pos, score_neg)
        } else if (pass_pos) {
          valid[s, j] <- 1L
          scores[s, j] <- restriction_score(irf_arr, shock_key, j, 1)
        } else {
          valid[s, j] <- -1L
          scores[s, j] <- restriction_score(irf_arr, shock_key, j, -1)
        }
      }
    }
  }
  list(valid = valid, scores = scores)
}

find_best_assignment <- function(valid, scores) {
  n_shocks <- nrow(valid)
  if (any(rowSums(valid != 0L) == 0L)) {
    return(list(ok = FALSE, reason = "no_valid_assignment", n_assignments = 0L))
  }
  ok <- rep(TRUE, nrow(ASSIGNMENT_GRID))
  assignment_scores <- rep(0, nrow(ASSIGNMENT_GRID))
  for (s in seq_len(n_shocks)) {
    cols <- ASSIGNMENT_GRID[, s]
    valid_values <- valid[cbind(s, cols)]
    ok <- ok & valid_values != 0L
    score_values <- scores[cbind(s, cols)]
    score_values[!is.finite(score_values)] <- 0
    assignment_scores <- assignment_scores + score_values
  }
  ok_rows <- which(ok)
  if (length(ok_rows) == 0L) {
    return(list(ok = FALSE, reason = "non_distinct_columns", n_assignments = 0L))
  }
  best_row <- ok_rows[which.max(assignment_scores[ok_rows])]
  best_cols <- as.integer(ASSIGNMENT_GRID[best_row, ])
  best_signs <- valid[cbind(seq_along(best_cols), best_cols)]
  list(ok = TRUE, reason = "accepted", n_assignments = length(ok_rows), cols = best_cols, signs = best_signs, score = assignment_scores[[best_row]])
}

load_target_median_irf <- function() {
  path <- file.path(STRUCTURAL_DIR, "04_refined4_structural_irf.xlsx")
  irf <- openxlsx::read.xlsx(path, sheet = "S1_structural_irf_all")
  irf <- irf |>
    filter(model_variant == MODEL_VARIANT, horizon %in% 0:HORIZON) |>
    select(shock, response, horizon, median_irf)

  target <- array(
    NA_real_,
    dim = c(HORIZON + 1L, length(MODEL_VARS), nrow(SHOCKS)),
    dimnames = list(horizon = as.character(0:HORIZON), response = MODEL_VARS, shock = SHOCKS$shock)
  )
  for (i in seq_len(nrow(irf))) {
    target[as.character(irf$horizon[[i]]), irf$response[[i]], irf$shock[[i]]] <- irf$median_irf[[i]]
  }
  scales <- vapply(MODEL_VARS, function(v) {
    x <- as.numeric(target[, v, ])
    s <- sd(x, na.rm = TRUE)
    ifelse(is.finite(s) && s > 1e-6, s, 1)
  }, numeric(1))
  list(target = target, response_scales = scales)
}

score_representative_irf <- function(irf_assigned, target, response_scales) {
  total <- 0
  n <- 0
  for (v in MODEL_VARS) {
    diff <- (irf_assigned[, v, ] - target[, v, ]) / response_scales[[v]]
    total <- total + sum(diff^2, na.rm = TRUE)
    n <- n + sum(is.finite(diff))
  }
  total / n
}

validate_restrictions_for_B <- function(A, B_ordered) {
  A_powers <- precompute_powers(A, max(RESTRICTION_HORIZONS))
  irf <- compute_irf_array(A_powers, B_ordered, max(RESTRICTION_HORIZONS))
  rows <- list()
  id <- 1L
  for (s in seq_len(nrow(SHOCKS))) {
    rules <- SIGN_RESTRICTIONS[SIGN_RESTRICTIONS$shock_key == SHOCKS$shock_key[[s]], , drop = FALSE]
    for (i in seq_len(nrow(rules))) {
      vals <- irf[RESTRICTION_HORIZONS + 1L, rules$variable[[i]], s]
      ok <- if (rules$restriction[[i]] == "positive") all(vals > SIGN_TOL) else all(vals < -SIGN_TOL)
      rows[[id]] <- data.frame(
        shock = SHOCKS$shock[[s]],
        variable = rules$variable[[i]],
        restriction = rules$restriction[[i]],
        horizons = paste(RESTRICTION_HORIZONS, collapse = ", "),
        min_value = min(vals),
        max_value = max(vals),
        passed = ok,
        stringsAsFactors = FALSE
      )
      id <- id + 1L
    }
  }
  bind_rows(rows)
}

select_representative_B <- function(A, P) {
  target <- load_target_median_irf()
  set.seed(S1_SEED)
  k <- length(MODEL_VARS)
  A_powers <- precompute_powers(A, HORIZON)
  A_restriction_powers <- precompute_powers(A, max(RESTRICTION_HORIZONS))

  best <- list(score = Inf)
  accepted <- 0L
  rejected_no_valid <- 0L
  rejected_non_distinct <- 0L
  unique_assignment_draws <- 0L

  for (draw in seq_len(DEFAULT_DRAWS)) {
    Q <- random_orthogonal(k)
    B <- P %*% Q
    irf_restriction <- compute_irf_array(A_restriction_powers, B, max(RESTRICTION_HORIZONS))
    vm <- build_valid_matrix(irf_restriction)
    assignment <- find_best_assignment(vm$valid, vm$scores)
    if (!assignment$ok) {
      if (assignment$reason == "no_valid_assignment") rejected_no_valid <- rejected_no_valid + 1L
      if (assignment$reason == "non_distinct_columns") rejected_non_distinct <- rejected_non_distinct + 1L
      next
    }

    accepted <- accepted + 1L
    if (assignment$n_assignments == 1L) unique_assignment_draws <- unique_assignment_draws + 1L

    irf_full <- compute_irf_array(A_powers, B, HORIZON)
    assigned_irf <- array(
      NA_real_,
      dim = c(HORIZON + 1L, k, nrow(SHOCKS)),
      dimnames = list(horizon = as.character(0:HORIZON), response = MODEL_VARS, shock = SHOCKS$shock)
    )
    for (s in seq_len(nrow(SHOCKS))) {
      assigned_irf[, , s] <- assignment$signs[[s]] * irf_full[, , assignment$cols[[s]]]
    }
    score <- score_representative_irf(assigned_irf, target$target, target$response_scales)
    if (is.finite(score) && score < best$score) {
      best <- list(
        score = score,
        candidate_draw = draw,
        accepted_draw = accepted,
        B_raw = B,
        Q = Q,
        assignment = assignment,
        assigned_irf = assigned_irf
      )
    }

    if (draw %% 10000L == 0L) {
      cat("S1 representative search draw", draw, "accepted", accepted, "best_score", round(best$score, 6), "\n")
    }
  }

  if (!is.finite(best$score)) stop("Could not find an accepted S1 representative draw.")

  identified_cols <- best$assignment$cols
  other_cols <- setdiff(seq_len(k), identified_cols)
  B_ordered <- matrix(NA_real_, nrow = k, ncol = k)
  for (s in seq_len(nrow(SHOCKS))) {
    B_ordered[, s] <- best$assignment$signs[[s]] * best$B_raw[, identified_cols[[s]]]
  }
  for (j in seq_along(other_cols)) {
    B_ordered[, nrow(SHOCKS) + j] <- best$B_raw[, other_cols[[j]]]
  }
  rownames(B_ordered) <- MODEL_VARS
  colnames(B_ordered) <- STRUCTURAL_COLUMNS

  previous_acc <- openxlsx::read.xlsx(file.path(STRUCTURAL_DIR, "03_refined4_acceptance_diagnostics.xlsx"), sheet = "acceptance_summary_all") |>
    filter(model_variant == MODEL_VARIANT)
  acceptance <- data.frame(
    model_variant = MODEL_VARIANT,
    seed = S1_SEED,
    candidate_rotations = DEFAULT_DRAWS,
    accepted_rotations = accepted,
    acceptance_rate = accepted / DEFAULT_DRAWS,
    unique_assignment_draws = unique_assignment_draws,
    unique_assignment_rate = unique_assignment_draws / accepted,
    rejected_no_valid_assignment = rejected_no_valid,
    rejected_non_distinct_columns = rejected_non_distinct,
    previous_accepted_rotations = previous_acc$accepted_rotations[[1]],
    previous_acceptance_rate = previous_acc$acceptance_rate[[1]],
    previous_unique_assignment_rate = previous_acc$unique_assignment_rate[[1]],
    accepted_rotation_difference = accepted - previous_acc$accepted_rotations[[1]],
    acceptance_rate_difference = accepted / DEFAULT_DRAWS - previous_acc$acceptance_rate[[1]],
    unique_assignment_rate_difference = unique_assignment_draws / accepted - previous_acc$unique_assignment_rate[[1]],
    stringsAsFactors = FALSE
  )

  representative <- data.frame(
    selection_method = "Rerun refined4 S1 with original seed; choose accepted rotation minimizing standardized squared distance to saved median IRF across all 4 shocks, 7 variables and horizons 0..12.",
    model_variant = MODEL_VARIANT,
    seed = S1_SEED,
    candidate_draw = best$candidate_draw,
    accepted_draw = best$accepted_draw,
    representative_score = best$score,
    assigned_columns = paste(best$assignment$cols, collapse = ", "),
    assigned_signs = paste(best$assignment$signs, collapse = ", "),
    other_columns = paste(other_cols, collapse = ", "),
    possible_distinct_assignments = best$assignment$n_assignments,
    stringsAsFactors = FALSE
  )

  list(
    B = B_ordered,
    Q = best$Q,
    acceptance = acceptance,
    representative = representative,
    validation = validate_restrictions_for_B(A, B_ordered),
    assigned_irf = best$assigned_irf
  )
}

read_hd_data <- function() {
  residuals_long <- openxlsx::read.xlsx(FE_WORKBOOK, sheet = "residuals")
  panel <- openxlsx::read.xlsx(DATA_WORKBOOK, sheet = "estimation_balanced_dataset")

  date_map <- panel |>
    select(Country, quarter_index, Date, Year, Quarter, Quarter_ID, all_of(MODEL_VARS))

  residuals_wide <- residuals_long |>
    pivot_wider(
      id_cols = c(Country, quarter_index),
      names_from = equation,
      values_from = c(residual, fitted, actual),
      names_glue = "{.value}_{equation}"
    ) |>
    left_join(date_map[, c("Country", "quarter_index", "Date", "Year", "Quarter", "Quarter_ID")], by = c("Country", "quarter_index")) |>
    arrange(Country, quarter_index)

  residuals_wide
}

recover_structural_shocks <- function(residuals_wide, B) {
  U <- as.matrix(residuals_wide[, paste0("residual_", MODEL_VARS)])
  colnames(U) <- MODEL_VARS
  eps <- t(solve(B, t(U)))
  colnames(eps) <- STRUCTURAL_COLUMNS
  u_recon <- eps %*% t(B)
  colnames(u_recon) <- MODEL_VARS
  err <- U - u_recon

  shocks <- bind_cols(
    residuals_wide |> select(Country, quarter_index, Date, Year, Quarter, Quarter_ID),
    as.data.frame(eps, check.names = FALSE)
  ) |>
    mutate(
      `Other / unidentified structural shocks` = rowSums(across(all_of(OTHER_SHOCKS))),
      reduced_form_residual_reconstruction_max_abs_error = apply(abs(err), 1, max),
      reduced_form_residual_reconstruction_rmse = sqrt(rowMeans(err^2))
    )

  residual_checks_total <- data.frame(
    check = c("mean_abs_error", "max_abs_error", "rmse"),
    value = c(mean(abs(err)), max(abs(err)), sqrt(mean(err^2))),
    stringsAsFactors = FALSE
  )
  residual_checks_by_variable <- data.frame(
    variable = MODEL_VARS,
    mean_abs_error = colMeans(abs(err)),
    max_abs_error = apply(abs(err), 2, max),
    rmse = sqrt(colMeans(err^2)),
    stringsAsFactors = FALSE
  )
  residual_checks_by_country <- residuals_wide |>
    select(Country) |>
    bind_cols(as.data.frame(abs(err), check.names = FALSE)) |>
    group_by(Country) |>
    summarise(
      mean_abs_error = mean(as.matrix(across(all_of(MODEL_VARS))), na.rm = TRUE),
      max_abs_error = max(as.matrix(across(all_of(MODEL_VARS))), na.rm = TRUE),
      .groups = "drop"
    )

  list(shocks = shocks, eps = eps, checks_total = residual_checks_total, checks_by_variable = residual_checks_by_variable, checks_by_country = residual_checks_by_country)
}

compute_hd <- function(A, B, residuals_wide, eps) {
  rows <- vector("list", nrow(residuals_wide) * length(MODEL_VARS))
  id <- 1L
  countries <- unique(residuals_wide$Country)

  for (country in countries) {
    idx <- which(residuals_wide$Country == country)
    idx <- idx[order(residuals_wide$quarter_index[idx])]
    contribution_state <- matrix(0, nrow = length(MODEL_VARS), ncol = length(STRUCTURAL_COLUMNS))
    rownames(contribution_state) <- MODEL_VARS
    colnames(contribution_state) <- STRUCTURAL_COLUMNS

    for (row_id in idx) {
      eps_t <- as.numeric(eps[row_id, ])
      for (s in seq_along(STRUCTURAL_COLUMNS)) {
        contribution_state[, s] <- as.numeric(A %*% contribution_state[, s] + B[, s] * eps_t[[s]])
      }
      actual_vec <- as.numeric(residuals_wide[row_id, paste0("actual_", MODEL_VARS)])
      fitted_vec <- as.numeric(residuals_wide[row_id, paste0("fitted_", MODEL_VARS)])
      shock_only <- rowSums(contribution_state)
      initial_component <- actual_vec - shock_only
      reconstructed <- shock_only + initial_component

      for (v in seq_along(MODEL_VARS)) {
        rows[[id]] <- data.frame(
          Country = country,
          quarter_index = residuals_wide$quarter_index[[row_id]],
          Date = residuals_wide$Date[[row_id]],
          Year = residuals_wide$Year[[row_id]],
          Quarter = residuals_wide$Quarter[[row_id]],
          Quarter_ID = residuals_wide$Quarter_ID[[row_id]],
          yq_index = q_index(residuals_wide$Quarter_ID[[row_id]]),
          variable = MODEL_VARS[[v]],
          actual = actual_vec[[v]],
          reduced_form_fitted = fitted_vec[[v]],
          shock_only_reconstructed = shock_only[[v]],
          fitted_reconstructed = reconstructed[[v]],
          contribution_energy = contribution_state[v, 1],
          contribution_ciss = contribution_state[v, 2],
          contribution_inflationary_monetary = contribution_state[v, 3],
          contribution_sovereign = contribution_state[v, 4],
          contribution_other = sum(contribution_state[v, 5:7]),
          initial_deterministic_component = initial_component[[v]],
          reconstruction_residual = actual_vec[[v]] - reconstructed[[v]],
          stringsAsFactors = FALSE
        )
        id <- id + 1L
      }
    }
  }

  bind_rows(rows)
}

make_panel_average <- function(country_hd) {
  country_hd |>
    group_by(quarter_index, Date, Year, Quarter, Quarter_ID, yq_index, variable) |>
    summarise(
      actual_panel_average = mean(actual, na.rm = TRUE),
      reduced_form_fitted_panel_average = mean(reduced_form_fitted, na.rm = TRUE),
      shock_only_reconstructed_panel_average = mean(shock_only_reconstructed, na.rm = TRUE),
      fitted_reconstructed_panel_average = mean(fitted_reconstructed, na.rm = TRUE),
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

make_cumulative <- function(country_hd, panel_average) {
  country_cum <- country_hd |>
    arrange(Country, variable, yq_index) |>
    group_by(Country, variable) |>
    mutate(
      cumulative_actual = cumsum(actual),
      cumulative_fitted_reconstructed = cumsum(fitted_reconstructed),
      cumulative_energy = cumsum(contribution_energy),
      cumulative_ciss = cumsum(contribution_ciss),
      cumulative_inflationary_monetary = cumsum(contribution_inflationary_monetary),
      cumulative_sovereign = cumsum(contribution_sovereign),
      cumulative_other = cumsum(contribution_other),
      cumulative_initial_deterministic = cumsum(initial_deterministic_component)
    ) |>
    ungroup()

  panel_cum <- panel_average |>
    arrange(variable, yq_index) |>
    group_by(variable) |>
    mutate(
      cumulative_actual = cumsum(actual_panel_average),
      cumulative_fitted_reconstructed = cumsum(fitted_reconstructed_panel_average),
      cumulative_energy = cumsum(contribution_energy),
      cumulative_ciss = cumsum(contribution_ciss),
      cumulative_inflationary_monetary = cumsum(contribution_inflationary_monetary),
      cumulative_sovereign = cumsum(contribution_sovereign),
      cumulative_other = cumsum(contribution_other),
      cumulative_initial_deterministic = cumsum(initial_deterministic_component)
    ) |>
    ungroup() |>
    mutate(Country = "Panel average", .before = 1)

  pct_panel <- panel_cum |>
    filter(variable == "dlog_CDS") |>
    transmute(
      Country,
      quarter_index,
      Date,
      Year,
      Quarter,
      Quarter_ID,
      variable,
      percent_effect_energy = exp(cumulative_energy) - 1,
      percent_effect_ciss = exp(cumulative_ciss) - 1,
      percent_effect_inflationary_monetary = exp(cumulative_inflationary_monetary) - 1,
      percent_effect_sovereign = exp(cumulative_sovereign) - 1,
      percent_effect_other = exp(cumulative_other) - 1
    )
  pct_country <- country_cum |>
    filter(variable == "dlog_CDS") |>
    transmute(
      Country,
      quarter_index,
      Date,
      Year,
      Quarter,
      Quarter_ID,
      variable,
      percent_effect_energy = exp(cumulative_energy) - 1,
      percent_effect_ciss = exp(cumulative_ciss) - 1,
      percent_effect_inflationary_monetary = exp(cumulative_inflationary_monetary) - 1,
      percent_effect_sovereign = exp(cumulative_sovereign) - 1,
      percent_effect_other = exp(cumulative_other) - 1
    )

  list(
    country_cumulative = country_cum,
    panel_cumulative = panel_cum,
    dlog_CDS_percent_effects = bind_rows(pct_panel, pct_country)
  )
}

add_periods <- function(df) {
  bind_rows(lapply(seq_len(nrow(PERIODS)), function(i) {
    start_i <- q_index(PERIODS$start[[i]])
    end_i <- q_index(PERIODS$end[[i]])
    df |>
      filter(yq_index >= start_i, yq_index <= end_i) |>
      mutate(period = PERIODS$period[[i]], period_start = PERIODS$start[[i]], period_end = PERIODS$end[[i]])
  }))
}

summarise_periods <- function(df, actual_col, fitted_col, group_cols = character()) {
  period_df <- add_periods(df)
  period_df |>
    group_by(across(all_of(c(group_cols, "period", "period_start", "period_end", "variable")))) |>
    summarise(
      observations = n(),
      mean_actual = mean(.data[[actual_col]], na.rm = TRUE),
      mean_fitted = mean(.data[[fitted_col]], na.rm = TRUE),
      mean_energy = mean(contribution_energy, na.rm = TRUE),
      mean_ciss = mean(contribution_ciss, na.rm = TRUE),
      mean_inflationary_monetary = mean(contribution_inflationary_monetary, na.rm = TRUE),
      mean_sovereign = mean(contribution_sovereign, na.rm = TRUE),
      mean_other = mean(contribution_other, na.rm = TRUE),
      cumulative_energy = sum(contribution_energy, na.rm = TRUE),
      cumulative_ciss = sum(contribution_ciss, na.rm = TRUE),
      cumulative_inflationary_monetary = sum(contribution_inflationary_monetary, na.rm = TRUE),
      cumulative_sovereign = sum(contribution_sovereign, na.rm = TRUE),
      cumulative_other = sum(contribution_other, na.rm = TRUE),
      abs_total = sum(abs(contribution_energy) + abs(contribution_ciss) + abs(contribution_inflationary_monetary) + abs(contribution_sovereign) + abs(contribution_other), na.rm = TRUE),
      share_abs_energy = sum(abs(contribution_energy), na.rm = TRUE) / abs_total,
      share_abs_ciss = sum(abs(contribution_ciss), na.rm = TRUE) / abs_total,
      share_abs_inflationary_monetary = sum(abs(contribution_inflationary_monetary), na.rm = TRUE) / abs_total,
      share_abs_sovereign = sum(abs(contribution_sovereign), na.rm = TRUE) / abs_total,
      share_abs_other = sum(abs(contribution_other), na.rm = TRUE) / abs_total,
      .groups = "drop"
    ) |>
    rowwise() |>
    mutate(
      dominant_positive_contributor = {
        vals <- c(
          Energy = cumulative_energy,
          CISS = cumulative_ciss,
          Inflationary = cumulative_inflationary_monetary,
          Sovereign = cumulative_sovereign,
          Other = cumulative_other
        )
        names(vals)[which.max(vals)]
      },
      dominant_negative_contributor = {
        vals <- c(
          Energy = cumulative_energy,
          CISS = cumulative_ciss,
          Inflationary = cumulative_inflationary_monetary,
          Sovereign = cumulative_sovereign,
          Other = cumulative_other
        )
        names(vals)[which.min(vals)]
      },
      dominant_abs_contributor = {
        vals <- c(
          Energy = abs(cumulative_energy),
          CISS = abs(cumulative_ciss),
          Inflationary = abs(cumulative_inflationary_monetary),
          Sovereign = abs(cumulative_sovereign),
          Other = abs(cumulative_other)
        )
        names(vals)[which.max(vals)]
      }
    ) |>
    ungroup()
}

make_rankings <- function(country_hd) {
  period_df <- add_periods(country_hd |> filter(variable == "dlog_CDS"))
  make_rank <- function(col_name) {
    period_df |>
      group_by(period, period_start, period_end, Country) |>
      summarise(cumulative_contribution = sum(.data[[col_name]], na.rm = TRUE), .groups = "drop") |>
      group_by(period) |>
      arrange(desc(cumulative_contribution), .by_group = TRUE) |>
      mutate(rank_desc = row_number()) |>
      ungroup()
  }
  list(
    energy = make_rank("contribution_energy"),
    ciss = make_rank("contribution_ciss"),
    inflation = make_rank("contribution_inflationary_monetary"),
    sovereign = make_rank("contribution_sovereign")
  )
}

make_shock_stats <- function(shocks) {
  shock_cols <- STRUCTURAL_COLUMNS
  bind_rows(lapply(shock_cols, function(v) {
    x <- shocks[[v]]
    data.frame(
      shock = v,
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      skewness = skewness_simple(x),
      kurtosis = kurtosis_simple(x),
      stringsAsFactors = FALSE
    )
  }))
}

make_reconstruction_checks <- function(country_hd) {
  total <- data.frame(
    level = "overall",
    unit = "all",
    mean_abs_error = mean(abs(country_hd$reconstruction_residual), na.rm = TRUE),
    max_abs_error = max(abs(country_hd$reconstruction_residual), na.rm = TRUE),
    rmse = sqrt(mean(country_hd$reconstruction_residual^2, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )
  by_variable <- country_hd |>
    group_by(variable) |>
    summarise(
      mean_abs_error = mean(abs(reconstruction_residual), na.rm = TRUE),
      max_abs_error = max(abs(reconstruction_residual), na.rm = TRUE),
      rmse = sqrt(mean(reconstruction_residual^2, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    transmute(level = "variable", unit = variable, mean_abs_error, max_abs_error, rmse)
  by_country <- country_hd |>
    group_by(Country) |>
    summarise(
      mean_abs_error = mean(abs(reconstruction_residual), na.rm = TRUE),
      max_abs_error = max(abs(reconstruction_residual), na.rm = TRUE),
      rmse = sqrt(mean(reconstruction_residual^2, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    transmute(level = "country", unit = Country, mean_abs_error, max_abs_error, rmse)
  bind_rows(total, by_variable, by_country)
}

make_main_findings <- function(panel_periods, rankings, reconstruction_checks) {
  full_cds <- panel_periods |> filter(period == "Full sample", variable == "dlog_CDS")
  crisis_cds <- panel_periods |> filter(period == "Energy-inflation shock period", variable == "dlog_CDS")
  crisis_cpi <- panel_periods |> filter(period == "Energy-inflation shock period", variable == "d_CPI")
  crisis_rate <- panel_periods |> filter(period == "Energy-inflation shock period", variable == "d_3MRate")

  abs_name <- function(row) {
    vals <- c(
      Energy = row$share_abs_energy,
      CISS = row$share_abs_ciss,
      Inflationary = row$share_abs_inflationary_monetary,
      Sovereign = row$share_abs_sovereign,
      Other = row$share_abs_other
    )
    names(vals)[which.max(vals)]
  }
  cum_name <- function(row) {
    vals <- c(
      Energy = abs(row$cumulative_energy),
      CISS = abs(row$cumulative_ciss),
      Inflationary = abs(row$cumulative_inflationary_monetary),
      Sovereign = abs(row$cumulative_sovereign),
      Other = abs(row$cumulative_other)
    )
    names(vals)[which.max(vals)]
  }

  top_energy <- rankings$energy |> filter(period == "Energy-inflation shock period") |> slice_max(cumulative_contribution, n = 3, with_ties = FALSE)
  top_ciss <- rankings$ciss |> filter(period == "Energy-inflation shock period") |> slice_max(cumulative_contribution, n = 3, with_ties = FALSE)
  top_sovereign <- rankings$sovereign |> filter(period == "Energy-inflation shock period") |> slice_max(cumulative_contribution, n = 3, with_ties = FALSE)
  max_recon <- reconstruction_checks$max_abs_error[reconstruction_checks$level == "overall"][[1]]
  other_share <- full_cds$share_abs_other[[1]]

  data.frame(
    question = c(
      "Largest mean absolute contributor to dlog_CDS, full sample",
      "Largest cumulative absolute contributor to dlog_CDS, 2021Q1-2023Q4",
      "Largest absolute contributor to d_CPI, 2021Q1-2023Q4",
      "Largest absolute contributor to d_3MRate, 2021Q1-2023Q4",
      "Top countries by cumulative energy contribution to dlog_CDS, 2021Q1-2023Q4",
      "Top countries by cumulative CISS contribution to dlog_CDS, 2021Q1-2023Q4",
      "Top countries by cumulative sovereign contribution to dlog_CDS, 2021Q1-2023Q4",
      "Other / unidentified component",
      "Maximum HD reconstruction error",
      "Counterfactual readiness"
    ),
    answer = c(
      abs_name(full_cds),
      cum_name(crisis_cds),
      abs_name(crisis_cpi),
      paste0(abs_name(crisis_rate), " by absolute contribution share; ", cum_name(crisis_rate), " by net cumulative absolute contribution"),
      paste(top_energy$Country, collapse = ", "),
      paste(top_ciss$Country, collapse = ", "),
      paste(top_sovereign$Country, collapse = ", "),
      paste0("Full-sample dlog_CDS absolute share = ", round(100 * other_share, 2), "%; ", ifelse(other_share > 0.35, "large", "moderate/acceptable")),
      format(max_recon, scientific = TRUE, digits = 4),
      ifelse(max_recon < 1e-8, "Mechanically ready, conditional on accepting refined4 identification and representative-draw approximation.", "Do not proceed before fixing reconstruction.")
    ),
    stringsAsFactors = FALSE
  )
}

plot_contribution_stack <- function(dat, variable_name, out_dir, prefix, actual_col, fitted_col, country_name = NULL, paper_name = NULL) {
  d <- dat |> filter(variable == variable_name) |> arrange(yq_index)
  if (!is.null(country_name)) d <- d |> filter(Country == country_name)
  if (nrow(d) == 0L) return(invisible(NULL))
  d$Quarter_ID <- factor(d$Quarter_ID, levels = unique(d$Quarter_ID))
  long <- d |>
    select(Quarter_ID, all_of(c(CONTRIB_COLS, "initial_deterministic_component")), all_of(c(actual_col, fitted_col))) |>
    pivot_longer(all_of(c(CONTRIB_COLS, "initial_deterministic_component")), names_to = "component", values_to = "contribution") |>
    mutate(component = factor(CONTRIB_LABELS[component], levels = CONTRIB_LABELS[c(CONTRIB_COLS, "initial_deterministic_component")]))

  breaks <- levels(d$Quarter_ID)[seq(1, length(levels(d$Quarter_ID)), by = 4)]
  title_prefix <- ifelse(is.null(country_name), "Panel average", country_name)
  p <- ggplot(long, aes(x = Quarter_ID, y = contribution, fill = component)) +
    geom_col(width = 0.82) +
    geom_line(data = d, aes(x = Quarter_ID, y = .data[[actual_col]], group = 1), inherit.aes = FALSE, color = "black", linewidth = 0.55) +
    geom_line(data = d, aes(x = Quarter_ID, y = .data[[fitted_col]], group = 1), inherit.aes = FALSE, color = "#ba3b35", linewidth = 0.45, linetype = "dashed") +
    scale_x_discrete(breaks = breaks) +
    labs(title = paste(title_prefix, "historical decomposition:", variable_name), x = NULL, y = "Contribution / actual", fill = NULL) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
  file <- file.path(out_dir, paste0(prefix, "_stacked_", safe_name(ifelse(is.null(country_name), variable_name, paste(country_name, variable_name))), ".png"))
  ggsave(file, p, width = 11, height = 6.2, dpi = 160)
  if (!is.null(paper_name)) ggsave(file.path(FIGURE_DIR, "paper_figures", paper_name), p, width = 11, height = 6.2, dpi = 180)
}

plot_actual_vs_reconstructed <- function(dat, variable_name, out_dir, prefix, actual_col, fitted_col, country_name = NULL) {
  d <- dat |> filter(variable == variable_name) |> arrange(yq_index)
  if (!is.null(country_name)) d <- d |> filter(Country == country_name)
  if (nrow(d) == 0L) return(invisible(NULL))
  d$Quarter_ID <- factor(d$Quarter_ID, levels = unique(d$Quarter_ID))
  breaks <- levels(d$Quarter_ID)[seq(1, length(levels(d$Quarter_ID)), by = 4)]
  title_prefix <- ifelse(is.null(country_name), "Panel average", country_name)
  p <- ggplot(d, aes(x = Quarter_ID)) +
    geom_line(aes(y = .data[[actual_col]], group = 1, color = "Actual"), linewidth = 0.65) +
    geom_line(aes(y = .data[[fitted_col]], group = 1, color = "Reconstructed"), linewidth = 0.55, linetype = "dashed") +
    scale_x_discrete(breaks = breaks) +
    scale_color_manual(values = c(Actual = "black", Reconstructed = "#ba3b35")) +
    labs(title = paste(title_prefix, "actual vs reconstructed:", variable_name), x = NULL, y = NULL, color = NULL) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
  file <- file.path(out_dir, paste0(prefix, "_actual_vs_reconstructed_", safe_name(ifelse(is.null(country_name), variable_name, paste(country_name, variable_name))), ".png"))
  ggsave(file, p, width = 9.5, height = 4.8, dpi = 160)
}

plot_cumulative <- function(cum_dat, variable_name, out_dir, prefix, country_name = NULL, paper_name = NULL) {
  d <- cum_dat |> filter(variable == variable_name) |> arrange(yq_index)
  if (!is.null(country_name)) d <- d |> filter(Country == country_name)
  if (nrow(d) == 0L) return(invisible(NULL))
  d$Quarter_ID <- factor(d$Quarter_ID, levels = unique(d$Quarter_ID))
  long <- d |>
    select(Quarter_ID, cumulative_energy, cumulative_ciss, cumulative_inflationary_monetary, cumulative_sovereign, cumulative_other) |>
    pivot_longer(starts_with("cumulative_"), names_to = "component", values_to = "cumulative_contribution") |>
    mutate(component = recode(
      component,
      cumulative_energy = "Energy-carbon pressure shock",
      cumulative_ciss = "Systemic financial stress shock",
      cumulative_inflationary_monetary = "Inflationary monetary-reaction shock",
      cumulative_sovereign = "Sovereign-risk repricing shock",
      cumulative_other = "Other / unidentified structural shocks"
    ))
  breaks <- levels(d$Quarter_ID)[seq(1, length(levels(d$Quarter_ID)), by = 4)]
  title_prefix <- ifelse(is.null(country_name), "Panel average", country_name)
  p <- ggplot(long, aes(x = Quarter_ID, y = cumulative_contribution, color = component, group = component)) +
    geom_hline(yintercept = 0, color = "grey55", linewidth = 0.25) +
    geom_line(linewidth = 0.75) +
    scale_x_discrete(breaks = breaks) +
    labs(title = paste(title_prefix, "cumulative HD:", variable_name), x = NULL, y = "Cumulative contribution", color = NULL) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
  file <- file.path(out_dir, paste0(prefix, "_cumulative_", safe_name(ifelse(is.null(country_name), variable_name, paste(country_name, variable_name))), ".png"))
  ggsave(file, p, width = 10.5, height = 5.8, dpi = 160)
  if (!is.null(paper_name)) ggsave(file.path(FIGURE_DIR, "paper_figures", paper_name), p, width = 10.5, height = 5.8, dpi = 180)
}

plot_country_ranking <- function(ranking, title, file_name) {
  dat <- ranking |> filter(period == "Energy-inflation shock period") |> arrange(cumulative_contribution)
  p <- ggplot(dat, aes(x = reorder(Country, cumulative_contribution), y = cumulative_contribution)) +
    geom_col(fill = "#2f6f88", width = 0.72) +
    coord_flip() +
    labs(title = title, x = NULL, y = "Cumulative contribution to dlog_CDS") +
    theme_minimal(base_size = 10)
  ggsave(file.path(FIGURE_DIR, "paper_figures", file_name), p, width = 8.5, height = 5.5, dpi = 180)
  ggsave(file.path(FIGURE_DIR, "hd_dlog_CDS_focus", file_name), p, width = 8.5, height = 5.5, dpi = 160)
}

create_figures <- function(panel_average, country_hd, cumulative, rankings) {
  for (v in MODEL_VARS) {
    paper_file <- switch(
      v,
      dlog_CDS = "paper_hd_panel_average_dlog_CDS.png",
      d_CPI = "paper_hd_panel_average_d_CPI.png",
      d_3MRate = "paper_hd_panel_average_d_3MRate.png",
      GDP_Growth = "paper_hd_panel_average_GDP_Growth.png",
      d_FiscalBalanceGDP = "paper_hd_panel_average_d_FiscalBalanceGDP.png",
      NULL
    )
    plot_contribution_stack(panel_average, v, file.path(FIGURE_DIR, "hd_panel_average"), "panel", "actual_panel_average", "fitted_reconstructed_panel_average", paper_name = paper_file)
    plot_actual_vs_reconstructed(panel_average, v, file.path(FIGURE_DIR, "hd_panel_average"), "panel", "actual_panel_average", "fitted_reconstructed_panel_average")
    paper_cumulative_file <- NULL
    if (v == "dlog_CDS") paper_cumulative_file <- "paper_hd_cumulative_dlog_CDS.png"
    plot_cumulative(cumulative$panel_cumulative, v, file.path(FIGURE_DIR, "hd_cumulative"), "panel", paper_name = paper_cumulative_file)
  }

  for (country in unique(country_hd$Country)) {
    plot_contribution_stack(country_hd, "dlog_CDS", file.path(FIGURE_DIR, "hd_country_level"), "country", "actual", "fitted_reconstructed", country)
    plot_actual_vs_reconstructed(country_hd, "dlog_CDS", file.path(FIGURE_DIR, "hd_country_level"), "country", "actual", "fitted_reconstructed", country)
    plot_cumulative(cumulative$country_cumulative, "dlog_CDS", file.path(FIGURE_DIR, "hd_country_level"), "country", country)
  }

  selected_countries <- intersect(c("Romania", "Poland", "Hungary", "Czech Republic", "Bulgaria"), unique(country_hd$Country))
  for (country in selected_countries) {
    for (v in setdiff(MODEL_VARS, "dlog_CDS")) {
      plot_contribution_stack(country_hd, v, file.path(FIGURE_DIR, "hd_country_level"), "selected_country", "actual", "fitted_reconstructed", country)
      plot_actual_vs_reconstructed(country_hd, v, file.path(FIGURE_DIR, "hd_country_level"), "selected_country", "actual", "fitted_reconstructed", country)
      plot_cumulative(cumulative$country_cumulative, v, file.path(FIGURE_DIR, "hd_country_level"), "selected_country", country)
    }
  }

  plot_country_ranking(rankings$energy, "Energy contribution to cumulative dlog_CDS, 2021Q1-2023Q4", "paper_hd_country_comparison_dlog_CDS_energy_contribution.png")
  plot_country_ranking(rankings$sovereign, "Sovereign-risk contribution to cumulative dlog_CDS, 2021Q1-2023Q4", "paper_hd_country_comparison_dlog_CDS_sovereign_contribution.png")
}

write_reports <- function(rf, structural, residual_checks, hd_checks, panel_periods, country_periods, rankings, main_findings) {
  stable <- rf$numerical_checks$value[rf$numerical_checks$check == "reduced_form_stable_from_workbook"][[1]]
  max_mod <- rf$numerical_checks$value[rf$numerical_checks$check == "max_modulus_recomputed"][[1]]
  sigma_pd <- rf$numerical_checks$value[rf$numerical_checks$check == "Sigma_u_positive_definite"][[1]]
  min_eig <- rf$numerical_checks$value[rf$numerical_checks$check == "Sigma_u_min_eigenvalue"][[1]]
  max_hd_error <- hd_checks$max_abs_error[hd_checks$level == "overall"][[1]]
  max_u_error <- residual_checks$checks_total$value[residual_checks$checks_total$check == "max_abs_error"][[1]]

  full_cds <- panel_periods |> filter(period == "Full sample", variable == "dlog_CDS")
  crisis_cds <- panel_periods |> filter(period == "Energy-inflation shock period", variable == "dlog_CDS")
  crisis_cpi <- panel_periods |> filter(period == "Energy-inflation shock period", variable == "d_CPI")
  crisis_rate <- panel_periods |> filter(period == "Energy-inflation shock period", variable == "d_3MRate")
  other_large <- full_cds$share_abs_other[[1]] > 0.35
  ready <- max_hd_error < 1e-8 && max_u_error < 1e-8

  top_rank <- function(x) paste(x$Country[seq_len(min(3, nrow(x)))], collapse = ", ")
  top_energy <- rankings$energy |> filter(period == "Energy-inflation shock period") |> arrange(desc(cumulative_contribution))
  top_ciss <- rankings$ciss |> filter(period == "Energy-inflation shock period") |> arrange(desc(cumulative_contribution))
  top_sov <- rankings$sovereign |> filter(period == "Energy-inflation shock period") |> arrange(desc(cumulative_contribution))

  lines <- c(
    "# Historical Decomposition Report - refined4 final model",
    "",
    "## Model Used",
    paste0("Structural model: ", MODEL_LABEL, "."),
    "This stage uses the refined4 structural output, not repaired4. No counterfactual analysis was run.",
    paste0("Variables: ", paste(MODEL_VARS, collapse = ", "), "."),
    paste0("Shocks: ", paste(SHOCKS$shock, collapse = "; "), "; Other / unidentified structural shocks."),
    "",
    "## Reduced-Form Checks",
    paste0("Stable: ", stable, ". Max modulus: ", max_mod, "."),
    paste0("Sigma_u positive definite: ", sigma_pd, ". Min eigenvalue: ", min_eig, "."),
    paste0("Sample in reduced-form workbook: ", rf$sample_summary$countries[[1]], " countries, ", rf$sample_summary$quarters[[1]], " quarters, ", rf$sample_summary$observations[[1]], " observations."),
    "Historical decomposition uses residual-bearing PVAR observations, so the effective HD sample starts after the first lag.",
    "",
    "## Structural Matrix Choice",
    "The accepted rotations were not stored in the previous refined4 workbook. Therefore S1 was rerun with the original seed and restrictions, then the accepted draw closest to the saved median IRF was selected.",
    paste(capture.output(print(structural$representative)), collapse = "\n"),
    paste(capture.output(print(structural$acceptance)), collapse = "\n"),
    "The representative B matrix is saved in `01_hd_model_setup.xlsx`.",
    "",
    "## Reconstruction Accuracy",
    paste0("Reduced-form residual reconstruction max absolute error: ", format(max_u_error, scientific = TRUE, digits = 4), "."),
    paste0("Historical decomposition reconstruction max absolute error: ", format(max_hd_error, scientific = TRUE, digits = 4), "."),
    "",
    "## Period Findings",
    paste(capture.output(print(main_findings)), collapse = "\n"),
    "",
    "## Limitations",
    "Historical decomposition is conditional on one representative accepted draw. It is not a full posterior distribution over all accepted rotations.",
    "The initial/deterministic component absorbs country fixed effects, lagged initial conditions and the deterministic fitted path not attributed to structural innovations.",
    "Other / unidentified structural shocks aggregate the three structural columns not labelled by the four refined4 shocks.",
    "",
    "## Final Answers",
    paste0("1. Modelul refined4 folosit este stabil? ", stable, "."),
    paste0("2. Matricea structurala folosita pentru HD: representative accepted B draw from refined4 S1, saved in `01_hd_model_setup.xlsx`."),
    paste0("3. Representative draw: candidate draw ", structural$representative$candidate_draw[[1]], ", accepted draw ", structural$representative$accepted_draw[[1]], ", closest to saved median IRF by standardized squared distance."),
    paste0("4. Socurile structurale reconstruiesc reziduurile reduced-form? ", max_u_error < 1e-8, "."),
    paste0("5. Decompozitia istorica reconstruieste seriile fitted/actual plus initial component? ", max_hd_error < 1e-8, "."),
    paste0("6. Eroarea maxima de reconstructie: ", format(max_hd_error, scientific = TRUE, digits = 4), "."),
    paste0("7. Cel mai mare contributor mediu absolut la dlog_CDS full sample: ", main_findings$answer[main_findings$question == "Largest mean absolute contributor to dlog_CDS, full sample"], "."),
    paste0("8. Cel mai mare contributor cumulativ absolut la dlog_CDS in 2021Q1-2023Q4: ", main_findings$answer[main_findings$question == "Largest cumulative absolute contributor to dlog_CDS, 2021Q1-2023Q4"], "."),
    paste0("9. Cel mai mare contributor la d_CPI in 2021Q1-2023Q4: ", main_findings$answer[main_findings$question == "Largest absolute contributor to d_CPI, 2021Q1-2023Q4"], "."),
    paste0("10. Cel mai mare contributor la d_3MRate in 2021Q1-2023Q4: ", main_findings$answer[main_findings$question == "Largest absolute contributor to d_3MRate, 2021Q1-2023Q4"], "."),
    paste0("11. Tari cu cea mai mare contributie cumulativa energy la dlog_CDS: ", top_rank(top_energy), "."),
    paste0("12. Tari cu cea mai mare contributie cumulativa CISS la dlog_CDS: ", top_rank(top_ciss), "."),
    paste0("13. Tari cu cea mai mare contributie cumulativa sovereign-risk la dlog_CDS: ", top_rank(top_sov), "."),
    paste0("14. Componenta Other / unidentified este mare sau acceptabila? ", ifelse(other_large, "Mare", "Moderata/acceptabila"), "."),
    paste0("15. Decompozitia sustine narativul energy-inflation-sovereign risk? ", ifelse(ready, "Partial, conditional on refined4 identification", "Nu, reconstructia nu este suficienta"), "."),
    "16. Rezultate care contrazic narativul: vezi componentele Other si rankingurile pe tari; daca Other domina o variabila/perioada, interpretarea trebuie temperata.",
    "17. Rezultate suficient de clare pentru paper: panel-average HD, cumulative dlog_CDS si rankingurile pe tari pentru perioada 2021Q1-2023Q4.",
    "18. Rezultate pentru appendix: toate decompozitiile country-level non-CDS si tabelele complete pe variabile.",
    paste0("19. HD este suficient de stabila pentru etapa counterfactual? ", ready, "."),
    paste0("20. Recomandare finala: ", ifelse(ready, "Se poate trece la counterfactual analysis in etapa urmatoare, fara a schimba modelul.", "Nu trece la counterfactual pana nu este reparata reconstructia.")),
    ""
  )
  writeLines(lines, file.path(OUTPUT_DIR, "summary_report_historical_decomposition.md"))

  paper_lines <- c(
    "# Historical Decomposition Interpretation for Paper",
    "",
    "## 1. Historical Decomposition Methodology",
    "The decomposition uses the refined4 sign-restricted structural PVAR, with a representative accepted draw selected to approximate the median structural IRF profile. The exercise attributes model-implied movements to four labelled shocks and an aggregate Other/unidentified component.",
    "",
    "## 2. Aggregate CEE Results",
    "The panel-average tables show the contribution of energy-carbon pressure, systemic financial stress, inflationary monetary-reaction, sovereign-risk repricing and Other shocks for all seven variables.",
    "",
    "## 3. Energy-Carbon Contribution",
    paste0("For d_CPI in the energy-inflation period, the dominant absolute contributor is ", main_findings$answer[main_findings$question == "Largest absolute contributor to d_CPI, 2021Q1-2023Q4"], ". Interpret this as a model-implied contribution, not a complete causal explanation."),
    "",
    "## 4. Systemic Financial Stress Contribution",
    "The systemic stress contribution is most informative for GDP_Growth, d_CISS and dlog_CDS, but cross-country heterogeneity should be checked before using it as a broad CEE claim.",
    "",
    "## 5. Inflationary Monetary-Reaction Contribution",
    paste0("For d_3MRate in the energy-inflation period, the dominant absolute contributor is ", main_findings$answer[main_findings$question == "Largest absolute contributor to d_3MRate, 2021Q1-2023Q4"], "."),
    "",
    "## 6. Sovereign-Risk Repricing Contribution",
    "The sovereign-risk repricing shock is the own CDS repricing component left after the labelled energy, stress and inflation-rate shocks. It should not be interpreted as an external independent factor.",
    "",
    "## 7. Cross-Country Heterogeneity",
    paste0("Top countries by energy contribution to cumulative dlog_CDS in 2021Q1-2023Q4: ", top_rank(top_energy), "."),
    paste0("Top countries by sovereign-risk contribution to cumulative dlog_CDS in 2021Q1-2023Q4: ", top_rank(top_sov), "."),
    "",
    "## 8. Cumulative CDS Effects",
    "For dlog_CDS, cumulative log contributions are also converted to approximate percent effects using exp(cumulative contribution)-1. These are reported for both panel average and countries.",
    "",
    "## 9. Limitations",
    paste0("The Other/unidentified share for full-sample dlog_CDS is ", round(100 * full_cds$share_abs_other[[1]], 2), "%, so conclusions must acknowledge unlabelled structural variation."),
    "The decomposition is representative-draw based and should be treated as a deterministic decomposition conditional on refined4, not as uncertainty bands.",
    "",
    "## 10. Implications for the Next Counterfactual Stage",
    ifelse(ready, "The reconstruction checks are numerically sound, so counterfactual analysis can be run next using the same refined4 representative B matrix.", "Counterfactual analysis should wait until reconstruction issues are fixed.")
  )
  writeLines(paper_lines, file.path(OUTPUT_DIR, "historical_decomposition_interpretation_for_paper.md"))
}

main <- function() {
  cat("Reading reduced-form and refined4 inputs...\n")
  rf <- read_reduced_form()

  cat("Selecting refined4 S1 representative structural matrix...\n")
  structural <- select_representative_B(rf$A, rf$P)

  cat("Recovering structural shocks from FE/LSDV residuals...\n")
  residuals_wide <- read_hd_data()
  residual_checks <- recover_structural_shocks(residuals_wide, structural$B)

  cat("Computing historical decomposition...\n")
  country_hd <- compute_hd(rf$A, structural$B, residuals_wide, residual_checks$eps)
  panel_average <- make_panel_average(country_hd)
  cumulative <- make_cumulative(country_hd, panel_average)
  hd_checks <- make_reconstruction_checks(country_hd)
  panel_periods <- summarise_periods(panel_average, "actual_panel_average", "fitted_reconstructed_panel_average")
  country_periods <- summarise_periods(country_hd, "actual", "fitted_reconstructed", group_cols = "Country")
  rankings <- make_rankings(country_hd)
  main_findings <- make_main_findings(panel_periods, rankings, hd_checks)

  shock_stats <- make_shock_stats(residual_checks$shocks)
  shock_corr <- as.data.frame(cor(residual_checks$shocks[, SHOCKS$shock], use = "pairwise.complete.obs"), check.names = FALSE) |>
    tibble::rownames_to_column("shock")

  cat("Creating figures...\n")
  create_figures(panel_average, country_hd, cumulative, rankings)

  cat("Writing Excel outputs...\n")
  setup_notes <- data.frame(
    item = c(
      "model_name",
      "structural_input_folder",
      "reduced_form_input_folder",
      "historical_decomposition_effective_sample",
      "structural_matrix_choice",
      "initial_component_note",
      "counterfactual_analysis"
    ),
    value = c(
      MODEL_LABEL,
      STRUCTURAL_DIR,
      INPUT_DIR,
      paste(min(country_hd$Quarter_ID), "to", max(country_hd$Quarter_ID)),
      "Representative accepted refined4 S1 draw closest to saved median IRF.",
      "Initial/deterministic component absorbs fixed effects, initial conditions and deterministic fitted path not assigned to shocks.",
      "Not run in this stage."
    ),
    stringsAsFactors = FALSE
  )

  write_workbook(
    list(
      model_info = setup_notes,
      sample_summary = rf$sample_summary,
      countries = rf$countries,
      variable_order = data.frame(order = seq_along(MODEL_VARS), variable = MODEL_VARS),
      A1_matrix = matrix_to_sheet(rf$A, "response"),
      Sigma_u = matrix_to_sheet(rf$Sigma, "response"),
      structural_B_matrix = matrix_to_sheet(structural$B, "response"),
      representative_draw = structural$representative,
      acceptance_reproduction = structural$acceptance,
      sign_restriction_validation = structural$validation,
      reduced_form_checks = rf$numerical_checks,
      residual_reconstruction = residual_checks$checks_total,
      hd_reconstruction = hd_checks,
      methodology_notes = setup_notes
    ),
    file.path(OUTPUT_DIR, "01_hd_model_setup.xlsx")
  )

  write_workbook(
    list(
      structural_shocks = residual_checks$shocks,
      shock_summary_statistics = shock_stats,
      identified_shock_correlations = shock_corr,
      residual_reconstruction_total = residual_checks$checks_total,
      residual_reconstruction_by_var = residual_checks$checks_by_variable,
      residual_reconstruction_by_cty = residual_checks$checks_by_country
    ),
    file.path(OUTPUT_DIR, "02_hd_structural_shocks.xlsx")
  )

  panel_sheets <- setNames(lapply(MODEL_VARS, function(v) panel_average |> filter(variable == v)), MODEL_VARS)
  panel_sheets$reconstruction_checks <- hd_checks
  panel_sheets$summary_by_period <- panel_periods
  write_workbook(panel_sheets, file.path(OUTPUT_DIR, "03_hd_panel_average.xlsx"))

  write_workbook(
    list(country_level_long = country_hd),
    file.path(OUTPUT_DIR, "04_hd_country_level.xlsx")
  )

  cumulative_period_summary <- bind_rows(
    panel_periods |> mutate(level = "panel_average", Country = "Panel average", .before = 1),
    country_periods |> mutate(level = "country", .before = 1)
  )
  write_workbook(
    list(
      panel_average_cumulative = cumulative$panel_cumulative,
      country_level_cumulative = cumulative$country_cumulative,
      dlog_CDS_percent_effects = cumulative$dlog_CDS_percent_effects,
      summary_cumulative_by_period = cumulative_period_summary
    ),
    file.path(OUTPUT_DIR, "05_hd_cumulative.xlsx")
  )

  sheet_name_mapping <- data.frame(
    requested = c(
      "hd_average_contribution_by_period",
      "hd_cumulative_contribution_by_period",
      "country_ranking_dlog_CDS_energy",
      "country_ranking_dlog_CDS_ciss",
      "country_ranking_dlog_CDS_inflation",
      "country_ranking_dlog_CDS_sovereign"
    ),
    actual_sheet = c(
      "hd_avg_contrib_by_period",
      "hd_cum_contrib_by_period",
      "rank_dlog_CDS_energy",
      "rank_dlog_CDS_ciss",
      "rank_dlog_CDS_inflation",
      "rank_dlog_CDS_sovereign"
    ),
    note = "Excel sheet names are limited to 31 characters.",
    stringsAsFactors = FALSE
  )

  write_workbook(
    list(
      sheet_name_mapping = sheet_name_mapping,
      hd_avg_contrib_by_period = panel_periods,
      hd_cum_contrib_by_period = cumulative_period_summary,
      hd_dlog_CDS_summary = panel_periods |> filter(variable == "dlog_CDS"),
      hd_d_CPI_summary = panel_periods |> filter(variable == "d_CPI"),
      hd_d_3MRate_summary = panel_periods |> filter(variable == "d_3MRate"),
      hd_GDP_Growth_summary = panel_periods |> filter(variable == "GDP_Growth"),
      hd_fiscal_summary = panel_periods |> filter(variable == "d_FiscalBalanceGDP"),
      rank_dlog_CDS_energy = rankings$energy,
      rank_dlog_CDS_ciss = rankings$ciss,
      rank_dlog_CDS_inflation = rankings$inflation,
      rank_dlog_CDS_sovereign = rankings$sovereign,
      main_findings = main_findings
    ),
    file.path(OUTPUT_DIR, "06_hd_summary_tables_for_paper.xlsx")
  )

  cat("Writing Markdown reports...\n")
  write_reports(rf, structural, residual_checks, hd_checks, panel_periods, country_periods, rankings, main_findings)

  cat("Historical decomposition complete.\n")
  cat("Output directory:", normalizePath(OUTPUT_DIR, winslash = "/"), "\n")
  print(structural$acceptance)
  print(hd_checks |> filter(level == "overall"))
  print(main_findings)
}

main()
