# Structural PVAR refined baseline with four identified shocks.
# Uses the final FE/LSDV PVAR(1) reduced-form output and does not run
# historical decomposition or counterfactual analysis.

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
BASELINE3_DIR <- "structural_pvar_ciss_full7_structural_baseline_outputs"
FE_WORKBOOK <- file.path(INPUT_DIR, "04_fe_lsdv_pvar1_full7_final.xlsx")
DATA_WORKBOOK <- file.path(INPUT_DIR, "01_data_preparation_full7_final.xlsx")
OUTPUT_DIR <- "structural_pvar_ciss_full7_structural_refined4_outputs"
FIGURE_DIR <- file.path(OUTPUT_DIR, "figures")
FIGURE_SUBDIRS <- c(
  "structural_irf_all",
  "structural_irf_key",
  "structural_fevd",
  "acceptance_diagnostics",
  "comparison_with_baseline3",
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
  interpretation = c(
    "Common increase in energy-carbon price pressure.",
    "Increase in European systemic financial stress.",
    "Inflationary pressure accompanied by a positive short-rate reaction.",
    "Own sovereign-risk repricing component captured by a CDS increase."
  ),
  stringsAsFactors = FALSE
)

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
  rationale = c(
    "Defines the common energy-carbon pressure impulse.",
    "Energy-carbon pressure should raise quarterly inflation pressure.",
    "Inflation pressure should be accompanied by a positive short-rate reaction.",
    "Defines systemic financial stress.",
    "Financial stress should reduce real activity.",
    "Defines the inflationary component.",
    "The inflationary shock is accompanied by a positive short-rate reaction.",
    "Defines sovereign-risk repricing through a CDS increase."
  ),
  stringsAsFactors = FALSE
) |>
  left_join(SHOCKS[, c("shock_key", "shock")], by = "shock_key") |>
  select(shock_key, shock, variable, restriction, rationale)

NEAR_ZERO_RESTRICTIONS <- data.frame(
  shock_key = c("inflationary_reaction", "sovereign_repricing"),
  variable = c("Energy_Factor", "d_CISS"),
  restriction = c("near_zero", "near_zero"),
  rationale = c(
    "Separates the inflationary monetary-reaction shock from an impact energy shock.",
    "Separates sovereign-risk repricing from an impact systemic stress shock."
  ),
  stringsAsFactors = FALSE
) |>
  left_join(SHOCKS[, c("shock_key", "shock")], by = "shock_key") |>
  select(shock_key, shock, variable, restriction, rationale)

KEY_IRF_REQUESTS <- data.frame(
  shock = c(
    rep("Energy-carbon pressure shock", 5),
    rep("Systemic financial stress shock", 5),
    rep("Inflationary monetary-reaction shock", 5),
    rep("Sovereign-risk repricing shock", 6)
  ),
  response = c(
    "d_CPI", "d_3MRate", "dlog_CDS", "GDP_Growth", "d_FiscalBalanceGDP",
    "d_CISS", "GDP_Growth", "dlog_CDS", "d_CPI", "d_3MRate",
    "d_CPI", "d_3MRate", "dlog_CDS", "Energy_Factor", "GDP_Growth",
    "dlog_CDS", "GDP_Growth", "d_CISS", "d_CPI", "d_3MRate", "d_FiscalBalanceGDP"
  ),
  stringsAsFactors = FALSE
)

HORIZON <- 12L
FEVD_HORIZONS <- c(1L, 2L, 4L, 8L, 12L)
TARGET_MIN_ACCEPTED <- 1000L
TARGET_PREFERRED_ACCEPTED <- 2000L
SIGN_TOL <- 1e-10
NEAR_ZERO_PILOT_DRAWS <- as.integer(Sys.getenv("REFINED4_NEAR_ZERO_PILOT_DRAWS", "10000"))
NEAR_ZERO_TOLERANCE_FRACTION <- as.numeric(Sys.getenv("REFINED4_NEAR_ZERO_TOL_FRACTION", "0.10"))
DEFAULT_DRAWS <- as.integer(Sys.getenv("REFINED4_DRAWS", "50000"))
EXTENDED_DRAWS <- as.integer(Sys.getenv("REFINED4_EXTENDED_DRAWS", "100000"))
SEEDS <- c(S1 = 20260631L, S2 = 20260632L, S3 = 20260633L, pilot = 20260630L)

VARIANTS <- list(
  S1 = list(
    model_variant = "S1_four_shock_sign_only_h0_h2",
    model_label = "S1 - four-shock sign-only refined baseline h=0..2",
    restriction_horizons = 0:2,
    use_near_zero = FALSE,
    n_draws = DEFAULT_DRAWS,
    seed = SEEDS[["S1"]]
  ),
  S2 = list(
    model_variant = "S2_four_shock_sign_only_h0_h1",
    model_label = "S2 - four-shock sign-only sensitivity h=0..1",
    restriction_horizons = 0:1,
    use_near_zero = FALSE,
    n_draws = DEFAULT_DRAWS,
    seed = SEEDS[["S2"]]
  ),
  S3 = list(
    model_variant = "S3_four_shock_near_zero_h0_h2",
    model_label = "S3 - optional separated-shocks near-zero h=0..2",
    restriction_horizons = 0:2,
    use_near_zero = TRUE,
    n_draws = DEFAULT_DRAWS,
    seed = SEEDS[["S3"]]
  )
)

make_assignment_grid <- function(k, n_shocks) {
  grid <- do.call(expand.grid, c(rep(list(seq_len(k)), n_shocks), list(KEEP.OUT.ATTRS = FALSE)))
  grid <- as.matrix(grid)
  grid[apply(grid, 1, function(x) length(unique(as.integer(x))) == length(x)), , drop = FALSE]
}

ASSIGNMENT_GRID <- make_assignment_grid(length(MODEL_VARS), nrow(SHOCKS))

dir.create(OUTPUT_DIR, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)
for (subdir in FIGURE_SUBDIRS) {
  dir.create(file.path(FIGURE_DIR, subdir), recursive = TRUE, showWarnings = FALSE)
}

safe_name <- function(x) {
  out <- gsub("[^A-Za-z0-9_]+", "_", x)
  out <- gsub("_+", "_", out)
  gsub("^_|_$", "", out)
}

write_workbook <- function(sheets, path) {
  openxlsx::write.xlsx(sheets, path, overwrite = TRUE)
}

matrix_to_sheet <- function(mat, row_name = "row") {
  as.data.frame(mat, check.names = FALSE) |>
    tibble::rownames_to_column(row_name)
}

read_reduced_form <- function() {
  if (!file.exists(FE_WORKBOOK)) stop("Missing FE workbook: ", FE_WORKBOOK)
  if (!file.exists(DATA_WORKBOOK)) stop("Missing data workbook: ", DATA_WORKBOOK)
  A_df <- openxlsx::read.xlsx(FE_WORKBOOK, sheet = "A1_matrix")
  Sigma_df <- openxlsx::read.xlsx(FE_WORKBOOK, sheet = "residual_covariance")
  stability <- openxlsx::read.xlsx(FE_WORKBOOK, sheet = "stability_summary")
  sample_summary <- openxlsx::read.xlsx(DATA_WORKBOOK, sheet = "estimation_sample")

  A <- as.matrix(A_df[, MODEL_VARS])
  Sigma <- as.matrix(Sigma_df[, MODEL_VARS])
  storage.mode(A) <- "double"
  storage.mode(Sigma) <- "double"
  rownames(A) <- colnames(A) <- MODEL_VARS
  rownames(Sigma) <- colnames(Sigma) <- MODEL_VARS
  Sigma_sym <- (Sigma + t(Sigma)) / 2

  eigen_A <- eigen(A)$values
  eigen_Sigma <- eigen(Sigma_sym, symmetric = TRUE)$values
  cholesky_problem <- NULL
  P <- tryCatch(t(chol(Sigma_sym)), error = function(e) {
    cholesky_problem <<- conditionMessage(e)
    NULL
  })
  if (is.null(P)) stop("Sigma_u cannot be factorized by Cholesky: ", cholesky_problem)
  rownames(P) <- MODEL_VARS
  colnames(P) <- paste0("structural_col_", seq_along(MODEL_VARS))

  list(
    A = A,
    Sigma = Sigma_sym,
    P = P,
    eigen_A = eigen_A,
    eigen_Sigma = eigen_Sigma,
    stability = stability,
    sample_summary = sample_summary,
    numerical_checks = data.frame(
      check = c(
        "reduced_form_max_modulus_from_workbook",
        "reduced_form_stable_from_workbook",
        "max_modulus_recomputed",
        "Sigma_u_symmetric_after_processing",
        "Sigma_u_positive_definite",
        "Sigma_u_min_eigenvalue",
        "Sigma_u_max_eigenvalue",
        "Cholesky_factorization",
        "Cholesky_reconstruction_max_abs_error"
      ),
      value = c(
        as.character(stability$max_modulus[[1]]),
        as.character(stability$stable[[1]]),
        as.character(max(Mod(eigen_A))),
        as.character(isTRUE(all.equal(Sigma_sym, t(Sigma_sym), tolerance = 1e-10))),
        as.character(all(eigen_Sigma > SIGN_TOL)),
        as.character(min(eigen_Sigma)),
        as.character(max(eigen_Sigma)),
        ifelse(is.null(cholesky_problem), "ok", cholesky_problem),
        as.character(max(abs(Sigma_sym - P %*% t(P))))
      ),
      stringsAsFactors = FALSE
    )
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

compute_near_zero_tolerances <- function(P) {
  set.seed(SEEDS[["pilot"]])
  k <- nrow(P)
  impact_values <- matrix(NA_real_, nrow = NEAR_ZERO_PILOT_DRAWS * k, ncol = length(MODEL_VARS))
  colnames(impact_values) <- MODEL_VARS
  idx <- 1L
  for (draw in seq_len(NEAR_ZERO_PILOT_DRAWS)) {
    B <- P %*% random_orthogonal(k)
    impact_values[idx:(idx + k - 1L), ] <- t(B)
    idx <- idx + k
  }
  sd_impact <- apply(impact_values, 2, sd, na.rm = TRUE)
  data.frame(
    variable = names(sd_impact),
    pilot_draws = NEAR_ZERO_PILOT_DRAWS,
    candidate_impact_values = NEAR_ZERO_PILOT_DRAWS * k,
    impact_sd = as.numeric(sd_impact),
    tolerance_fraction = NEAR_ZERO_TOLERANCE_FRACTION,
    near_zero_tolerance = NEAR_ZERO_TOLERANCE_FRACTION * as.numeric(sd_impact),
    stringsAsFactors = FALSE
  )
}

make_restriction_grid <- function(config, near_zero_tolerances = NULL) {
  base <- expand.grid(
    shock_key = SHOCKS$shock_key,
    variable = MODEL_VARS,
    stringsAsFactors = FALSE
  ) |>
    left_join(SHOCKS, by = "shock_key") |>
    left_join(SIGN_RESTRICTIONS[, c("shock_key", "variable", "restriction", "rationale")], by = c("shock_key", "variable")) |>
    mutate(
      restriction = ifelse(is.na(restriction), "free", restriction),
      rationale = ifelse(is.na(rationale), "Left unrestricted in this variant.", rationale),
      horizons_imposed = ifelse(restriction == "free", "none", paste(config$restriction_horizons, collapse = ", ")),
      model_variant = config$model_variant,
      model_label = config$model_label,
      near_zero_tolerance = NA_real_
    )

  if (isTRUE(config$use_near_zero)) {
    nz <- NEAR_ZERO_RESTRICTIONS |>
      mutate(
        horizons_imposed = "0",
        model_variant = config$model_variant,
        model_label = config$model_label
      ) |>
      left_join(near_zero_tolerances[, c("variable", "near_zero_tolerance")], by = "variable")
    base <- bind_rows(
      base,
      nz |> select(model_variant, model_label, shock_key, shock, variable, restriction, horizons_imposed, rationale, near_zero_tolerance)
    )
  }

  base |>
    select(model_variant, model_label, shock, variable, restriction, horizons_imposed, near_zero_tolerance, rationale)
}

sign_restriction_passes <- function(irf_arr, shock_key, col_id, sign_flip, restriction_horizons) {
  rules <- SIGN_RESTRICTIONS[SIGN_RESTRICTIONS$shock_key == shock_key, , drop = FALSE]
  for (i in seq_len(nrow(rules))) {
    vals <- sign_flip * irf_arr[restriction_horizons + 1L, rules$variable[[i]], col_id]
    if (rules$restriction[[i]] == "positive" && !all(vals > SIGN_TOL, na.rm = TRUE)) return(FALSE)
    if (rules$restriction[[i]] == "negative" && !all(vals < -SIGN_TOL, na.rm = TRUE)) return(FALSE)
  }
  TRUE
}

near_zero_passes <- function(irf_arr, shock_key, col_id, near_zero_tolerances) {
  rules <- NEAR_ZERO_RESTRICTIONS[NEAR_ZERO_RESTRICTIONS$shock_key == shock_key, , drop = FALSE]
  if (nrow(rules) == 0L) return(TRUE)
  for (i in seq_len(nrow(rules))) {
    tol <- near_zero_tolerances$near_zero_tolerance[near_zero_tolerances$variable == rules$variable[[i]]][[1]]
    if (!is.finite(tol) || tol <= 0) return(FALSE)
    if (abs(irf_arr[1L, rules$variable[[i]], col_id]) > tol) return(FALSE)
  }
  TRUE
}

restriction_score <- function(irf_arr, shock_key, col_id, sign_flip) {
  rules <- SIGN_RESTRICTIONS[SIGN_RESTRICTIONS$shock_key == shock_key, , drop = FALSE]
  vals <- sign_flip * irf_arr[1L, rules$variable, col_id]
  sum(abs(vals), na.rm = TRUE)
}

build_valid_matrix <- function(irf_arr, config, near_zero_tolerances) {
  n_shocks <- nrow(SHOCKS)
  k <- length(MODEL_VARS)
  valid_sign <- matrix(0L, nrow = n_shocks, ncol = k, dimnames = list(SHOCKS$shock_key, paste0("col", seq_len(k))))
  valid_full <- valid_sign
  scores <- matrix(NA_real_, nrow = n_shocks, ncol = k, dimnames = list(SHOCKS$shock_key, paste0("col", seq_len(k))))

  for (s in seq_len(n_shocks)) {
    shock_key <- SHOCKS$shock_key[[s]]
    for (j in seq_len(k)) {
      pass_pos_sign <- sign_restriction_passes(irf_arr, shock_key, j, 1, config$restriction_horizons)
      pass_neg_sign <- sign_restriction_passes(irf_arr, shock_key, j, -1, config$restriction_horizons)
      if (pass_pos_sign || pass_neg_sign) {
        if (pass_pos_sign && pass_neg_sign) {
          score_pos <- restriction_score(irf_arr, shock_key, j, 1)
          score_neg <- restriction_score(irf_arr, shock_key, j, -1)
          valid_sign[s, j] <- ifelse(score_pos >= score_neg, 1L, -1L)
          scores[s, j] <- max(score_pos, score_neg)
        } else if (pass_pos_sign) {
          valid_sign[s, j] <- 1L
          scores[s, j] <- restriction_score(irf_arr, shock_key, j, 1)
        } else {
          valid_sign[s, j] <- -1L
          scores[s, j] <- restriction_score(irf_arr, shock_key, j, -1)
        }
      }

      if (valid_sign[s, j] != 0L) {
        if (!isTRUE(config$use_near_zero) || near_zero_passes(irf_arr, shock_key, j, near_zero_tolerances)) {
          valid_full[s, j] <- valid_sign[s, j]
        }
      }
    }
  }
  list(valid_sign = valid_sign, valid_full = valid_full, scores = scores)
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

update_overlap_counts <- function(valid, overlap_counts) {
  for (i in seq_len(nrow(valid))) {
    for (j in seq_len(nrow(valid))) {
      if (i == j) next
      overlap_counts[i, j] <- overlap_counts[i, j] + as.integer(any(valid[i, ] != 0L & valid[j, ] != 0L))
    }
  }
  overlap_counts
}

fevd_for_draw <- function(irf_arr, assigned_cols, horizons) {
  k <- length(MODEL_VARS)
  n_groups <- nrow(SHOCKS) + 1L
  out <- array(
    NA_real_,
    dim = c(length(horizons), k, n_groups),
    dimnames = list(
      horizon = as.character(horizons),
      response = MODEL_VARS,
      shock = c(SHOCKS$shock, "Other / unidentified structural shocks")
    )
  )
  for (hh in seq_along(horizons)) {
    idx <- seq_len(horizons[[hh]])
    for (r in seq_len(k)) {
      contributions <- colSums(irf_arr[idx, r, , drop = FALSE]^2)
      denom <- sum(contributions)
      if (!is.finite(denom) || denom <= 0) next
      for (s in seq_len(nrow(SHOCKS))) out[hh, r, s] <- contributions[assigned_cols[[s]]] / denom
      out[hh, r, n_groups] <- sum(contributions[setdiff(seq_len(k), assigned_cols)]) / denom
    }
  }
  out
}

summarise_values <- function(x) {
  if (length(x) == 0L || all(is.na(x))) {
    return(c(mean = NA_real_, median = NA_real_, p16 = NA_real_, p84 = NA_real_, p2_5 = NA_real_, p97_5 = NA_real_))
  }
  c(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    p16 = as.numeric(quantile(x, 0.16, na.rm = TRUE, names = FALSE)),
    p84 = as.numeric(quantile(x, 0.84, na.rm = TRUE, names = FALSE)),
    p2_5 = as.numeric(quantile(x, 0.025, na.rm = TRUE, names = FALSE)),
    p97_5 = as.numeric(quantile(x, 0.975, na.rm = TRUE, names = FALSE))
  )
}

run_identification <- function(A, P, config, near_zero_tolerances) {
  set.seed(config$seed)
  k <- length(MODEL_VARS)
  n_shocks <- nrow(SHOCKS)
  A_powers <- precompute_powers(A, HORIZON)
  max_restriction_horizon <- max(config$restriction_horizons)
  A_restriction_powers <- precompute_powers(A, max_restriction_horizon)
  n_draws <- config$n_draws

  irf_draws <- array(
    NA_real_,
    dim = c(n_draws, HORIZON + 1L, k, n_shocks),
    dimnames = list(draw = NULL, horizon = as.character(0:HORIZON), response = MODEL_VARS, shock = SHOCKS$shock)
  )
  fevd_draws <- array(
    NA_real_,
    dim = c(n_draws, length(FEVD_HORIZONS), k, n_shocks + 1L),
    dimnames = list(draw = NULL, horizon = as.character(FEVD_HORIZONS), response = MODEL_VARS, shock = c(SHOCKS$shock, "Other / unidentified structural shocks"))
  )
  restrictions_for_impact <- bind_rows(
    SIGN_RESTRICTIONS |> mutate(restriction_type = restriction),
    if (isTRUE(config$use_near_zero)) NEAR_ZERO_RESTRICTIONS |> mutate(restriction_type = restriction) else data.frame()
  )
  impact_draws <- matrix(NA_real_, nrow = n_draws, ncol = nrow(restrictions_for_impact))
  colnames(impact_draws) <- paste(restrictions_for_impact$shock_key, restrictions_for_impact$variable, restrictions_for_impact$restriction_type, sep = "::")

  accepted <- 0L
  rejected_no_valid <- 0L
  rejected_non_distinct <- 0L
  rejected_due_near_zero <- 0L
  sign_only_would_accept <- 0L
  candidate_multi_col_draws <- 0L
  accepted_multi_col_draws <- 0L
  candidate_multi_col_total <- 0L
  accepted_multi_col_total <- 0L
  unique_assignment_draws <- 0L
  sign_flip_counts <- setNames(rep(0L, n_shocks), SHOCKS$shock)
  sign_flip_any_count <- 0L
  assignments_count <- integer(n_draws)
  assignment_columns <- matrix(NA_integer_, nrow = n_draws, ncol = n_shocks, dimnames = list(NULL, SHOCKS$shock))
  assignment_signs <- matrix(NA_integer_, nrow = n_draws, ncol = n_shocks, dimnames = list(NULL, SHOCKS$shock))
  colnames(assignment_columns) <- paste0(safe_name(SHOCKS$shock), "_assigned_column")
  colnames(assignment_signs) <- paste0(safe_name(SHOCKS$shock), "_assigned_sign")
  candidate_overlap <- matrix(0L, nrow = n_shocks, ncol = n_shocks, dimnames = list(SHOCKS$shock, SHOCKS$shock))
  accepted_overlap <- candidate_overlap

  for (draw in seq_len(n_draws)) {
    Q <- random_orthogonal(k)
    B <- P %*% Q
    irf_restriction <- compute_irf_array(A_restriction_powers, B, max_restriction_horizon)
    vm <- build_valid_matrix(irf_restriction, config, near_zero_tolerances)
    candidate_overlap <- update_overlap_counts(vm$valid_full, candidate_overlap)
    multi_cols <- sum(colSums(vm$valid_full != 0L) > 1L)
    if (multi_cols > 0L) {
      candidate_multi_col_draws <- candidate_multi_col_draws + 1L
      candidate_multi_col_total <- candidate_multi_col_total + multi_cols
    }

    sign_only_assignment <- if (isTRUE(config$use_near_zero)) find_best_assignment(vm$valid_sign, vm$scores) else list(ok = FALSE)
    if (isTRUE(config$use_near_zero) && isTRUE(sign_only_assignment$ok)) sign_only_would_accept <- sign_only_would_accept + 1L
    assignment <- find_best_assignment(vm$valid_full, vm$scores)
    if (!assignment$ok) {
      if (isTRUE(config$use_near_zero) && isTRUE(sign_only_assignment$ok)) rejected_due_near_zero <- rejected_due_near_zero + 1L
      if (assignment$reason == "no_valid_assignment") rejected_no_valid <- rejected_no_valid + 1L
      if (assignment$reason == "non_distinct_columns") rejected_non_distinct <- rejected_non_distinct + 1L
      next
    }

    accepted <- accepted + 1L
    assignments_count[accepted] <- assignment$n_assignments
    if (assignment$n_assignments == 1L) unique_assignment_draws <- unique_assignment_draws + 1L
    if (multi_cols > 0L) {
      accepted_multi_col_draws <- accepted_multi_col_draws + 1L
      accepted_multi_col_total <- accepted_multi_col_total + multi_cols
    }
    accepted_overlap <- update_overlap_counts(vm$valid_full, accepted_overlap)
    assignment_columns[accepted, ] <- assignment$cols
    assignment_signs[accepted, ] <- assignment$signs
    if (any(assignment$signs == -1L)) sign_flip_any_count <- sign_flip_any_count + 1L
    sign_flip_counts <- sign_flip_counts + as.integer(assignment$signs == -1L)

    irf_full <- compute_irf_array(A_powers, B, HORIZON)
    for (s in seq_len(n_shocks)) {
      irf_draws[accepted, , , s] <- assignment$signs[[s]] * irf_full[, , assignment$cols[[s]]]
    }
    fevd_draws[accepted, , , ] <- fevd_for_draw(irf_full, assignment$cols, FEVD_HORIZONS)

    for (rr in seq_len(nrow(restrictions_for_impact))) {
      sid <- match(restrictions_for_impact$shock_key[[rr]], SHOCKS$shock_key)
      impact_draws[accepted, rr] <- irf_draws[accepted, 1L, restrictions_for_impact$variable[[rr]], sid]
    }

    if (draw %% 10000L == 0L) {
      cat(config$model_variant, "draw", draw, "accepted", accepted, "rate", round(accepted / draw, 4), "\n")
    }
  }

  if (accepted == 0L) stop("No accepted rotations for ", config$model_variant)

  irf_draws <- irf_draws[seq_len(accepted), , , , drop = FALSE]
  fevd_draws <- fevd_draws[seq_len(accepted), , , , drop = FALSE]
  impact_draws <- impact_draws[seq_len(accepted), , drop = FALSE]
  assignments_count <- assignments_count[seq_len(accepted)]
  assignment_columns <- assignment_columns[seq_len(accepted), , drop = FALSE]
  assignment_signs <- assignment_signs[seq_len(accepted), , drop = FALSE]

  acceptance_summary <- data.frame(
    model_variant = config$model_variant,
    model_label = config$model_label,
    restriction_horizons = paste(config$restriction_horizons, collapse = ", "),
    near_zero_used = isTRUE(config$use_near_zero),
    candidate_rotations = n_draws,
    accepted_rotations = accepted,
    acceptance_rate = accepted / n_draws,
    target_minimum_accepted = TARGET_MIN_ACCEPTED,
    target_preferred_accepted = TARGET_PREFERRED_ACCEPTED,
    accepted_meets_minimum_target = accepted >= TARGET_MIN_ACCEPTED,
    accepted_meets_preferred_target = accepted >= TARGET_PREFERRED_ACCEPTED,
    rejected_no_valid_assignment = rejected_no_valid,
    rejected_non_distinct_columns = rejected_non_distinct,
    rejected_due_near_zero = rejected_due_near_zero,
    sign_only_would_accept = ifelse(isTRUE(config$use_near_zero), sign_only_would_accept, NA_integer_),
    acceptance_rate_loss_vs_sign_only_with_same_draws = ifelse(isTRUE(config$use_near_zero), (sign_only_would_accept - accepted) / n_draws, NA_real_),
    candidate_draws_with_multi_shock_columns = candidate_multi_col_draws,
    accepted_draws_with_multi_shock_columns = accepted_multi_col_draws,
    candidate_multi_shock_column_total = candidate_multi_col_total,
    accepted_multi_shock_column_total = accepted_multi_col_total,
    unique_assignment_draws = unique_assignment_draws,
    unique_assignment_rate = unique_assignment_draws / accepted,
    nonunique_assignment_draws = accepted - unique_assignment_draws,
    sign_flip_any_draws = sign_flip_any_count,
    sign_flip_any_rate = sign_flip_any_count / accepted,
    stringsAsFactors = FALSE
  )

  shock_diag <- data.frame(
    model_variant = config$model_variant,
    shock = SHOCKS$shock,
    accepted_draws_for_shock = accepted,
    sign_flip_draws = as.integer(sign_flip_counts),
    sign_flip_rate = as.integer(sign_flip_counts) / accepted,
    median_assigned_column = apply(assignment_columns, 2, median, na.rm = TRUE),
    most_common_assigned_column = apply(assignment_columns, 2, function(x) as.integer(names(sort(table(x), decreasing = TRUE))[1])),
    stringsAsFactors = FALSE
  )

  impact_diag <- bind_rows(lapply(seq_len(nrow(restrictions_for_impact)), function(i) {
    x <- impact_draws[, i]
    tol <- if (restrictions_for_impact$restriction_type[[i]] == "near_zero") {
      near_zero_tolerances$near_zero_tolerance[near_zero_tolerances$variable == restrictions_for_impact$variable[[i]]][[1]]
    } else {
      NA_real_
    }
    data.frame(
      model_variant = config$model_variant,
      shock = restrictions_for_impact$shock[[i]],
      variable = restrictions_for_impact$variable[[i]],
      restriction = restrictions_for_impact$restriction_type[[i]],
      near_zero_tolerance = tol,
      mean_impact = mean(x, na.rm = TRUE),
      mean_abs_impact = mean(abs(x), na.rm = TRUE),
      median_impact = median(x, na.rm = TRUE),
      min_impact = min(x, na.rm = TRUE),
      max_impact = max(x, na.rm = TRUE),
      p16_impact = as.numeric(quantile(x, 0.16, na.rm = TRUE, names = FALSE)),
      p84_impact = as.numeric(quantile(x, 0.84, na.rm = TRUE, names = FALSE)),
      stringsAsFactors = FALSE
    )
  }))

  overlap_candidate_df <- as.data.frame(candidate_overlap) |>
    tibble::rownames_to_column("shock") |>
    mutate(model_variant = config$model_variant, denominator = n_draws, statistic = "candidate_draw_count", .before = 1)
  overlap_accepted_df <- as.data.frame(accepted_overlap) |>
    tibble::rownames_to_column("shock") |>
    mutate(model_variant = config$model_variant, denominator = accepted, statistic = "accepted_draw_count", .before = 1)
  overlap_rate_df <- as.data.frame(accepted_overlap / accepted) |>
    tibble::rownames_to_column("shock") |>
    mutate(model_variant = config$model_variant, denominator = accepted, statistic = "accepted_draw_rate", .before = 1)

  list(
    config = config,
    irf_draws = irf_draws,
    fevd_draws = fevd_draws,
    acceptance_summary = acceptance_summary,
    shock_diag = shock_diag,
    impact_diag = impact_diag,
    assignment_diag = data.frame(
      model_variant = config$model_variant,
      accepted_draw = seq_len(accepted),
      possible_distinct_assignments = assignments_count,
      unique_assignment = assignments_count == 1L,
      assignment_columns,
      assignment_signs,
      check.names = FALSE
    ),
    overlap = bind_rows(overlap_candidate_df, overlap_accepted_df, overlap_rate_df)
  )
}

run_with_extension <- function(A, P, config, near_zero_tolerances) {
  result <- run_identification(A, P, config, near_zero_tolerances)
  if (!isTRUE(config$use_near_zero) &&
      result$acceptance_summary$accepted_rotations[[1]] < TARGET_MIN_ACCEPTED &&
      EXTENDED_DRAWS > config$n_draws) {
    cat(config$model_variant, "accepted below target; rerunning with", EXTENDED_DRAWS, "draws.\n")
    config$n_draws <- EXTENDED_DRAWS
    rm(result)
    gc()
    result <- run_identification(A, P, config, near_zero_tolerances)
  }
  result
}

restriction_label_for <- function(config, shock_name, response, horizon) {
  shock_key <- SHOCKS$shock_key[SHOCKS$shock == shock_name][[1]]
  sign_row <- SIGN_RESTRICTIONS[SIGN_RESTRICTIONS$shock_key == shock_key & SIGN_RESTRICTIONS$variable == response, , drop = FALSE]
  if (nrow(sign_row) > 0L && horizon %in% config$restriction_horizons) return(sign_row$restriction[[1]])
  if (isTRUE(config$use_near_zero)) {
    nz_row <- NEAR_ZERO_RESTRICTIONS[NEAR_ZERO_RESTRICTIONS$shock_key == shock_key & NEAR_ZERO_RESTRICTIONS$variable == response, , drop = FALSE]
    if (nrow(nz_row) > 0L && horizon == 0L) return("near_zero")
  }
  "free"
}

summarise_irfs <- function(result) {
  draws <- result$irf_draws
  out <- vector("list", length(dimnames(draws)$shock) * length(dimnames(draws)$response) * (HORIZON + 1L))
  id <- 1L
  for (shock in dimnames(draws)$shock) {
    for (response in dimnames(draws)$response) {
      for (h in 0:HORIZON) {
        vals <- draws[, as.character(h), response, shock]
        q <- summarise_values(vals)
        restriction <- restriction_label_for(result$config, shock, response, h)
        out[[id]] <- data.frame(
          model_variant = result$config$model_variant,
          model_label = result$config$model_label,
          shock = shock,
          response = response,
          horizon = h,
          mean_irf = q[["mean"]],
          median_irf = q[["median"]],
          p16 = q[["p16"]],
          p84 = q[["p84"]],
          p2_5 = q[["p2_5"]],
          p97_5 = q[["p97_5"]],
          response_restriction = restriction,
          response_was_restricted = restriction != "free",
          zero_outside_68_band = !is.na(q[["p16"]]) && !is.na(q[["p84"]]) && (q[["p16"]] > 0 || q[["p84"]] < 0),
          zero_outside_95_band = !is.na(q[["p2_5"]]) && !is.na(q[["p97_5"]]) && (q[["p2_5"]] > 0 || q[["p97_5"]] < 0),
          stringsAsFactors = FALSE
        )
        id <- id + 1L
      }
    }
  }
  bind_rows(out)
}

summarise_fevd <- function(result) {
  draws <- result$fevd_draws
  out <- list()
  id <- 1L
  for (response in dimnames(draws)$response) {
    for (h in FEVD_HORIZONS) {
      for (shock in dimnames(draws)$shock) {
        vals <- draws[, as.character(h), response, shock]
        q <- summarise_values(vals)
        out[[id]] <- data.frame(
          model_variant = result$config$model_variant,
          model_label = result$config$model_label,
          response = response,
          horizon = h,
          shock = shock,
          mean_share = q[["mean"]],
          median_share = q[["median"]],
          p16 = q[["p16"]],
          p84 = q[["p84"]],
          p2_5 = q[["p2_5"]],
          p97_5 = q[["p97_5"]],
          mean_share_pct = 100 * q[["mean"]],
          median_share_pct = 100 * q[["median"]],
          stringsAsFactors = FALSE
        )
        id <- id + 1L
      }
    }
  }
  with_other <- bind_rows(out) |>
    group_by(model_variant, response, horizon) |>
    mutate(
      sum_mean_share = sum(mean_share, na.rm = TRUE),
      sum_median_share = sum(median_share, na.rm = TRUE),
      sum_mean_share_pct = 100 * sum_mean_share,
      sum_median_share_pct = 100 * sum_median_share,
      sum_close_to_one_mean = abs(sum_mean_share - 1) < 1e-8
    ) |>
    ungroup()
  identified <- with_other |>
    filter(shock != "Other / unidentified structural shocks") |>
    group_by(model_variant, response, horizon) |>
    mutate(
      identified_sum_mean_share = sum(mean_share, na.rm = TRUE),
      identified_sum_median_share = sum(median_share, na.rm = TRUE),
      identified_sum_mean_share_pct = 100 * identified_sum_mean_share,
      identified_sum_median_share_pct = 100 * identified_sum_median_share
    ) |>
    ungroup()
  list(with_other = with_other, identified_only = identified)
}

make_key_irf_summary <- function(irf_summary) {
  irf_summary |>
    inner_join(KEY_IRF_REQUESTS, by = c("shock", "response")) |>
    arrange(model_variant, shock, response, horizon)
}

load_baseline3 <- function() {
  if (!dir.exists(BASELINE3_DIR)) return(NULL)
  acc_path <- file.path(BASELINE3_DIR, "03_acceptance_diagnostics_baseline.xlsx")
  irf_path <- file.path(BASELINE3_DIR, "04_structural_irf_baseline.xlsx")
  fevd_path <- file.path(BASELINE3_DIR, "05_structural_fevd_baseline.xlsx")
  if (!all(file.exists(c(acc_path, irf_path, fevd_path)))) return(NULL)
  list(
    acceptance = openxlsx::read.xlsx(acc_path, "acceptance_summary") |> mutate(model_variant = "baseline3_h0_h2"),
    irf = openxlsx::read.xlsx(irf_path, "structural_irf_all") |> mutate(model_variant = "baseline3_h0_h2"),
    fevd = openxlsx::read.xlsx(fevd_path, "FEVD_with_other_unidentified") |> mutate(model_variant = "baseline3_h0_h2")
  )
}

make_comparison_with_baseline3 <- function(baseline3, refined_acceptance, refined_irf, refined_fevd) {
  if (is.null(baseline3)) {
    note <- data.frame(note = "Baseline 3-shock outputs not found.")
    return(list(acceptance = note, irf = note, fevd = note, other = note, interpretation = note))
  }
  s1_acc <- refined_acceptance |> filter(model_variant == "S1_four_shock_sign_only_h0_h2")
  acc <- bind_rows(
    baseline3$acceptance |>
      transmute(
        model = "Baseline 3 shocks",
        candidate_rotations,
        accepted_rotations,
        acceptance_rate,
        unique_assignment_rate,
        accepted_draws_with_multi_shock_columns,
        accepted_multi_shock_column_total
      ),
    s1_acc |>
      transmute(
        model = "Refined 4 shocks S1",
        candidate_rotations,
        accepted_rotations,
        acceptance_rate,
        unique_assignment_rate,
        accepted_draws_with_multi_shock_columns,
        accepted_multi_shock_column_total
      )
  ) |>
    mutate(
      acceptance_rate_pct = 100 * acceptance_rate,
      unique_assignment_rate_pct = 100 * unique_assignment_rate
    )

  common_shocks <- c(
    "Energy-carbon pressure shock",
    "Systemic financial stress shock",
    "Inflationary monetary-reaction shock"
  )
  irf_comp <- bind_rows(
    baseline3$irf |> filter(shock %in% common_shocks, response == "dlog_CDS") |> mutate(model = "Baseline 3 shocks"),
    refined_irf |> filter(model_variant == "S1_four_shock_sign_only_h0_h2", shock %in% common_shocks, response == "dlog_CDS") |> mutate(model = "Refined 4 shocks S1")
  ) |>
    select(model, shock, response, horizon, median_irf, mean_irf, p16, p84, p2_5, p97_5, zero_outside_68_band, zero_outside_95_band)

  fevd_comp <- bind_rows(
    baseline3$fevd |> filter(response == "dlog_CDS") |> mutate(model = "Baseline 3 shocks"),
    refined_fevd |> filter(model_variant == "S1_four_shock_sign_only_h0_h2", response == "dlog_CDS") |> mutate(model = "Refined 4 shocks S1")
  ) |>
    select(model, response, horizon, shock, mean_share, median_share, mean_share_pct, median_share_pct, sum_mean_share_pct, sum_median_share_pct)

  other <- fevd_comp |>
    filter(shock == "Other / unidentified structural shocks") |>
    select(model, response, horizon, mean_share_pct, median_share_pct)

  other_h12 <- other |> filter(horizon == 12)
  baseline_other <- other_h12$mean_share_pct[other_h12$model == "Baseline 3 shocks"][[1]]
  refined_other <- other_h12$mean_share_pct[other_h12$model == "Refined 4 shocks S1"][[1]]
  interpretation <- data.frame(
    criterion = c(
      "acceptance_rate_change_pct_points",
      "unique_assignment_rate_change_pct_points",
      "dlog_CDS_other_share_h12_change_pct_points",
      "dlog_CDS_other_declines",
      "first_three_CDS_responses_remain_positive_h0_h2"
    ),
    value = c(
      100 * (s1_acc$acceptance_rate[[1]] - baseline3$acceptance$acceptance_rate[[1]]),
      100 * (s1_acc$unique_assignment_rate[[1]] - baseline3$acceptance$unique_assignment_rate[[1]]),
      refined_other - baseline_other,
      as.character(refined_other < baseline_other),
      as.character(all(
        refined_irf |>
          filter(model_variant == "S1_four_shock_sign_only_h0_h2", shock %in% common_shocks, response == "dlog_CDS", horizon %in% 0:2) |>
          pull(median_irf) > 0
      ))
    ),
    stringsAsFactors = FALSE
  )
  list(acceptance = acc, irf = irf_comp, fevd = fevd_comp, other = other, interpretation = interpretation)
}

plot_irf_grid <- function(irf_summary, shock_name) {
  dat <- irf_summary |> filter(model_variant == "S1_four_shock_sign_only_h0_h2", shock == shock_name)
  p <- ggplot(dat, aes(x = horizon, y = median_irf)) +
    geom_ribbon(aes(ymin = p2_5, ymax = p97_5), fill = "#c7dbe8", alpha = 0.35) +
    geom_ribbon(aes(ymin = p16, ymax = p84), fill = "#5f9fbe", alpha = 0.35) +
    geom_hline(yintercept = 0, color = "grey45", linewidth = 0.25) +
    geom_line(color = "#225e78", linewidth = 0.8) +
    facet_wrap(~response, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = 0:HORIZON) +
    labs(title = paste("Refined4 S1 structural IRF:", shock_name), x = "Horizon", y = "Response") +
    theme_minimal(base_size = 10)
  path <- file.path(FIGURE_DIR, "structural_irf_all", paste0("refined4_s1_irf_grid_", safe_name(shock_name), ".png"))
  ggsave(path, p, width = 11, height = 8, dpi = 160)
}

plot_key_irfs <- function(irf_key) {
  key <- irf_key |> filter(model_variant == "S1_four_shock_sign_only_h0_h2")
  pairs <- unique(key[, c("shock", "response")])
  for (i in seq_len(nrow(pairs))) {
    pair <- pairs[i, ]
    dat <- key |> filter(shock == pair$shock, response == pair$response)
    p <- ggplot(dat, aes(x = horizon, y = median_irf)) +
      geom_ribbon(aes(ymin = p2_5, ymax = p97_5), fill = "#d8c3a5", alpha = 0.35) +
      geom_ribbon(aes(ymin = p16, ymax = p84), fill = "#b36b42", alpha = 0.32) +
      geom_hline(yintercept = 0, color = "grey40", linewidth = 0.25) +
      geom_line(color = "#8c3d1f", linewidth = 0.85) +
      geom_point(color = "#8c3d1f", size = 1.25) +
      scale_x_continuous(breaks = 0:HORIZON) +
      labs(title = paste(pair$shock, "->", pair$response), x = "Horizon", y = "Response") +
      theme_minimal(base_size = 10)
    ggsave(
      file.path(FIGURE_DIR, "structural_irf_key", paste0("refined4_s1_irf_", safe_name(pair$shock), "_to_", safe_name(pair$response), ".png")),
      p,
      width = 7.2,
      height = 4.6,
      dpi = 160
    )
  }
}

plot_fevd_response <- function(fevd_with_other, response_name) {
  dat <- fevd_with_other |>
    filter(model_variant == "S1_four_shock_sign_only_h0_h2", response == response_name) |>
    mutate(
      shock = factor(shock, levels = c(SHOCKS$shock, "Other / unidentified structural shocks")),
      horizon = factor(horizon, levels = FEVD_HORIZONS)
    )
  p <- ggplot(dat, aes(x = horizon, y = mean_share, fill = shock)) +
    geom_col(width = 0.75, color = "white", linewidth = 0.2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = paste("Refined4 S1 structural FEVD:", response_name), x = "Horizon", y = "Mean share", fill = "Shock") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")
  ggsave(file.path(FIGURE_DIR, "structural_fevd", paste0("refined4_s1_fevd_", safe_name(response_name), ".png")), p, width = 9.5, height = 5.6, dpi = 160)
}

plot_acceptance <- function(acceptance, baseline3 = NULL) {
  dat <- acceptance |>
    transmute(model = model_variant, accepted = accepted_rotations, rejected_no_valid = rejected_no_valid_assignment, rejected_non_distinct = rejected_non_distinct_columns, rejected_near_zero = rejected_due_near_zero) |>
    pivot_longer(-model, names_to = "status", values_to = "count")
  if (!is.null(baseline3)) {
    b <- baseline3$acceptance |>
      transmute(model = "baseline3_h0_h2", accepted = accepted_rotations, rejected_no_valid = rejected_no_valid_assignment, rejected_non_distinct = rejected_non_distinct_columns, rejected_near_zero = 0) |>
      pivot_longer(-model, names_to = "status", values_to = "count")
    dat <- bind_rows(b, dat)
  }
  p <- ggplot(dat, aes(x = model, y = count, fill = status)) +
    geom_col(width = 0.72) +
    labs(title = "Structural acceptance diagnostics", x = NULL, y = "Candidate rotations") +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "bottom")
  ggsave(file.path(FIGURE_DIR, "acceptance_diagnostics", "refined4_acceptance_diagnostics.png"), p, width = 10, height = 5.5, dpi = 160)
}

plot_overlap_heatmap <- function(overlap) {
  mat <- overlap |>
    filter(model_variant == "S1_four_shock_sign_only_h0_h2", statistic == "accepted_draw_rate") |>
    select(-model_variant, -denominator, -statistic) |>
    pivot_longer(-shock, names_to = "other_shock", values_to = "rate")
  p <- ggplot(mat, aes(x = other_shock, y = shock, fill = rate)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = scales::percent(rate, accuracy = 1)), size = 3) +
    scale_fill_gradient(low = "#f4f7f7", high = "#1f6f8b", labels = scales::percent_format(accuracy = 1)) +
    labs(title = "S1 accepted-draw overlap between shock restrictions", x = NULL, y = NULL, fill = "Rate") +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))
  ggsave(file.path(FIGURE_DIR, "acceptance_diagnostics", "refined4_s1_overlap_heatmap.png"), p, width = 9, height = 6, dpi = 160)
}

plot_baseline3_comparison <- function(comparison) {
  if (!"model" %in% names(comparison$fevd)) return(invisible(NULL))
  dat <- comparison$fevd |>
    filter(response == "dlog_CDS", horizon %in% FEVD_HORIZONS) |>
    mutate(horizon = factor(horizon, levels = FEVD_HORIZONS))
  p <- ggplot(dat, aes(x = horizon, y = mean_share, fill = shock)) +
    geom_col(width = 0.72, color = "white", linewidth = 0.2) +
    facet_wrap(~model) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = "dlog_CDS structural FEVD: 3-shock baseline vs refined4 S1", x = "Horizon", y = "Mean share", fill = "Shock") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")
  ggsave(file.path(FIGURE_DIR, "comparison_with_baseline3", "fevd_dlog_cds_baseline3_vs_refined4.png"), p, width = 11, height = 6, dpi = 160)
}

dominant_identified <- function(fevd_identified, response_name, model_variant = "S1_four_shock_sign_only_h0_h2", horizon = 12L) {
  dat <- fevd_identified |> filter(model_variant == !!model_variant, response == !!response_name, horizon == !!horizon)
  if (nrow(dat) == 0L) return(data.frame(response = response_name, shock = NA_character_, mean_share_pct = NA_real_, median_share_pct = NA_real_))
  dat[which.max(dat$mean_share), c("response", "horizon", "shock", "mean_share_pct", "median_share_pct")]
}

irf_h0_h2_statement <- function(irf, shock_name, response_name, model_variant = "S1_four_shock_sign_only_h0_h2") {
  dat <- irf |> filter(model_variant == !!model_variant, shock == !!shock_name, response == !!response_name, horizon %in% 0:2)
  data.frame(
    shock = shock_name,
    response = response_name,
    impact_median = dat$median_irf[dat$horizon == 0],
    avg_h0_h2_median = mean(dat$median_irf, na.rm = TRUE),
    positive_all_h0_h2 = all(dat$median_irf > 0, na.rm = TRUE),
    negative_all_h0_h2 = all(dat$median_irf < 0, na.rm = TRUE),
    zero_outside_68_any_h0_h2 = any(dat$zero_outside_68_band, na.rm = TRUE),
    zero_outside_95_any_h0_h2 = any(dat$zero_outside_95_band, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

write_reports <- function(rf, results, irf_all, fevd_all, comparison, baseline3) {
  acc <- bind_rows(lapply(results, `[[`, "acceptance_summary"))
  s1_acc <- acc |> filter(model_variant == "S1_four_shock_sign_only_h0_h2")
  s2_acc <- acc |> filter(model_variant == "S2_four_shock_sign_only_h0_h1")
  s3_acc <- acc |> filter(model_variant == "S3_four_shock_near_zero_h0_h2")
  setup <- rf$numerical_checks
  fe_stable <- setup$value[setup$check == "reduced_form_stable_from_workbook"]
  sigma_pd <- setup$value[setup$check == "Sigma_u_positive_definite"]

  common_shocks <- c("Energy-carbon pressure shock", "Systemic financial stress shock", "Inflationary monetary-reaction shock")
  cds_energy <- irf_h0_h2_statement(irf_all, "Energy-carbon pressure shock", "dlog_CDS")
  cds_stress <- irf_h0_h2_statement(irf_all, "Systemic financial stress shock", "dlog_CDS")
  cds_infl <- irf_h0_h2_statement(irf_all, "Inflationary monetary-reaction shock", "dlog_CDS")
  cds_sovereign <- irf_h0_h2_statement(irf_all, "Sovereign-risk repricing shock", "dlog_CDS")
  cpi_energy <- irf_h0_h2_statement(irf_all, "Energy-carbon pressure shock", "d_CPI")
  rate_energy <- irf_h0_h2_statement(irf_all, "Energy-carbon pressure shock", "d_3MRate")
  gdp_stress <- irf_h0_h2_statement(irf_all, "Systemic financial stress shock", "GDP_Growth")
  cpi_infl <- irf_h0_h2_statement(irf_all, "Inflationary monetary-reaction shock", "d_CPI")
  rate_infl <- irf_h0_h2_statement(irf_all, "Inflationary monetary-reaction shock", "d_3MRate")

  dom_cds <- dominant_identified(fevd_all$identified_only, "dlog_CDS")
  dom_cpi <- dominant_identified(fevd_all$identified_only, "d_CPI")
  dom_rate <- dominant_identified(fevd_all$identified_only, "d_3MRate")

  fevd_cds_h12 <- fevd_all$with_other |> filter(model_variant == "S1_four_shock_sign_only_h0_h2", response == "dlog_CDS", horizon == 12)
  other_h12 <- fevd_cds_h12$mean_share_pct[fevd_cds_h12$shock == "Other / unidentified structural shocks"][[1]]
  sovereign_h12 <- fevd_cds_h12$mean_share_pct[fevd_cds_h12$shock == "Sovereign-risk repricing shock"][[1]]

  baseline_other_declines <- NA
  unique_improves <- NA
  if (!is.null(baseline3)) {
    b_other <- baseline3$fevd |> filter(response == "dlog_CDS", horizon == 12, shock == "Other / unidentified structural shocks") |> pull(mean_share_pct)
    if (length(b_other) > 0) baseline_other_declines <- other_h12 < b_other[[1]]
    unique_improves <- s1_acc$unique_assignment_rate[[1]] >= baseline3$acceptance$unique_assignment_rate[[1]]
  }

  s1_overlap <- results$S1$overlap |> filter(statistic == "accepted_draw_rate")
  overlap_energy_infl <- s1_overlap |>
    filter(shock == "Energy-carbon pressure shock") |>
    pull(`Inflationary monetary-reaction shock`)
  overlap_fin_sov <- s1_overlap |>
    filter(shock == "Systemic financial stress shock") |>
    pull(`Sovereign-risk repricing shock`)

  sensitivity_max_diff <- irf_all |>
    filter(model_variant %in% c("S1_four_shock_sign_only_h0_h2", "S2_four_shock_sign_only_h0_h1"), response == "dlog_CDS", shock %in% common_shocks) |>
    select(model_variant, shock, response, horizon, median_irf) |>
    pivot_wider(names_from = model_variant, values_from = median_irf) |>
    mutate(abs_diff = abs(S2_four_shock_sign_only_h0_h1 - S1_four_shock_sign_only_h0_h2)) |>
    pull(abs_diff) |>
    max(na.rm = TRUE)

  refined_usable <- isTRUE(s1_acc$accepted_meets_minimum_target[[1]]) && fe_stable == "TRUE" && sigma_pd == "TRUE"
  s3_usable <- isTRUE(s3_acc$accepted_meets_minimum_target[[1]]) && s3_acc$acceptance_rate[[1]] > 0.01
  refined_better <- refined_usable && isTRUE(baseline_other_declines) && all(c(cds_energy$positive_all_h0_h2, cds_stress$positive_all_h0_h2, cds_infl$positive_all_h0_h2))

  lines <- c(
    "# Structural Refined4 Report",
    "",
    "## Reduced-Form Setup",
    paste0("Input reduced-form: FE/LSDV PVAR(1) from `", FE_WORKBOOK, "`."),
    "Variable order: Energy_Factor, d_CISS, d_CPI, GDP_Growth, d_3MRate, d_FiscalBalanceGDP, dlog_CDS.",
    paste0("Sample: ", rf$sample_summary$countries[[1]], " countries, ", rf$sample_summary$quarters[[1]], " quarters, ", rf$sample_summary$observations[[1]], " observations, ", rf$sample_summary$min_quarter[[1]], "-", rf$sample_summary$max_quarter[[1]], "."),
    paste0("Reduced-form stable: ", fe_stable, ". Max modulus: ", setup$value[setup$check == "max_modulus_recomputed"], "."),
    paste0("Sigma_u positive definite: ", sigma_pd, ". Min eigenvalue: ", setup$value[setup$check == "Sigma_u_min_eigenvalue"], "."),
    "",
    "## Refined Sign Restrictions",
    "S1 identifies four shocks with sign restrictions on h=0,1,2. S2 applies the same restrictions on h=0,1. S3 adds near-zero impact restrictions for separation and is treated as optional sensitivity.",
    "dlog_CDS remains unrestricted in the first three shocks. It is restricted positive only in the Sovereign-risk repricing shock.",
    "",
    "## Acceptance Diagnostics",
    paste(capture.output(print(acc)), collapse = "\n"),
    paste0("S1 acceptance rate: ", round(100 * s1_acc$acceptance_rate[[1]], 2), "%; unique assignment rate: ", round(100 * s1_acc$unique_assignment_rate[[1]], 2), "%."),
    paste0("S2 acceptance rate: ", round(100 * s2_acc$acceptance_rate[[1]], 2), "%."),
    paste0("S3 acceptance rate: ", round(100 * s3_acc$acceptance_rate[[1]], 2), "%; near-zero rejection count: ", s3_acc$rejected_due_near_zero[[1]], "."),
    paste0("S1 Energy-Inflation overlap accepted-draw rate: ", round(100 * overlap_energy_infl[[1]], 2), "%."),
    paste0("S1 Financial-Sovereign overlap accepted-draw rate: ", round(100 * overlap_fin_sov[[1]], 2), "%."),
    "",
    "## Structural IRF Interpretation",
    paste0("Energy shock -> d_CPI and d_3MRate positive h0-h2: ", cpi_energy$positive_all_h0_h2 && rate_energy$positive_all_h0_h2, "."),
    paste0("Energy shock -> unrestricted dlog_CDS positive h0-h2: ", cds_energy$positive_all_h0_h2, "; impact median: ", round(cds_energy$impact_median, 5), "."),
    paste0("Financial stress shock -> GDP_Growth negative h0-h2: ", gdp_stress$negative_all_h0_h2, "."),
    paste0("Financial stress shock -> unrestricted dlog_CDS positive h0-h2: ", cds_stress$positive_all_h0_h2, "; impact median: ", round(cds_stress$impact_median, 5), "."),
    paste0("Inflationary monetary-reaction shock -> d_CPI and d_3MRate positive h0-h2: ", cpi_infl$positive_all_h0_h2 && rate_infl$positive_all_h0_h2, "."),
    paste0("Inflationary monetary-reaction shock -> unrestricted dlog_CDS positive h0-h2: ", cds_infl$positive_all_h0_h2, "; impact median: ", round(cds_infl$impact_median, 5), "."),
    paste0("Sovereign-risk repricing shock -> dlog_CDS positive h0-h2: ", cds_sovereign$positive_all_h0_h2, "; impact median: ", round(cds_sovereign$impact_median, 5), "."),
    "",
    "## Structural FEVD Interpretation",
    paste(capture.output(print(fevd_cds_h12[, c("shock", "mean_share_pct", "median_share_pct", "sum_mean_share_pct", "sum_median_share_pct")])), collapse = "\n"),
    paste0("At horizon 12, among identified shocks, dlog_CDS is mostly explained by: ", dom_cds$shock[[1]], " (mean share ", round(dom_cds$mean_share_pct[[1]], 2), "%)."),
    paste0("At horizon 12, among identified shocks, d_CPI is mostly explained by: ", dom_cpi$shock[[1]], " (mean share ", round(dom_cpi$mean_share_pct[[1]], 2), "%)."),
    paste0("At horizon 12, among identified shocks, d_3MRate is mostly explained by: ", dom_rate$shock[[1]], " (mean share ", round(dom_rate$mean_share_pct[[1]], 2), "%)."),
    paste0("Sovereign-risk repricing shock explains ", round(sovereign_h12, 2), "% of dlog_CDS at horizon 12. Other/unidentified is ", round(other_h12, 2), "%."),
    "",
    "## Comparison With Baseline3",
    paste(capture.output(print(comparison$acceptance)), collapse = "\n"),
    paste(capture.output(print(comparison$interpretation)), collapse = "\n"),
    "",
    "## Final Answers",
    paste0("1. Reduced-form-ul folosit ramane stabil? ", fe_stable, "."),
    paste0("2. Sigma_u ramane pozitiv definita? ", sigma_pd, "."),
    paste0("3. Modelul cu 4 socuri are acceptance rate rezonabil? ", s1_acc$accepted_meets_minimum_target[[1]], "."),
    paste0("4. Unique assignment rate se imbunatateste fata de baseline3? ", unique_improves, "."),
    paste0("5. Overlap-ul intre Energy shock si Inflationary shock se reduce? Vezi matricea overlap; rata S1 acceptata este ", round(100 * overlap_energy_infl[[1]], 2), "%."),
    paste0("6. Overlap-ul intre Financial stress si Sovereign-risk este acceptabil? ", overlap_fin_sov[[1]] < 0.8, "; rata S1 acceptata este ", round(100 * overlap_fin_sov[[1]], 2), "%."),
    paste0("7. dlog_CDS raspunde pozitiv la Energy-carbon pressure shock fara restrictie? ", cds_energy$positive_all_h0_h2, "."),
    paste0("8. dlog_CDS raspunde pozitiv la Systemic financial stress shock fara restrictie? ", cds_stress$positive_all_h0_h2, "."),
    paste0("9. dlog_CDS raspunde pozitiv la Inflationary monetary-reaction shock fara restrictie? ", cds_infl$positive_all_h0_h2, "."),
    paste0("10. Sovereign-risk repricing shock genereaza raspuns pozitiv si persistent al dlog_CDS? ", cds_sovereign$positive_all_h0_h2, "."),
    paste0("11. Sovereign-risk repricing shock reduce Other/unidentified in FEVD dlog_CDS? ", baseline_other_declines, "."),
    paste0("12. Socul care explica cel mai mult dlog_CDS la h12: ", dom_cds$shock[[1]], "."),
    paste0("13. Socul care explica cel mai mult d_CPI la h12: ", dom_cpi$shock[[1]], "."),
    paste0("14. Socul care explica cel mai mult d_3MRate la h12: ", dom_rate$shock[[1]], "."),
    paste0("15. Sensitivity h=0,1 confirma baseline-ul? Diferenta maxima in median dlog_CDS pentru primele trei socuri este ", round(sensitivity_max_diff, 5), "."),
    paste0("16. S3 near-zero este implementabila? TRUE; tolerance fraction = ", NEAR_ZERO_TOLERANCE_FRACTION, "."),
    paste0("17. Daca S3 este rulat, imbunatateste identificarea sau devine prea restrictiv? S3 usable = ", s3_usable, "; acceptance rate = ", round(100 * s3_acc$acceptance_rate[[1]], 2), "%."),
    paste0("18. Modelul refined4 este mai bun decat baseline3? ", refined_better, "."),
    paste0("19. Putem folosi refined4 pentru historical decomposition? ", refined_usable, ", dupa validarea finala a overlap-ului si a S3 ca sensitivity, nu in aceasta etapa."),
    paste0("20. Putem folosi refined4 pentru counterfactual analysis? ", refined_usable, ", ulterior, dupa historical decomposition si validari suplimentare."),
    "",
    "## Conclusion",
    paste0("Recommended next baseline: ", ifelse(refined_better, "Refined4 S1, with S2/S3 as sensitivity.", "Keep refined4 as sensitivity until overlap/FEVD is reviewed.")),
    "No historical decomposition or counterfactual analysis was run in this stage."
  )
  writeLines(lines, file.path(OUTPUT_DIR, "summary_report_structural_refined4.md"))

  paper_lines <- c(
    "# Structural Refined4 Interpretation for Paper",
    "",
    "The refined four-shock model extends the previous three-shock baseline by adding a Sovereign-risk repricing shock identified through a positive dlog_CDS response.",
    "The first three shocks keep dlog_CDS unrestricted, so their CDS responses remain empirical outcomes rather than imposed restrictions.",
    "",
    "## Shock Interpretation",
    "Energy-carbon pressure shock captures common energy-carbon price pressure and is expected to pass through to inflation and short rates.",
    "Systemic financial stress shock captures European stress through CISS and a negative real-activity response.",
    "Inflationary monetary-reaction shock captures inflationary pressure accompanied by higher short rates; it is not a pure monetary policy shock.",
    "Sovereign-risk repricing shock captures the own CDS repricing component not labelled by the first three shocks.",
    "",
    "## Main Finding",
    paste0("In S1, dlog_CDS is positive over h=0..2 after the first three unrestricted-CDS shocks: energy=", cds_energy$positive_all_h0_h2, ", stress=", cds_stress$positive_all_h0_h2, ", inflationary reaction=", cds_infl$positive_all_h0_h2, "."),
    paste0("The sovereign-risk shock explains ", round(sovereign_h12, 2), "% of dlog_CDS at horizon 12, while Other/unidentified is ", round(other_h12, 2), "%."),
    "",
    "## Recommendation",
    ifelse(refined_better, "Use refined4 S1 as the preferred structural baseline and retain S2/S3 as sensitivity checks.", "Do not replace the three-shock baseline without reviewing overlap and FEVD diagnostics; use refined4 as sensitivity for now."),
    "Next stages may add historical decomposition and counterfactual analysis only after the refined4 identification is accepted."
  )
  writeLines(paper_lines, file.path(OUTPUT_DIR, "structural_refined4_interpretation_for_paper.md"))
}

main <- function() {
  cat("Reading reduced-form FE/LSDV PVAR(1) outputs...\n")
  rf <- read_reduced_form()
  cat("Computing near-zero tolerances from candidate impact rotations...\n")
  near_zero_tolerances <- compute_near_zero_tolerances(rf$P)

  cat("Running refined4 structural variants...\n")
  results <- list()
  for (name in names(VARIANTS)) {
    results[[name]] <- run_with_extension(rf$A, rf$P, VARIANTS[[name]], near_zero_tolerances)
  }

  cat("Summarising IRFs and FEVDs...\n")
  irf_all <- bind_rows(lapply(results, summarise_irfs))
  irf_key <- make_key_irf_summary(irf_all)
  fevd_list <- lapply(results, summarise_fevd)
  fevd_all <- list(
    with_other = bind_rows(lapply(fevd_list, `[[`, "with_other")),
    identified_only = bind_rows(lapply(fevd_list, `[[`, "identified_only"))
  )
  acceptance_all <- bind_rows(lapply(results, `[[`, "acceptance_summary"))
  shock_diag_all <- bind_rows(lapply(results, `[[`, "shock_diag"))
  impact_diag_all <- bind_rows(lapply(results, `[[`, "impact_diag"))
  assignment_all <- bind_rows(lapply(results, `[[`, "assignment_diag"))
  overlap_all <- bind_rows(lapply(results, `[[`, "overlap"))
  restrictions_all <- bind_rows(lapply(VARIANTS, make_restriction_grid, near_zero_tolerances = near_zero_tolerances))

  baseline3 <- load_baseline3()
  comparison <- make_comparison_with_baseline3(baseline3, acceptance_all, irf_all, fevd_all$with_other)

  cat("Creating figures...\n")
  for (shock in SHOCKS$shock) plot_irf_grid(irf_all, shock)
  plot_key_irfs(irf_key)
  for (response in c("dlog_CDS", "d_CPI", "d_3MRate")) plot_fevd_response(fevd_all$with_other, response)
  plot_acceptance(acceptance_all, baseline3)
  plot_overlap_heatmap(overlap_all)
  plot_baseline3_comparison(comparison)

  cat("Writing workbooks...\n")
  write_workbook(
    list(
      variable_order = data.frame(order = seq_along(MODEL_VARS), variable = MODEL_VARS),
      sample_summary = rf$sample_summary,
      reduced_form_A1 = matrix_to_sheet(rf$A, "response"),
      reduced_form_Sigma_u = matrix_to_sheet(rf$Sigma, "response"),
      Cholesky_factor_P = matrix_to_sheet(rf$P, "response"),
      reduced_form_stability = rf$stability,
      eigenvalues_A1 = data.frame(root = seq_along(rf$eigen_A), real = Re(rf$eigen_A), imaginary = Im(rf$eigen_A), modulus = Mod(rf$eigen_A)),
      Sigma_u_eigenvalues = data.frame(root = seq_along(rf$eigen_Sigma), eigenvalue = rf$eigen_Sigma),
      numerical_checks = rf$numerical_checks,
      near_zero_tolerances = near_zero_tolerances
    ),
    file.path(OUTPUT_DIR, "01_refined4_structural_setup.xlsx")
  )

  write_workbook(
    list(
      shock_definitions = SHOCKS,
      S1_restrictions = restrictions_all |> filter(model_variant == "S1_four_shock_sign_only_h0_h2"),
      S2_restrictions = restrictions_all |> filter(model_variant == "S2_four_shock_sign_only_h0_h1"),
      S3_restrictions = restrictions_all |> filter(model_variant == "S3_four_shock_near_zero_h0_h2"),
      all_restrictions = restrictions_all,
      unrestricted_variables = restrictions_all |> filter(restriction == "free"),
      near_zero_tolerances = near_zero_tolerances
    ),
    file.path(OUTPUT_DIR, "02_refined4_sign_restrictions.xlsx")
  )

  write_workbook(
    list(
      acceptance_summary_all = acceptance_all,
      shock_diagnostics_all = shock_diag_all,
      impact_diagnostics_all = impact_diag_all,
      assignment_diagnostics_all = assignment_all,
      overlap_matrices_all = overlap_all,
      overlap_S1_accepted_rates = overlap_all |> filter(model_variant == "S1_four_shock_sign_only_h0_h2", statistic == "accepted_draw_rate"),
      baseline3_acceptance_comparison = comparison$acceptance
    ),
    file.path(OUTPUT_DIR, "03_refined4_acceptance_diagnostics.xlsx")
  )

  write_workbook(
    list(
      structural_irf_all = irf_all,
      structural_irf_key = irf_key,
      S1_structural_irf_all = irf_all |> filter(model_variant == "S1_four_shock_sign_only_h0_h2"),
      S1_cds_unrestricted_first3 = irf_all |> filter(model_variant == "S1_four_shock_sign_only_h0_h2", shock %in% SHOCKS$shock[1:3], response == "dlog_CDS"),
      S3_near_zero_checks = irf_all |> filter(model_variant == "S3_four_shock_near_zero_h0_h2", response_restriction == "near_zero")
    ),
    file.path(OUTPUT_DIR, "04_refined4_structural_irf.xlsx")
  )

  write_workbook(
    list(
      FEVD_identified_shocks_only = fevd_all$identified_only,
      FEVD_with_other_unidentified = fevd_all$with_other,
      S1_FEVD_with_other = fevd_all$with_other |> filter(model_variant == "S1_four_shock_sign_only_h0_h2"),
      S1_FEVD_dlog_CDS = fevd_all$with_other |> filter(model_variant == "S1_four_shock_sign_only_h0_h2", response == "dlog_CDS"),
      S1_FEVD_d_CPI = fevd_all$with_other |> filter(model_variant == "S1_four_shock_sign_only_h0_h2", response == "d_CPI"),
      S1_FEVD_d_3MRate = fevd_all$with_other |> filter(model_variant == "S1_four_shock_sign_only_h0_h2", response == "d_3MRate"),
      S1_FEVD_GDP_Growth = fevd_all$with_other |> filter(model_variant == "S1_four_shock_sign_only_h0_h2", response == "GDP_Growth"),
      S1_FEVD_d_FiscalBalanceGDP = fevd_all$with_other |> filter(model_variant == "S1_four_shock_sign_only_h0_h2", response == "d_FiscalBalanceGDP")
    ),
    file.path(OUTPUT_DIR, "05_refined4_structural_fevd.xlsx")
  )

  write_workbook(
    list(
      acceptance_comparison = comparison$acceptance,
      irf_dlog_CDS_comparison = comparison$irf,
      fevd_dlog_CDS_comparison = comparison$fevd,
      other_unidentified_comparison = comparison$other,
      interpretation_comparison = comparison$interpretation
    ),
    file.path(OUTPUT_DIR, "06_refined4_vs_baseline3_comparison.xlsx")
  )

  write_workbook(
    list(
      clean_four_shock_restrictions = restrictions_all |> filter(model_variant == "S1_four_shock_sign_only_h0_h2"),
      clean_acceptance_table = acceptance_all,
      clean_key_structural_irfs = irf_key |> filter(model_variant == "S1_four_shock_sign_only_h0_h2"),
      clean_fevd_dlog_CDS = fevd_all$with_other |> filter(model_variant == "S1_four_shock_sign_only_h0_h2", response == "dlog_CDS"),
      clean_fevd_d_CPI = fevd_all$with_other |> filter(model_variant == "S1_four_shock_sign_only_h0_h2", response == "d_CPI"),
      clean_fevd_d_3MRate = fevd_all$with_other |> filter(model_variant == "S1_four_shock_sign_only_h0_h2", response == "d_3MRate"),
      clean_comparison_3_vs_4 = comparison$interpretation
    ),
    file.path(OUTPUT_DIR, "07_refined4_tables_for_paper.xlsx")
  )

  cat("Writing reports...\n")
  write_reports(rf, results, irf_all, fevd_all, comparison, baseline3)

  cat("Refined4 structural workflow complete.\n")
  cat("Output directory:", normalizePath(OUTPUT_DIR, winslash = "/"), "\n")
  print(acceptance_all[, c("model_variant", "candidate_rotations", "accepted_rotations", "acceptance_rate", "unique_assignment_rate")])
}

main()
