# External handoff orchestrator for the final paper pipeline.
# Inspect the flags before running. This wrapper starts each stage in a separate
# Rscript process so scripts that clear the workspace cannot affect later stages.

ROOT_DIR <- getwd()

# Main execution flags. Set any flag to FALSE to skip that stage.
RUN_DATA_PREP <- TRUE
RUN_REDUCED_FORM <- TRUE
RUN_GMM_ROBUSTNESS <- TRUE
RUN_LP_DK_ROBUSTNESS <- TRUE
RUN_STRUCTURAL_PVAR <- TRUE
RUN_HISTORICAL_DECOMPOSITION <- TRUE
RUN_COUNTERFACTUAL <- TRUE
GENERATE_MASTER_EXCEL <- TRUE
GENERATE_ALL_FIGURES <- TRUE
GENERATE_PAPER_FIGURES <- TRUE
RUN_Q1_POLISHING <- TRUE
RUN_PRE_MODEL_DIAGNOSTICS <- TRUE
RUN_DK_INFERENCE <- TRUE
RUN_METHOD_AUDIT <- TRUE
RUN_HANDOFF_DOCS <- TRUE

MODEL_NAME <- "Structural PVAR refined4 S1 - four-shock sign-only, h=0,1,2"
MODEL_VARS <- c(
  "Energy_Factor",
  "d_CISS",
  "d_CPI",
  "GDP_Growth",
  "d_3MRate",
  "d_FiscalBalanceGDP",
  "dlog_CDS"
)

run_rscript <- function(script, run_flag = TRUE, env = character()) {
  if (!isTRUE(run_flag)) {
    message("Skipping: ", script)
    return(invisible(FALSE))
  }
  path <- file.path(ROOT_DIR, script)
  if (!file.exists(path)) {
    stop("Missing script: ", path)
  }
  message("\n--- Running: ", script, " ---")
  rscript <- file.path(R.home("bin"), "Rscript")
  status <- system2(
    rscript,
    args = shQuote(normalizePath(path, winslash = "/", mustWork = TRUE)),
    env = env,
    stdout = "",
    stderr = ""
  )
  if (!identical(status, 0L)) {
    stop("Stage failed: ", script, " (status ", status, ")")
  }
  invisible(TRUE)
}

SCRIPT_INSTALL <- "code/00_install_packages.R"
SCRIPT_FINAL_WORKFLOW <- "code/05_structural_pvar/15_structural_pvar_full7_final_workflow.R"
SCRIPT_STRUCTURAL <- "code/05_structural_pvar/17_structural_pvar_full7_refined4.R"
SCRIPT_HD <- "code/06_historical_decomposition/19_structural_pvar_full7_hd_refined4.R"
SCRIPT_CF <- "code/07_counterfactuals/20_structural_pvar_full7_counterfactual_refined4.R"
SCRIPT_LEGACY_CONSOLIDATION <- "archive/old_master_scripts/00_master_pipeline_full_paper.R"
SCRIPT_POLISH <- "code/08_tables_figures/21_polish_q1_figures_tables.R"
SCRIPT_DIAGNOSTICS <- "code/02_pre_model_diagnostics/22_pre_model_diagnostics_cleanup.R"
SCRIPT_DK <- "code/04_robustness/23_fe_lsdv_pvar_dk_inference.R"
SCRIPT_AUDIT <- "code/09_validation/24_methodological_code_audit.R"
SCRIPT_HANDOFF_DOCS <- "code/09_validation/25_pipeline_handoff_docs.R"

message("Final empirical pipeline handoff")
message("Model: ", MODEL_NAME)
message("Variable order: ", paste(MODEL_VARS, collapse = ", "))
message("A full rebuild can take a long time because the structural stage uses 50,000 candidate rotations.")

# Stage 0: package setup.
run_rscript(SCRIPT_INSTALL, TRUE)

# Stage 1: data, reduced-form baseline and robustness outputs.
run_rscript(
  SCRIPT_FINAL_WORKFLOW,
  RUN_DATA_PREP || RUN_REDUCED_FORM || RUN_GMM_ROBUSTNESS || RUN_LP_DK_ROBUSTNESS
)

# Stage 2: structural identification.
run_rscript(SCRIPT_STRUCTURAL, RUN_STRUCTURAL_PVAR)

# Stage 3: historical decomposition.
run_rscript(SCRIPT_HD, RUN_HISTORICAL_DECOMPOSITION)

# Stage 4: counterfactual analysis.
run_rscript(SCRIPT_CF, RUN_COUNTERFACTUAL)

# Stage 5: final consolidation.
# This legacy consolidation script is archived because the external-facing
# committed outputs now live in outputs/. It is still callable for full reruns.
master_env <- c(
  "RUN_FROM_SCRATCH=false",
  "USE_CACHED_INTERMEDIATE_OUTPUTS=true",
  paste0("RUN_REDUCED_FORM=", tolower(RUN_REDUCED_FORM)),
  paste0("RUN_ROBUSTNESS=", tolower(RUN_GMM_ROBUSTNESS || RUN_LP_DK_ROBUSTNESS)),
  paste0("RUN_STRUCTURAL_PVAR=", tolower(RUN_STRUCTURAL_PVAR)),
  paste0("RUN_HISTORICAL_DECOMPOSITION=", tolower(RUN_HISTORICAL_DECOMPOSITION)),
  paste0("RUN_COUNTERFACTUAL=", tolower(RUN_COUNTERFACTUAL)),
  paste0("GENERATE_ALL_FIGURES=", tolower(GENERATE_ALL_FIGURES)),
  paste0("GENERATE_PAPER_FIGURES=", tolower(GENERATE_PAPER_FIGURES)),
  paste0("GENERATE_MASTER_EXCEL=", tolower(GENERATE_MASTER_EXCEL))
)
run_rscript(
  SCRIPT_LEGACY_CONSOLIDATION,
  GENERATE_MASTER_EXCEL || GENERATE_ALL_FIGURES || GENERATE_PAPER_FIGURES,
  env = master_env
)

# Stage 6: documentation-only and inference-polishing layers.
run_rscript(SCRIPT_POLISH, RUN_Q1_POLISHING)
run_rscript(SCRIPT_DIAGNOSTICS, RUN_PRE_MODEL_DIAGNOSTICS)
run_rscript(SCRIPT_DK, RUN_DK_INFERENCE)
run_rscript(SCRIPT_AUDIT, RUN_METHOD_AUDIT)
run_rscript(SCRIPT_HANDOFF_DOCS, RUN_HANDOFF_DOCS)

message("\nHandoff pipeline completed.")
message("Use docs/ and outputs/ for external-facing reproduction guidance and output selection.")
