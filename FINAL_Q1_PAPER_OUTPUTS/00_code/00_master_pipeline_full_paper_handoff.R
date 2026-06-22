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

message("Final empirical pipeline handoff")
message("Model: ", MODEL_NAME)
message("Variable order: ", paste(MODEL_VARS, collapse = ", "))
message("A full rebuild can take a long time because the structural stage uses 50,000 candidate rotations.")

# Stage 0: package setup.
run_rscript("00_install_packages.R", TRUE)

# Stage 1: data, reduced-form baseline and robustness outputs.
run_rscript(
  "15_structural_pvar_full7_final_workflow.R",
  RUN_DATA_PREP || RUN_REDUCED_FORM || RUN_GMM_ROBUSTNESS || RUN_LP_DK_ROBUSTNESS
)

# Stage 2: structural identification.
run_rscript("17_structural_pvar_full7_refined4.R", RUN_STRUCTURAL_PVAR)

# Stage 3: historical decomposition.
run_rscript("19_structural_pvar_full7_hd_refined4.R", RUN_HISTORICAL_DECOMPOSITION)

# Stage 4: counterfactual analysis.
run_rscript("20_structural_pvar_full7_counterfactual_refined4.R", RUN_COUNTERFACTUAL)

# Stage 5: final consolidation. This can rebuild from scratch if caches are absent.
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
  "00_master_pipeline_full_paper.R",
  GENERATE_MASTER_EXCEL || GENERATE_ALL_FIGURES || GENERATE_PAPER_FIGURES,
  env = master_env
)

# Stage 6: documentation-only and inference-polishing layers.
run_rscript("21_polish_q1_figures_tables.R", RUN_Q1_POLISHING)
run_rscript("22_pre_model_diagnostics_cleanup.R", RUN_PRE_MODEL_DIAGNOSTICS)
run_rscript("23_fe_lsdv_pvar_dk_inference.R", RUN_DK_INFERENCE)
run_rscript("24_methodological_code_audit.R", RUN_METHOD_AUDIT)
run_rscript("25_pipeline_handoff_docs.R", RUN_HANDOFF_DOCS)

message("\nHandoff pipeline completed.")
message("Use FINAL_Q1_PAPER_OUTPUTS/08_pipeline_handoff for reproduction guidance and output selection.")
