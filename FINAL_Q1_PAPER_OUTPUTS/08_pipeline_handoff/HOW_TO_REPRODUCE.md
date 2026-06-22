# How To Reproduce

Run from the repository root.

## 1. Install packages

```bash
Rscript 00_install_packages.R
```

## 2. Standard clean-clone reproduction

```bash
Rscript 00_master_pipeline_full_paper.R
```

In a clean clone, the master script detects missing intermediate caches and rebuilds from `incercare v2.xlsx`.

## 3. Explicit full rebuild

```bash
RUN_FROM_SCRATCH=true USE_CACHED_INTERMEDIATE_OUTPUTS=false Rscript 00_master_pipeline_full_paper.R
```

PowerShell:

```powershell
$env:RUN_FROM_SCRATCH = "true"
$env:USE_CACHED_INTERMEDIATE_OUTPUTS = "false"
Rscript .\00_master_pipeline_full_paper.R
```

The full rebuild can take substantially longer because the structural stage uses 50,000 candidate rotations.

## 4. Documentation-only refresh

These scripts do not re-estimate the model:

```bash
Rscript 21_polish_q1_figures_tables.R
Rscript 22_pre_model_diagnostics_cleanup.R
Rscript 23_fe_lsdv_pvar_dk_inference.R
Rscript 24_methodological_code_audit.R
Rscript 25_pipeline_handoff_docs.R
```

## 5. Optional all-stage handoff wrapper

```bash
Rscript 00_master_pipeline_full_paper_handoff.R
```

Open `00_master_pipeline_full_paper_handoff.R` first and inspect the flags. It is intended as an explicit handoff orchestrator, not as a replacement for reading the stage scripts.
