# Code

Recommended entry point:

```bash
Rscript code/00_master_pipeline_full_paper_handoff.R
```

Script organization:

- `00_install_packages.R`: package setup.
- `00_master_pipeline_full_paper_handoff.R`: external-facing orchestration script.
- `02_pre_model_diagnostics/`: cleaned pre-model diagnostic outputs.
- `04_robustness/`: Driscoll-Kraay coefficient-level inference and robustness scripts.
- `05_structural_pvar/`: final data construction, reduced-form baseline, GMM/LP robustness and Structural PVAR scripts.
- `06_historical_decomposition/`: historical decomposition.
- `07_counterfactuals/`: counterfactual analysis.
- `08_tables_figures/`: paper-ready table and figure polishing.
- `09_validation/`: methodological audit and handoff manifest scripts.

The old consolidation script is archived in `archive/old_master_scripts/` and is called only as a legacy consolidation helper by the handoff master.
