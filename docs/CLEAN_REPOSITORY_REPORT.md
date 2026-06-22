# Clean Repository Report

## 1. What Was Reorganized

The repository was reorganized into an external-facing replication package. Root-level scripts were moved into `code/`, final input data into `data/raw/`, manuscript-ready outputs into `outputs/`, documentation into `docs/`, and internal or superseded material into `archive/`.

No empirical results were re-estimated or modified.

## 2. What Remains in Root

Root now contains only:

- `README.md`
- `LICENSE`
- `.gitignore`
- `.gitattributes`
- `data/`
- `code/`
- `outputs/`
- `docs/`
- `archive/`

## 3. Master Script

Recommended master script:

- `code/00_master_pipeline_full_paper_handoff.R`

The old consolidation script is retained as:

- `archive/old_master_scripts/00_master_pipeline_full_paper.R`

It is marked as a legacy consolidation helper and is not the recommended entry point for external users.

## 4. Main Outputs

Main manuscript tables:

- `outputs/02_tables/main_paper/`

Main manuscript figures:

- `outputs/03_figures/main_paper/`

## 5. Appendix Outputs

Appendix tables:

- `outputs/02_tables/appendix/`

Appendix figures:

- `outputs/03_figures/appendix/`

Robustness and diagnostics:

- `outputs/02_tables/robustness/`

## 6. Archive and Replication-Only Outputs

Replication-only outputs are stored in:

- `outputs/06_replication_only/`

Internal audit and previous handoff files are stored in:

- `archive/internal_audit/`

Local untracked intermediate caches are stored under:

- `archive/old_outputs/local_untracked_intermediate_outputs/`

That folder is ignored by Git because it was not part of the clean tracked replication package.

## 7. What to Read First

For supervisors, coauthors or external reviewers, start with:

1. `README.md`
2. `docs/OUTPUT_ROADMAP.md`
3. `docs/METHODOLOGY_OVERVIEW.md`
4. `outputs/04_reports/empirical_results_summary.md`
5. `docs/MANUSCRIPT_OUTPUT_SELECTION.md`

## 8. Remaining Old Files

Old or technical files remain only in `outputs/06_replication_only/` and `archive/`. They are clearly separated from the manuscript-ready outputs.

## 9. Final Verdict

The repository is clean, externally readable, and ready to be shared with supervisors, coauthors and technical reviewers.
