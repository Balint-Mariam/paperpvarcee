# Pipeline Handoff Report

Generated: 2026-06-22 10:41:05 EEST
Final model: Structural PVAR refined4 S1 - four-shock sign-only, h=0,1,2
Audit verdict: PASS WITH MINOR WARNINGS - pipeline is coherent, but minor documentation fixes are needed.

## Generated files

- `PROJECT_HANDOFF_GUIDE.md`
- `HOW_TO_REPRODUCE.md`
- `script_execution_manifest.xlsx`
- `output_manifest_final.xlsx`
- `methodology_to_outputs_map.xlsx`
- `MANUSCRIPT_OUTPUT_SELECTION.md`
- `pipeline_handoff_report.md`

## Inventory summary

- Files indexed in FINAL_Q1_PAPER_OUTPUTS: 542
- Main-paper use files: 16
- Appendix/robustness files: 22
- Replication-only files: 246

## Audit warnings carried into handoff

- M06: Unpolished figures remain in the replication package but should not be used as main-paper figures.
- C_21_polish_q1_figures_tables.R: Polishing script uses output-driven variable ordering and labels; source model ordering is audited in model scripts.
- L02: The raw scenario table marks CF2 as main, but polished manuscript selection and handoff documentation should keep only CF1, CF4 and CF6 in the main paper.

## Handoff decision

The repository is coherent for external replication. The only carried warnings are output-selection/documentation warnings: use polished figures and selected CF scenarios for the manuscript.
