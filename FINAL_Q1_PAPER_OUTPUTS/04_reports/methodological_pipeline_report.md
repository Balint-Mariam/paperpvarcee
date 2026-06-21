# Methodological Pipeline Report

## 1. Data Construction
The final model-ready data are read from `C:/Users/user/Desktop/model/structural_pvar_ciss_full7_final_outputs/01_data_preparation_full7_final.xlsx`. The final panel contains 11 countries, 47 quarters and 517 observations.

## 2. Transformations
The final variables are Energy_Factor, d_CISS, d_CPI, GDP_Growth, d_3MRate, d_FiscalBalanceGDP and dlog_CDS. Energy_Factor is obtained from PCA on common energy-carbon price inputs.

## 3. Reduced-Form Model
The main reduced-form model is FE/LSDV PVAR(1). The max modulus is approximately 0.642314 and the system is stable.

## 4. Robustness
Robustness tables consolidate restricted PVAR-GMM and panel local projection outputs available from the validated workflow.

## 5. Structural PVAR
The final structural model is Structural PVAR refined4 S1 - four-shock sign-only, h=0,1,2. It uses 50,000 candidate rotations, 12,715 accepted rotations, acceptance rate 25.43%, and unique assignment rate 20.34%.

## 6. Historical Decomposition
Historical decomposition uses the representative refined4 S1 structural matrix from candidate draw 23085 / accepted draw 5782. The HD reconstruction max error is approximately 4.44e-16.

## 7. Counterfactual Analysis
Counterfactuals remove selected labelled shock contributions from the validated HD while preserving Other/unidentified and initial/deterministic components.

## 8. Final Outputs
The final folder contains consolidated Excel workbooks, paper-ready figures, appendix figures, table and figure manifests, captions and reports.
