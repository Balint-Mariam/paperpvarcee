# Alternative Driscoll-Kraay Inference for Baseline FE/LSDV PVAR(1)

## 1. Purpose

This robustness step recalculates coefficient-level inference for the baseline FE/LSDV PVAR(1) using Driscoll-Kraay standard errors. It does not change the model, the estimated coefficient matrix, the Structural PVAR, FEVD, historical decomposition or counterfactual analysis.

## 2. Model and Coefficients

- Model: FE/LSDV PVAR(1), equation-by-equation within estimator with country fixed effects
- Effective regression sample: 2014Q3-2025Q4
- Countries: 11
- Effective observations: 506
- Coefficient check: Maximum absolute coefficient difference vs master T11 = 8.882e-15

## 3. Why Driscoll-Kraay?

Driscoll-Kraay standard errors are used because the diagnostics indicate cross-sectional dependence and because the panel setting may also involve heteroskedasticity and serial correlation. The correction is applied equation by equation to the fixed-effects/within regressions.

## 4. DK Settings

The baseline DK lag is 4, appropriate for quarterly data. Sensitivity checks are reported for lags 2, 4, 6.

## 5. Key Channels

| Relation | Coefficient | Original p-value | DK p-value | Final interpretation |
|---|---:|---:|---:|---|
| Energy_Factor -> d_CPI | 0.2731 | <0.001 | 0.006 | robust under DK |
| Energy_Factor -> d_3MRate | 0.0464 | 0.001 | 0.002 | robust under DK |
| d_CPI -> d_3MRate | 0.0265 | 0.031 | 0.003 | robust under DK |
| Energy_Factor -> dlog_CDS | 0.0218 | <0.001 | 0.002 | robust under DK |
| d_CISS -> dlog_CDS | 0.5754 | <0.001 | <0.001 | robust under DK |
| d_CPI -> dlog_CDS | 0.0129 | 0.004 | 0.007 | robust under DK |
| GDP_Growth -> d_FiscalBalanceGDP | 0.0557 | <0.001 | 0.002 | robust under DK |
| d_3MRate -> dlog_CDS | 0.0090 | 0.450 | 0.737 | not significant under either |
| d_FiscalBalanceGDP -> dlog_CDS | -0.0027 | 0.769 | 0.543 | not significant under either |

## 6. Relations Remaining Significant Under DK

- Energy_Factor -> d_CPI
- Energy_Factor -> d_3MRate
- d_CPI -> d_3MRate
- Energy_Factor -> dlog_CDS
- d_CISS -> dlog_CDS
- d_CPI -> dlog_CDS
- GDP_Growth -> d_FiscalBalanceGDP

## 7. Relations Weaker Under DK

No key channel that was originally significant becomes insignificant under DK.

## 8. Relations Not Significant Under Either

- d_3MRate -> dlog_CDS
- d_FiscalBalanceGDP -> dlog_CDS

## 9. Relations Changing Interpretation

No key channel changes from insignificant originally to significant under DK.

## 10. Recommendation for Paper

Use the DK table as an appendix or robustness-inference table for the baseline reduced-form FE/LSDV PVAR. The structural IRF, FEVD, historical decomposition and counterfactual exercises should continue to rely on the stable FE/LSDV dynamic system; this step only changes reported standard errors and significance stars.

## Proposed Paper Text

Given the strong evidence of cross-sectional dependence in the pre-model and residual diagnostics, coefficient-level inference for the baseline FE/LSDV PVAR(1) is additionally assessed using Driscoll-Kraay standard errors computed equation by equation. This adjustment affects only the reported standard errors and significance levels, while leaving the estimated dynamic coefficient matrix unchanged. The structural impulse responses, FEVD, historical decomposition and counterfactual exercises continue to rely on the stable FE/LSDV dynamic system. The main reduced-form channels remain broadly robust under Driscoll-Kraay inference.

## Output

- C:/Users/user/Desktop/model/paperpvarcee_repo/FINAL_Q1_PAPER_OUTPUTS/06_alternative_inference_DK/FE_LSDV_PVAR_DK_inference.xlsx
- C:/Users/user/Desktop/model/paperpvarcee_repo/FINAL_Q1_PAPER_OUTPUTS/06_alternative_inference_DK/DK_inference_report.md
