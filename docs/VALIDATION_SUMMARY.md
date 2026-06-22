# Validation Summary

The repository passed the final methodological code audit with:

- 82 PASS checks
- 3 minor warnings
- 0 FAIL checks

No methodological FAIL was detected.

## Main Validation Points

- The final sample is a balanced panel of 11 CEE countries over 2014Q2-2025Q4.
- The final variable order is consistent across the model scripts.
- The FE/LSDV Panel VAR(1) baseline is stable.
- Driscoll-Kraay inference confirms the coefficient-level robustness layer and does not alter the FE/LSDV dynamic matrix.
- Pre-model diagnostics include CIPS/CADF stationarity checks and were successfully run.
- PVAR-GMM(1) and LP-DK are retained as robustness evidence.
- Structural PVAR refined4 S1 is the final structural specification.
- Structural FEVD, historical decomposition and counterfactual reconstruction checks pass.

## Minor Warnings

The three warnings are documentation and output-selection warnings:

1. old unpolished figures remain in the replication package but should not be used as final manuscript figures;
2. the polishing script uses output-driven ordering for labels, while the source model ordering is audited in the model scripts;
3. the raw counterfactual scenario table contains additional scenarios, but the main manuscript selection is CF1, CF4 and CF6.

## External-Facing Conclusion

The empirical pipeline is coherent. The warnings do not affect the model, the structural analysis, the historical decomposition or the counterfactual results.
