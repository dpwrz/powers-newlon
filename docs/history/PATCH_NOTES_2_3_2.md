# Fantasy Model 2.3.2 patch notes

- Adds historical OOF signal discovery against both actual FPPG and remaining model residual.
- Exposes structured neutral expected production as an expected-FPPG signal.
- Adds redundancy pruning and stability-aware contextual residual ridge correction.
- Adds bounded decayed PID-like feedback from prior player forecast errors only.
- Tunes feedback gains on an inner prior-season holdout; zero gains are valid and preferred when feedback is noise.
- Adds integral-windup and derivative/correction caps.
- Adds multi-metric selection using MAE, RMSE, starter MAE, Pearson and Spearman correlation.
- Adds a hard per-position production promotion gate against locked 2.3.
- Adds risk/confidence calibration using disagreement, correction magnitude and rolling FPPG volatility.
- Adds 2026 projection-history fields needed for prospective error feedback.
