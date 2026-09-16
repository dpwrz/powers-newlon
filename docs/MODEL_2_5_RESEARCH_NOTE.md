# Model 2.5 research note

A prototype XGBoost direct challenger was tested against the existing position-specific production champion using strict chronological 2022–2025 folds and the 114-feature pre-kickoff contract now encoded in `R/weekly_engine_25.R`.

The research run found that a conservative boosted blend can improve both MAE and RMSE at QB/RB/WR while preserving the incumbent as an anchor. The strongest gains were at RB and WR; QB gains were smaller but broadly aligned across error and correlation metrics. TE showed aggregate gains but weaker year-to-year RMSE stability, which is exactly the kind of case the 2.5 promotion gate is designed to reject until it proves stable.

Separately, honest role forecasting experiments improved next-week opportunity estimates (pass attempts/carries/targets), and role forecast error is materially related to fantasy residual error. However, directly converting the role gap into a fantasy-point correction was not consistently better for starters. Role forecasting therefore remains a structured challenger/input for the next tournament rather than being automatically promoted into 2.5.

Important: runtime results from the R implementation are authoritative. XGBoost versions and platform details can move exact metrics slightly, so production promotion is determined by `weekly_2_5_promotion.csv`, not by this research note.
