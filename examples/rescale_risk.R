library(riskintrodata)
library(riskintroanalysis)

dat <- data.frame(RISK = seq(0, 12, by = 0.01))

dat_scaled <- rescale_risk_scores(
  dataset = dat,
  cols = "RISK",
  method = "quadratic",
  from = c(0, 12),
  to = c(0, 100),
  names_prefix = "quadratic_",
  keep_cols = TRUE
)

ggplot(dat_scaled, aes(x = RISK, y  = quadratic_RISK)) +
  geom_point()
