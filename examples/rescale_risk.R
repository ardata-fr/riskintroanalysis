library(riskintrodata)
library(riskintroanalysis)

dat <- data.frame(RISK = seq(0, 12, by = 0.01))

dat_scaled <- rescale_risk_scores(
  dataset = dat,
  cols = "RISK",
  method = "linear",
  from = c(0, 12),
  to = c(0, 100),
  names_prefix = "sigmoid_"
)

ggplot(dat_scaled, aes(x = RISK, y  = sigmoid_RISK)) +
  geom_point()
