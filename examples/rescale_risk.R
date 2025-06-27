library(riskintro)

dat <- data.frame(RISK = seq(0, 12, by = 0.01))

dat_scaled <- rescale_risk(
  dataset = dat, risk_col = "RISK",
  method = "linear",
  from = c(0, 12),
  to = c(0, 100)
) |>
  rescale_risk(
    risk_col = "RISK",
    method = "quadratic",
    from = c(0, 12),
    to = c(0, 100)
  ) |>
  rescale_risk(
    risk_col = "RISK",
    method = "exponential",
    from = c(0, 12),
    to = c(0, 100)
  ) |>
  rescale_risk(
    risk_col = "RISK",
    method = "sigmoid",
    from = c(0, 12),
    to = c(0, 100)
  )

if (require(ggplot2) && require(tidyr) && require(tidyselect)) {
  # Plot it !
  dat_scaled |>
    pivot_longer(cols = -RISK, names_to = "method", values_to = "scaled") |>
    ggplot(aes(x = RISK, y = scaled, color = method)) +
    geom_line()
}
