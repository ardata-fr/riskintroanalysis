library(dplyr)
library(sf)

# Example epidemiological units (epi_units) as an sf object
epi_units <- st_as_sf(
  data.frame(
    eu_id = c(1, 2, 3, 4),
    geometry = st_sfc(
      st_point(c(0, 0)),
      st_point(c(1, 1)),
      st_point(c(2, 2)),
      st_point(c(3, 3))
    ),
    stringsAsFactors = FALSE
  )
)

# Example risk table with different risk scores
risk_table <- data.frame(
  eu_id = c(1, 2, 3, 4),
  ri_1 = c(0.2, 0.5, NA, 0.8),
  ri_2 = c(0.3, 0.6, NA, 0.9),
  ri_override = c(NA, NA, 0.7, 0.1)
)

# Calculate overall risk using the "mean" method
result <- overall_risk(epi_units, risk_table, method = "mean")

# Print result
print(result)
