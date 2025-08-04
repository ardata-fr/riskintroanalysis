library(riskintrodata)
library(riskintroanalysis)

tunisia_raw <- sf::read_sf(system.file(
  package = "riskintrodata",
  "samples",
  "tunisia",
  "epi_units",
  "tunisia_adm2_raw.gpkg"
))

# Apply mapping to prepare colnames and validate dataset
tunisia <- validate_dataset_content(
  x = tunisia_raw,
  table_name = "epi_units",
  eu_name = "NAME_2",
  geometry = "geom"
) |>
  extract_dataset()

rt <- risk_table(tunisia, scale = c(0, 100))
rt
