library(sf)
library(dplyr)
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

# Run function to get shared borders
shared_borders <- calc_border_lengths(
  epi_units = tunisia
)

wahis_factors <- riskintrodata::get_wahis_erf(
  disease = "Anthrax",
  species = "Cattle",
  animal_category = "Domestic"
)

emission_risk_table <- calc_emission_risk(wahis_factors)

ri_border <- calc_border_risk(
  epi_units = tunisia,
  shared_borders = shared_borders,
  emission_risk = emission_risk_table
)
ri_border

border_lines <- extract_border(ri_border)
border_lines
