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
tunisia <- validate_dataset(
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


algeria <- erf_row(
  iso3 = "DZA",
  country = "Algeria",
  disease = "Avian infectious laryngotracheitis",
  animal_category = "Domestic",
  species = "Birds",
  disease_notification = 0,
  targeted_surveillance = 1,
  general_surveillance = 0,
  screening = 1,
  precautions_at_the_borders = 1,
  slaughter = 1,
  selective_killing_and_disposal = 1,
  zoning = 1,
  official_vaccination = 1,
  last_outbreak_end_date = as.Date("30/06/2023"),
  commerce_illegal = 0L,
  commerce_legal = 0L
)

libya <- erf_row(
  iso3 = "LBY",
  country = "Libya",
  disease = "Avian infectious laryngotracheitis",
  animal_category = "Domestic",
  species = "Birds",
  disease_notification = TRUE,
  targeted_surveillance = 1,
  general_surveillance = 0,
  screening = 1,
  precautions_at_the_borders = 0,
  slaughter = 1,
  selective_killing_and_disposal = 1,
  zoning = 1,
  official_vaccination = 1,
  last_outbreak_end_date = as.Date("30/06/2019"),
  commerce_illegal = 0L,
  commerce_legal = 1
)

wahis_erf <- get_wahis_erf(
  disease = "Avian infectious laryngotracheitis",
  animal_category = "Domestic",
  species = "Birds"
)

emission_risk_factors <- dplyr::bind_rows(
  algeria,
  libya,
  wahis_erf
)
emission_risk_table <- calc_emission_risk(emission_risk_factors)

ri_border <- calc_border_risk(
  epi_units = tunisia,
  shared_borders = shared_borders,
  emission_risk = emission_risk_table
)
ri_border

border_lines <- extract_border(ri_border)
border_lines

plot_risk_interactive(ri_border)
