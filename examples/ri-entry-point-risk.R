library(riskintroanalysis)
library(dplyr)

entry_points_fp <-
  system.file(
    package = "riskintrodata",
    "samples",
    "tunisia",
    "entry_points", "BORDER_CROSSING_POINTS.csv"
  )

entry_points <- readr::read_csv(entry_points_fp)

entry_points <- apply_mapping(
  dataset = entry_points,
  mapping = mapping_entry_points(
    point_name = "NAME",
    lng = "LONGITUDE_X",
    lat = "LATITUDE_Y",
    mode = "MODE",
    type = "TYPE",
    sources = "SOURCES"
  ),
  validate = TRUE
)

tunisia_raw <- sf::read_sf(system.file(
  package = "riskintrodata",
  "samples", "tunisia", "epi_units", "tunisia_adm2_raw.gpkg"
))

# Apply mapping to prepare colnames and validate dataset
tunisia <- apply_mapping(
  tunisia_raw,
  mapping = mapping_epi_units(
    eu_name = "NAME_2",
    geometry = "geom"
  ),
  validate = TRUE
)

algeria <- riskintrodata::erf_row(
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

libya <- riskintrodata::erf_row(
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

wahis_erf <- riskintrodata::get_wahis_erf(
  disease = "Avian infectious laryngotracheitis",
  animal_category = "Domestic",
  species = "Birds"
)

emission_risk_factors <- dplyr::bind_rows(
  algeria,
  libya,
  wahis_erf
)

emission_risk_table <- calc_emission_risk(emission_risk_factors = emission_risk_factors)

ri_entry_points <- calc_entry_point_risk(
  entry_points = entry_points,
  epi_units = tunisia,
  emission_risk = emission_risk_table
)

plot_risk(ri_entry_points)

extract_point_risk(ri_entry_points)

attributes(ri_entry_points)

