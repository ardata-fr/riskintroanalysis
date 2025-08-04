library(riskintroanalysis)
library(riskintrodata)

animal_mobility_fp <- system.file(
  package = "riskintrodata",
  "samples",
  "tunisia",
  "animal_mobility",
  "ANIMAL_MOBILITY_raw.csv"
)

animal_mobility_raw <- readr::read_csv(animal_mobility_fp)

animal_mobility <- validate_dataset(
  x = animal_mobility_raw,
  table_name = "animal_mobility",
  o_name = "ORIGIN_NAME",
  o_lng = "ORIGIN_LONGITUDE_X",
  o_lat = "ORIGIN_LATITUDE_Y",
  d_name = "DESTINATION_NAME",
  d_lng = "DESTINATION_LONGITUDE_X",
  d_lat = "DESTINATION_LATITUDE_Y",
  quantity = "HEADCOUNT"
) |>
  extract_dataset()

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

emission_risk_table <- calc_emission_risk(
  emission_risk_factors = emission_risk_factors
)

ri_animal_mobility <- calc_animal_mobility_risk(
  animal_mobility = animal_mobility,
  emission_risk = emission_risk_table,
  epi_units = tunisia,
  method = "mean"
)

plot_risk(ri_animal_mobility)

extract_flow_risk(ri_animal_mobility)
attributes(ri_animal_mobility)
