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

entry_points_fp <-
  system.file(
    package = "riskintrodata",
    "samples",
    "tunisia",
    "entry_points",
    "BORDER_CROSSING_POINTS.csv"
  )

entry_points <- readr::read_csv(entry_points_fp)

entry_points <- validate_dataset(
  x = entry_points,
  table_name = "entry_points",
  point_name = "NAME",
  lng = "LONGITUDE_X",
  lat = "LATITUDE_Y",
  mode = "MODE",
  type = "TYPE",
  sources = "SOURCES"
) |>
  extract_dataset()

ri_entry_points <- calc_entry_point_risk(
  entry_points = entry_points,
  epi_units = tunisia,
  emission_risk = calc_emission_risk(
    emission_risk_factors = riskintrodata::get_wahis_erf(
      disease = "Anthrax",
      species = "Cattle",
      animal_category = "Domestic"
    )
  )
) |>
  rescale_risk_scores()

plot_risk(ri_entry_points, interactive = FALSE)
plot_risk(ri_entry_points, interactive = TRUE)
