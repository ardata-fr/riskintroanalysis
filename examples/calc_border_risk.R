

\donttest{

library(sf)
library(dplyr)

tunisia_raw <- read_sf(system.file(
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

tun_neighbours <- riskintrodata::neighbours_table |>
  filter(country_id == "TUN")

bordering_countries <- riskintrodata::world_sf |>
  filter(iso3 %in% tun_neighbours$neighbour_id)


# Run function to get shared borders
shared_borders <- calc_border_lengths(
  epi_units = tunisia,
  eu_id_col = "eu_id",
  bordering_countries = bordering_countries,
  bc_id_col = "iso3"
)

wahis_factors <- riskintrodata::get_wahis_erf(
  disease = "Anthrax",
  species = "Cattle",
  animal_category = "Domestic"
)

emission_risk_table <- calc_emission_risk(wahis_factors)

calc_border_risk(
  epi_units = tunisia,
  shared_borders = shared_borders,
  emission_risk = emission_risk_table
)
}
