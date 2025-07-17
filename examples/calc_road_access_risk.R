
\dontrun{
library(riskintroanalysis)
library(riskintrodata)
library(dplyr)
library(terra)
library(sf)
library(ggplot2)
# Example with raw sf files, previously downloaded with geodata::gadm()
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

road_raster_fp <- download_road_access_raster()
road_raster <- rast(road_raster_fp)

road_access_risk <- calc_road_access_risk(
  epi_units = tunisia,
  road_access_raster = road_raster,
  aggregate_fun = "mean"
)

# plotting the raster data
raster <- extract_raster(road_access_risk)
plot(raster)

# plotting the risk of introduction dataset
risk <- extract_intro_risk(road_access_risk)
ggplot(data=risk, aes(fill = road_access_risk)) + geom_sf()

}
