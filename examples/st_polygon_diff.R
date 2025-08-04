if (require("rnaturalearth", quietly = TRUE)) {
  africa <- ne_countries(continent = "africa", returnclass = "sf")

  library(sf)
  library(geodata)

  tunisia <- st_as_sf(gadm(country = "Tunisia", level = 0, path = tempdir()))
  africa_rest <- st_polygon_diff(africa, tunisia)
  nrow(africa)
  nrow(africa_rest)
  plot(st_geometry(africa[africa$name == "Tunisia", ]))
  plot(st_geometry(tunisia), add = TRUE, col = "red")
  plot(st_geometry(africa_rest), col = "lightgrey", add = TRUE)
}
