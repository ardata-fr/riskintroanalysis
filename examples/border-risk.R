library(sf)

# Create example epidemiological units (epi_units)
epi_units <- st_as_sf(
  data.frame(
    EU_ID = c("EU1", "EU2"),
    geometry = st_sfc(
      st_polygon(list(matrix(c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0), ncol = 2, byrow = TRUE))),
      st_polygon(list(matrix(c(2, 0, 4, 0, 4, 2, 2, 2, 2, 0), ncol = 2, byrow = TRUE)))
    ),
    stringsAsFactors = FALSE
  ),
  crs = 4326
)

# Create example bordering countries (bordering_countries)
bordering_countries <- st_as_sf(
  data.frame(
    BC_ID = c("BC1"),
    geometry = st_sfc(
      st_polygon(list(matrix(c(2, 0, 6, 0, 6, 4, 2, 4, 2, 0), ncol = 2, byrow = TRUE)))
    ),
    stringsAsFactors = FALSE
  ),
  crs = 4326
)

# Run function to get shared borders
shared_borders <- calc_border_lengths(
  epi_units = epi_units,
  eu_id_col = "EU_ID",
  bordering_countries = bordering_countries,
  bc_id_col = "BC_ID"
)

# Print output
print(shared_borders)

# Visualize using a simple plot
plot(st_geometry(epi_units), col = "lightblue", border = "blue", main = "Shared Borders")
plot(st_geometry(bordering_countries), col = "pink", border = "red", add = TRUE)
plot(st_geometry(shared_borders), col = "black", lwd = 2, add = TRUE)
