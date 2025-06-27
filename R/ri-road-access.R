
# Method -----------------------------------------------------------------------

#' Aggregate raster values over polygon areas
#'
#' Takes the mean of raster values in an area, ignoring any missing values.
#'
#' @param p polygon data, usually epidemiologcal units (EU).
#' @param r raster data to take the aggregate of each polygon in `p`.
#' @param new_col name of new column containing aggregated raster data
#'
#' @return returns the polygon data (sf) with a new column containing the
#' aggregated values from the raster data for each polygon.
#' @importFrom stats median
augment_epi_units_from_raster <- function(
    p, r, new_col = "risk") {

  p_splat <- as_Spatial(p)
  p_splat <- vect(p_splat)
  aggs <- zonal(r, p_splat, fun = "mean", na.rm=TRUE)[[1]]
  p[[new_col]] <- aggs
  p
}


# Leaflet ----------------------------------------------------------------

#' Plot risk access
#'
#' @param ll leaflet or leaflet proxy to add polygons and raster to
#' @param dat sf dataset with risks
#' @param r splatRaster obj
#'
#' @return leaflet plot with legend
#' @importFrom leaflet addLegend addPolygons colorBin leafletProxy
updateRoadAccessLeaflet <- function(ll, dat, r) {

  pal <- risk_palette()

  label_content <- paste0(
    "<strong>", dat$eu_name, "</strong>", "<br>",
    "Index average: ", fmt_num(dat$road_access_risk),"<br>",
    "Risk score: ", fmt_num(dat$ri_road_access), "/100"
  ) |>
    map(HTML)

  ll <- ll |>
    addPolygons(
      data = dat,
      fillColor = ~pal(dat$ri_road_access),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      label = label_content,
      labelOptions = riLabelOptions(),
      group = "polygons"
    ) |>
    addRiskLegend()

  ll <- ll |>
    addRasterImage(
      x = r,
      opacity = 0.7,
      group = "raster"
    )

  ll <- ll |>
    addLayersControl(
      baseGroups = c( "polygons", "raster"),
      overlayGroups = c("Stadia", "Esri"),
      options = layersControlOptions(
        collapsed = FALSE,
        autoZIndex = FALSE),
      position = "bottomright"
    )

  ll
}

# static plot -------
roadAccessRiskStaticPlot <- function(epi_units_agg_scaled, bounds) {

  ggout <- ggplot(epi_units_agg_scaled) +
    geom_sf(aes(fill = .data[["ri_road_access"]]), color = "white") +
    coord_sf()
  ggout <- get_risks_levels_scale(ggout)

  if (isTruthy(bounds)) {
    ggout <- ggout +
      xlim(c(bounds$west, bounds$east)) +
      ylim(c(bounds$south, bounds$north))
  }

  ggout <- ggout  +
    theme(
      legend.position = c(0.85, 0.8)
    )

  ggout <- ggout  +
    labs(
      title = "Road access risks"
    )

  ggout
}
