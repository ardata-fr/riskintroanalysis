#' Road access introduction risk analysis
#'
#' Calculates the risk of introduction for an animal disease using an
#' accessibility to roads index.
#'
#' @param epi_units epidemiological units dataset
#' @param road_access_raster road access raster data contains road accessibility
#' index. Can be aquired using [riskintrodata::download_road_access_raster()] and
#'  [terra::rast()].
#' @param aggregate_fun function to use to aggregate raster values over
#' epi units area, default is `mean`.
#'
#' @return list with class `ri_analysis` that contains:
#'
#' 1. `ri`: an `sf` dataset containing the road accessibility risk summarised over each
#' epidemiological unit. Contains the following columns:
#' -  `eu_id`: unique identifier for each epidemiological unit
#' -  `eu_name`: name provided in `epi_units` dataset.
#' -  `road_access_risk`: road access risk of inroduction score (unscaled)
#' -  `geometry`: geometry for each epidemiological unit of type `POLYGON` or `MULTIPOLYGON`.
#'
#' This data can be extracted from the `ri_analysis` object with [extract_raster()]
#'
#' 2. `raster`: a `SpatRaster` containing the cropped raster that covers only
#' the epidemiological units areas.
#'
#' @seealso [extract_raster()], [riskintrodata::download_road_access_raster()]
#' @export
#' @family default riskintro analysis
#' @importFrom terra crop
#' @example examples/calc_road_access_risk.R
calc_road_access_risk <- function(
    epi_units,
    road_access_raster,
    aggregate_fun = c("mean", "max", "min", "sum")
){
  aggregate_fun <- match.arg(aggregate_fun)
  cropped_raster <- crop(road_access_raster, epi_units, mask = TRUE)

  dataset <- augment_epi_units_with_raster(
    epi_units = epi_units,
    raster = cropped_raster,
    aggregate_fun = aggregate_fun,
    risk_name = "road_access_risk"
  )

  attr(dataset, "risk_col") <- "road_access_risk"
  attr(dataset, "table_name") <- "road_access"
  attr(dataset, "raster") <- cropped_raster
  attr(dataset, "scale") <- NULL
  dataset
}

#' Aggregate raster values over polygon areas
#'
#' Takes the mean of raster values in an area, ignoring any missing values.
#'
#' @param epi_units polygon data, usually epidemiologcal units (EU).
#' @param raster raster data to take the aggregate of each polygon in `p`.
#' @param risk_name name of new column containing aggregated raster values
#' @param aggregate_fun function to use to aggregate raster values over
#' epi units area, default is `mean`.
#'
#' @return returns the polygon data (sf) with a new column containing the
#' aggregated values from the raster data for each polygon.
#' @importFrom stats median
#' @importFrom sf as_Spatial
#' @importFrom terra vect zonal
#' @export
augment_epi_units_with_raster <- function(
    epi_units,
    raster,
    risk_name = "raster_risk",
    aggregate_fun = c("mean", "max", "min", "sum")
    ) {
  aggregate_fun <- match.arg(aggregate_fun)
  p <- epi_units
  r <- raster
  r <- terra::crop(r, p, mask = TRUE)
  p_splat <- as_Spatial(p)
  p_splat <- vect(p_splat)
  aggs <- zonal(r, p_splat, fun = aggregate_fun, na.rm=TRUE)[[1]]
  p[[risk_name]] <- aggs
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
