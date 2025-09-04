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
  check_dataset_valid(epi_units)
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
#' @param reproject whether to reproject `raster` to `epi_units` projection, default
#' is TRUE.
#'
#' @return returns the polygon data (sf) with a new column containing the
#' aggregated values from the raster data for each polygon.
#' @importFrom stats median
#' @importFrom sf as_Spatial
#' @importFrom terra vect zonal project
#' @export
augment_epi_units_with_raster <- function(
    epi_units,
    raster,
    risk_name = "raster_risk",
    aggregate_fun = c("mean", "max", "min", "sum"),
    reproject = TRUE
    ) {
  aggregate_fun <- match.arg(aggregate_fun)
  p <- epi_units
  r <- raster
  if (reproject) {
    if(is.null(st_crs(p)$input)){
      cli_abort("EPSG code not found for {.arg epi_units}.")
    }
    r <- project(r, st_crs(p)$input)
  }
  r <- terra::crop(r, p, mask = TRUE)
  p_splat <- as_Spatial(p)
  p_splat <- vect(p_splat)
  aggs <- zonal(r, p_splat, fun = aggregate_fun, na.rm=TRUE)[[1]]
  p[[risk_name]] <- aggs
  p
}
