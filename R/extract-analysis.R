#' @title Extract analysis subcomponents
#' @description
#'
#' Helper functions to access subcomponents of `riskintro_analysis` objets.
#'
#' @param x analysis object containing multiple steps/components.
#' @return Returns the analysis step depending on the fuction :
#'
#' -  `extract_point_risk(x)` returns the point risk, usually the first step
#' in entry point introduction risk analysis. Contains weighted and aggregated
#' emission risk data for each entry points.
#' -  `extract_flow_risk(x)` returns the flow risk, usually the first step in
#' animal mobility introduction risk analysis. Contains weighted and aggregated
#' emission risk data for each flow.
#' - `extract_raster(x)` returns the raster data cropped to the epidemiological units areas.
#' - `extract_border(x)` returns the border risk data for cropped to the epidemiological units areas.
#' @name extract-analysis
NULL

#' @export
#' @rdname extract-analysis
#' @importFrom rlang has_name
extract_point_risk <- function(x) {

  if (is.null(attr(x, "points"))) {
    cli_warn("No points dataset to extract from {.arg x}")
  }
  attr(x, "points")
}

#' @export
#' @rdname extract-analysis
#' @importFrom rlang has_name
extract_flow_risk <- function(x) {

  if (is.null(attr(x, "flows"))) {
    cli_warn("No flows dataset to extract from {.arg x}")
  }
  attr(x, "flows")
}


#' @export
#' @rdname extract-analysis
#' @importFrom rlang has_name
extract_raster <- function(x) {

  if (is.null(attr(x, "raster"))) {
    cli_warn("No raster dataset to extract from {.arg x}")
  }
  attr(x, "raster")
}

#' @export
#' @rdname extract-analysis
#' @importFrom rlang has_name
extract_border <- function(x) {

  if (is.null(attr(x, "borders"))) {
    cli_warn("No borders dataset to extract from {.arg x}")
  }
  attr(x, "borders")
}
