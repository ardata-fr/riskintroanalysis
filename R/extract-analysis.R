
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
#' -  `extract_intro_risk(x)` returns the dataset containing the risk of introduction
#' for each epidemiological unit. Usually the final step in all risk analyses.
#' - `extract_raster(x)` returns the raster data cropped to the epidemiological units areas.
#' - `extract_border(x)` returns the border risk data for cropped to the epidemiological units areas.
#' @name extract-analysis
NULL

#' @export
#' @rdname extract-analysis
#' @importFrom rlang has_name
extract_point_risk <- function(x){
  cli_abort_if_not(
    "No points dataset to extract from {.arg x}" = !is.null(attr(x, "points"))
  )
  attr(x, "points")
}

#' @export
#' @rdname extract-analysis
#' @importFrom rlang has_name
extract_flow_risk <- function(x){
  cli_abort_if_not(
    "No points dataset to extract from {.arg x}" = !is.null(attr(x, "flows"))
  )
  attr(x, "flows")
}


#' @export
#' @rdname extract-analysis
#' @importFrom rlang has_name
extract_raster <- function(x){
  cli_abort_if_not(
    "No points dataset to extract from {.arg x}" = !is.null(attr(x, "raster"))
  )
  attr(x, "raster")
}

#' @export
#' @rdname extract-analysis
#' @importFrom rlang has_name
extract_border <- function(x){
  cli_abort_if_not(
    "No points dataset to extract from {.arg x}" = !is.null(attr(x, "borders"))
  )
  attr(x, "borders")
}
