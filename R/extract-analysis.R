
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
#'
#' @name extract-analysis
NULL

#' @export
#' @rdname extract-analysis
#' @importFrom rlang has_name
extract_point_risk <- function(x){
  cli_abort_if_not(
    "{.arg x} has class {.cls {class(x)}} and should be class {.cls riskintro_analysis}" = inherits(x, "riskintro_analysis"),
    "Point risks not available for this risk analysis" = rlang::has_name(x, "points")
  )
  x[["points"]]
}

#' @export
#' @rdname extract-analysis
#' @importFrom rlang has_name
extract_flow_risk <- function(x){
  cli_abort_if_not(
    "{.arg x} has class {.cls {class(x)}} and should be class {.cls riskintro_analysis}" = inherits(x, "riskintro_analysis"),
    "Flows risks not available for this risk" =  rlang::has_name(x, "flows")
  )
  x[["flows"]]
}

#' @export
#' @rdname extract-analysis
#' @importFrom rlang has_name
extract_intro_risk <- function(x){
  cli_abort_if_not(
    "{.arg x} has class {.cls {class(x)}} and should be class {.cls riskintro_analysis}" = inherits(x, "riskintro_analysis"),
    "Introduction risk not available for this risk" = rlang::has_name(x, "ri")
  )
  x[["ri"]]
}
