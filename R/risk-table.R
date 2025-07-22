#' Initialise table of risks
#'
#' Creates the base table of risks where each risk associated with a study can be
#' stored. Each risk is checked that its on the same scale.
#'
#' @param epi_units epidemiological units dataset
#' @param scale the scale to use for all risk scores added to the risk table. Each
#' new risk added to the table will be checked to ensure the same scale is used. For
#' example, a risk table using a scale from 0 to 100 will not accept a risk on a
#' scale from -1 to 1. This ensures comparability across risks.
#'
#' @returns `sf` table with columns `eu_id`, `eu_name` and `geometry`, these all
#' come directly from the `epi_units` argument.
#' @export
#' @examples
#' @family risk-table
risk_table <- function(
    epi_units,
    scale = c(0,100)
) {

  cli_abort_if_not(
    "{.arg epi_units} should have attribute {.arg table_name}, it is NULL" = !is.null(attr(epi_units, "table_name")),
    "{.arg epi_units} should have attribute {.arg table_name} of {.arg epi_units} " = attr(epi_units, "table_name") == "epi_units",
    "{.arg epi_units} should have been validated by {.fn apply_mapping}" = attr(epi_units, "table_validated")
  )

  risk_table <- epi_units |>
    distinct(.data[["eu_id"]], .keep_all = TRUE) |>
    select(all_of(c(
      "eu_id", "eu_name"
    )))

  attr(risk_table, "scale") <- scale
  attr(risk_table, "table") <- "ri_risk_table"
  attr(risk_table, "risk_cols") <- NULL
  attr(risk_table, "table_validated") <- TRUE
  risk_table
}


#' Add risk to risk table
#'
#' Function to be used with risk table to add risk columns. The risk table
#' is designed to store all risks associataed with a riskintro study.
#'
#' @param risk_table a risk table initialised with [risk_table()]
#' @param risk_data a data.frame or risk analysis table containing the risk column to
#' be added to the risk table and the joining column (usually `eu_id`).
#' @param risk_col
#'
#' @returns
#' @export
#' @family risk-table
#' @importFrom dplyr select any_of all_of left_join
#' @importFrom sf st_drop_geometry
#' @examples
add_risk <- function(
    risk_table,
    risk_data,
    cols = NULL,
    scale = NULL,
    join_by = "eu_id",
    overwrite = FALSE
    ) {

  if (is.null(cols)) {
    cols <- attr(risk_data, "risk_col")
  }

  if (!is.null(scale) && is.null(attr(risk_data, "scale"))) {
    cli_warn("Scale attribute of {.arg risk_data} overwritten by {.arg risk_data} argument.")
  } else {
    scale <- attr(risk_data, "scale")
  }

  cli_abort_if_not(
    "{.arg risk_table} should be the output of {.fn risk_table}" = attr(risk_table, "table") == "ri_risk_table",
    "{.arg cols} is NULL, please provided one" = !is.null(cols),
    "{.arg scale} is NULL, please provided one" = !is.null(scale)
  )

  if (!all(scale == attr(risk_table, "scale"))) {
    cli_abort(paste(
      "This risk table expects all new risk scores a scale of {fmt_scale(attr(risk_table, 'scale'))}.",
      "The provided scale in {.arg risk_data} is {fmt_scale(scale)}.",
      "Ensure you are using the right arguments for {.fn rescale_risk_score}",
      "or overwrite the {.arg risk_data} scaling with `scale`"
    ))
  }

  if (overwrite){
    risk_table <- select(risk_table, -select(any_of(cols)))
  }

  # "{.arg cols} already exists in risk table, use {.code overwrite = TRUE}."

  ri_risk <- select(risk_data, all_of(c(join_by, cols))) |>
    st_drop_geometry()

  out <- left_join(
    x = risk_table,
    y = ri_risk,
    by = c(eu_id = join_by),
    relationship = "one-to-one"
  )

  attr(out, "scale") <- attr(risk_table, "scale")
  attr(out, "table") <- attr(risk_table, "table")
  attr(out, "risk_cols") <- c(attr(risk_table, "risk_cols"), cols)
  out
}


#' Remove risk from risk table
#'
#' @param risk_table risk table to remove risk from
#' @param cols refers to columns (risk scores) in the risk table to remove
#'
#' @returns `risk_table` with fewer associated risk scores
#' @export
#' @family risk-table
#' @importFrom dplyr select all_of
#' @examples
remove_risk <- function(risk_table, cols) {
  out <- risk_table |>
    select(-all_of(cols))
  attr(out, "risk_cols") <- attr(out, "risk_cols")[!attr(out, "risk_cols") %in% cols]
  out
}
