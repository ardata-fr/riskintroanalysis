#' Initialise table of risks
#'
#' Creates the base table of risks where each risk associated with a study can be
#' stored. The risk_table defines a master scale that all added risks must fit within.
#'
#' @details
#' The `scale` parameter defines the "universe" of possible risk values for this table.
#' When risks are added via [add_risk()], their scales must be within this range. For
#' example, if you create a risk_table with scale `c(0, 100)`, you can add risks with
#' scales like `c(0, 50)` or `c(20, 80)`, but not `c(0, 120)`.
#'
#' All risks added to the table will inherit this scale attribute, allowing risks with
#' different original scales to coexist as long as their values fit within the defined
#' range. This provides flexibility while maintaining a consistent frame of reference
#' for risk comparison and summarization.
#'
#' @param epi_units epidemiological units dataset
#' @param scale numeric vector of length 2 defining the minimum and maximum possible
#' risk values for this risk table (default: `c(0, 100)`). This establishes the valid
#' range for all risks that will be added to the table.
#'
#' @returns `sf` table with columns `eu_id`, `eu_name` and `geometry`, these all
#' come directly from the `epi_units` argument.
#' @export
#' @example examples/risk_table.R
#' @family risk-table
risk_table <- function(
    epi_units,
    scale = c(0,100)
) {
  cli_abort_if_not(
    "{.arg epi_units} must have attribute {.arg table_name}, it is NULL" = !is.null(attr(epi_units, "table_name")),
    "{.arg epi_units} must have attribute {.arg table_name} = {.val epi_units} " = attr(epi_units, "table_name") == "epi_units"
  )

  risk_table <- epi_units |>
    select(all_of(c(
      "eu_id", "eu_name"
    )))

  attr(risk_table, "scale") <- scale
  attr(risk_table, "table_name") <- "risk_table"
  attr(risk_table, "ri_dataset") <- TRUE
  attr(risk_table, "risk_cols") <- character(0L)
  risk_table
}


#' Add risk to risk table
#'
#' Function to be used with risk table to add risk columns. The risk table
#' is designed to store all risks associated with a riskintro study.
#'
#' @details
#' The risk being added must have a scale that is within (a subset of) the risk_table's
#' scale. For example, if the risk_table has a scale of `c(0, 100)`, you can add risks
#' with scales like `c(0, 50)`, `c(20, 80)`, or `c(0, 100)`. However, you cannot add
#' a risk with scale `c(0, 120)` or `c(-10, 50)` as these exceed the risk_table's range.
#'
#' Once added, the risk column inherits the risk_table's scale attribute, regardless of
#' its original scale. This allows risks with different scales to coexist in the same
#' risk_table, as long as their values fit within the risk_table's scale range.
#'
#' Users are responsible for using [rescale_risk_scores()] to ensure risk values are
#' appropriately scaled before adding them to the risk_table.
#'
#' @param risk_table a risk table initialised with [risk_table()]
#' @param risk_data a data.frame or risk analysis table containing the risk column to
#' be added to the risk table and the joining column (usually `eu_id`).
#' @param cols risk columns in `risk_data` to add to `risk_table`.
#' @param scale risk scale for `cols` in `risk_data`, should be a numeric vector of
#' length 2 (e.g., `c(0, 50)`). This scale must be within the risk_table's scale range.
#' If not provided, it will be extracted from the `risk_data` attributes.
#' @param join_by Name of column to be used to join `risk_data` to `risk_table`.
#' @returns the intial risk table with the new risk column added.
#' @export
#' @family risk-table
#' @importFrom dplyr select any_of all_of left_join
#' @importFrom sf st_drop_geometry
#' @example examples/add_risk.R
add_risk <- function(
    risk_table,
    risk_data,
    cols = NULL,
    scale = NULL,
    join_by = "eu_id"
    ) {
  cols <- cols %||% attr(risk_data, "risk_col")
  if (!is.null(scale) && is.null(attr(risk_data, "scale"))) {
    if (!shinyIsRunning()) {
      cli_inform("Scale attribute of {.arg risk_data} overwritten by {.arg scale} argument.")
    }
  } else {
    scale <- attr(risk_data, "scale")
  }

  cli_abort_if_not(
    "{.arg risk_table} should be the output of {.fn risk_table}" = attr(risk_table, "table_name") == "risk_table",
    "{.arg cols} is NULL, please provided one" = !is.null(cols),
    "{.arg cols} is zero-length" = length(cols) > 0L,
    "{.arg scale} should have length 2" = length(scale) == 2L,
    "{.arg scale} is NULL, please provided one" = !is.null(scale),
    "{.arg join_by} should have length 1" = length(join_by) == 1L,
    "{.arg join_by} must be in `risk_data`" = join_by %in% colnames(risk_data)
  )

  table_scale <- attr(risk_table, "scale")
  if (scale[1] < table_scale[1] || scale[2] > table_scale[2]) {
    cli_abort(paste(
      "Risk scale must be within the risk_table scale range.",
      "Risk scale is {fmt_scale(scale)}, but risk_table scale is {fmt_scale(table_scale)}.",
      "The risk values must fit within the risk_table's scale.",
      "Use {.fn rescale_risk_scores} to adjust the risk scale if needed."
    ))
  }

  preexisting_cols <- cols[cols %in% colnames(risk_table)]
  if (length(preexisting_cols) > 0) {
    risk_table <- select(risk_table, -any_of(preexisting_cols))
  }

  prep_risk_data <- dplyr::select(risk_data, all_of(c(join_by, cols)))
  prep_risk_data <- sf::st_drop_geometry(prep_risk_data)

  out <- left_join(
    x = risk_table,
    y = prep_risk_data,
    by = c(eu_id = join_by),
    relationship = "one-to-one"
  )

  attr(out, "scale") <- attr(risk_table, "scale")
  attr(out, "table_name") <- attr(risk_table, "table_name")
  attr(out, "risk_cols") <- unique(c(attr(risk_table, "risk_cols"), cols))
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
#' @example examples/add_risk.R
remove_risk <- function(risk_table, cols) {
  cli_abort_if_not(
    "{.arg risk_table} should be a risk table." = attr(risk_table, "table_name") == "risk_table",
    "{.arg cols} is zero-length" = length(cols) > 0L,
    "{.arg cols} not found in `risk_table`" = all(cols %in% colnames(risk_table))
  )
  out <- risk_table |> select(-all_of(cols))
  cols_attr <- setdiff(attr(risk_table, "risk_cols"), cols)
  if (length(cols_attr) > 0) {
    attr(out, "risk_cols") <- cols_attr
  } else {
    attr(out, "risk_cols") <- NULL
  }
  out
}


#' Check if a risk table has any risks
#'
#' Helper function to check if a risk table has any risks.
#'
#' @param x a risk table
#' @return TRUE or FALSE
#' @export
has_risk <- function(x) {
  table_name <- attr(x, "table_name")
  if (is.null(table_name) || table_name != "risk_table") {
    cli::cli_abort("{.arg risk_table} should be a risk table.")
  }
  isTruthy(attr(x, "risk_cols"))
}
