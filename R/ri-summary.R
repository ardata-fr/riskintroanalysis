

# Method -----------------------------------------------------------------------

#' Overall risk score for study EUs
#'
#' The overall risk displayed in the risk summary tab is calculated from the other
#' default risk/score values. The overall score is determined by the method chosen
#' by the user see `method` parameter.
#'
#' This is the case unless an overriding risk score has been added for that EU.
#'
#' @param epi_units study EUs sf object
#' @param risk_table risk table
#' @param method aggregation method
#'
#' @returns an sf table object with risk scores and EU polygons
#' @noRd
#'
#' @example examples/ri-summary.R
overall_risk <- function(epi_units, risk_table, method){

  method_func <- switch(
    method,
    "mean" = function(x) safe_stat(x, FUN = mean, NA_value = NA_real_),
    "max" = function(x) safe_stat(x, FUN = max, NA_value = NA_real_),
    "min" = function(x) safe_stat(x, FUN = min, NA_value = NA_real_)
  )

  epi_units_risk <- left_join(epi_units, risk_table, by = "eu_id", relationship  = "one-to-one")

  out <- epi_units_risk |>
    rowwise() |>
    mutate(overall_risk = method_func(c_across(starts_with("ri_")))) |>
    mutate(overall_risk = coalesce(.data$ri_override, .data$overall_risk)) |>
    ungroup()
  out
}


#' @title Summarise risk score
#' @description
#' Summarise risk scores across columns using the chosen method.
#' @param risk_table dataset containing all risk data for each epidemiological unit
#' @param cols columns to summarise
#' @param method summary method such as `"mean"` or `"max"`.
#' @param name_to name of overall risk column, defaults to `"overall_risk"`
#' @param keep_cols whether to keep `cols` or remove them.
#' @param overwrite_table a dataframe containing 3 columns: `eu_id`,
#' `overwrite_risk`, and `overwrite_risk_comm`. This dataset will be joined to
#' `risk_table` and `overwrite_risk` values will take precedence over calculated
#' risk values. This allows for analysts with specific information about certain
#' epidemiological units to modify the overall risk score. `overwrite_risk_comm` column
#' contains comments on why this decision was made.
#' @export
#' @importFrom dplyr rowwise mutate c_across ungroup all_of
summarise_scores <- function(
    risk_table,
    cols = NULL,
    method = c("mean", "max", "min", "median"),
    name_to = "overall_risk",
    keep_cols = FALSE,
    overwrite_table = NULL
    ){
  method <- match.arg(method)
  method_func <- switch(
    method,
    "mean" = function(x) safe_stat(x, FUN = mean, NA_value = NA_real_),
    "max" = function(x) safe_stat(x, FUN = max, NA_value = NA_real_),
    "min" = function(x) safe_stat(x, FUN = min, NA_value = NA_real_),
    "median" = function(x) safe_stat(x, FUN = median, NA_value = NA_real_)
  )

  cols <- cols %||% attr(risk_table, "risk_cols")

  out <- risk_table |>
    rowwise() |>
    mutate("{name_to}" := method_func(c_across(all_of(cols)))) |>
    ungroup()

  if (!is.null(overwrite_table)) {
    cli_abort_if_not(
      "{.arg overwrite_table} should have {.arg eu_id} column" = "eu_id" %in% colnames(overwrite_table),
      "{.arg eu_id} column of {.arg overwrite_table} should be character" =
        is.character(overwrite_table$eu_id),
      "{.arg overwrite_table} should have {.arg overwrite_risk} column" =
        "overwrite_risk" %in% colnames(overwrite_table),
      "{.arg overwrite_risk} column of {.arg overwrite_risk} should be numeric" =
        is.numeric(overwrite_table$overwrite_risk),
      "{.arg overwrite_table} should have {.arg overwrite_risk_comm} column" =
        "overwrite_risk_comm" %in% colnames(overwrite_table),
      "{.arg overwrite_risk_comm} column of {.arg overwrite_table} should be character" =
        is.character(overwrite_table$overwrite_risk_comm)
    )

    out <- dplyr::left_join(
      out, overwrite_table,
      by = "eu_id"
    )

    out <- mutate(
      out,
      "{name_to}" := if_else(
        !is.na(.data[["overwrite_risk"]]),
        .data[["overwrite_risk"]],
        .data[[name_to]])
    )
    attr(out, "has_overwrite") <- TRUE
  } else {
    attr(out, "has_overwrite") <- FALSE
  }

  if (!keep_cols) {
    out <- select(out, -all_of(cols))
    attr(out, "risk_cols") <- NULL
  } else {
    attr(out, "risk_cols") <- cols
  }



  attr(out, "scale") <- attr(risk_table, "scale")
  attr(out, "risk_col") <- name_to
  attr(out, "table_name") <- "risk_table"
  out
}


