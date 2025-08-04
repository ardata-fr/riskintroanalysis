

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
#'
#' @export
#' @importFrom dplyr rowwise mutate c_across ungroup
summarise_risk_scores <- function(
    risk_table,
    cols = NULL,
    method = c("mean", "max", "min", "median"),
    name_to = "overall_risk",
    keep_cols = FALSE
    ){
  method <- match.arg(method)
  method_func <- switch(
    method,
    "mean" = function(x) safe_stat(x, FUN = mean, NA_value = NA_real_),
    "max" = function(x) safe_stat(x, FUN = max, NA_value = NA_real_),
    "min" = function(x) safe_stat(x, FUN = min, NA_value = NA_real_),
    "median" = function(x) safe_stat(x, FUN = median, NA_value = NA_real_)
  )

  if (!is.null(cols) && is.null(attr(risk_table, "risk_cols"))) {
    cols <- cols
  } else if (is.null(cols) && !is.null(attr(risk_table, "risk_cols"))) {
    cols <- attr(risk_table, "risk_cols")
  } else if(!is.null(cols) && is.null(attr(risk_table, "risk_cols"))) {
    cli_warn("{.arg risk_cols} attribute of {.arg risk_table} is being overwritten by {.arg cols}")
  } else {
    cli_abort("{.args cols} needs to be provided as {.arg risk_table} has no {.arg risk_cols} attribute")
  }

  out <- risk_table |>
    rowwise() |>
    mutate("{name_to}" := method_func(c_across(cols))) |>
    ungroup()

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

# Visualisation --------------------------------------------------------------
#' @importFrom stringr str_to_sentence
#' @importFrom leaflet highlightOptions colorBin
updateRiskSummaryLeaflet <- function(ll, dat){

  name_labels <- list()
  name_labels$eu_name <- paste0("<strong>", "Name: ", dat$eu_name, "</strong>", "<br>")

  x <- select(dat,starts_with("ri_"), all_of("overall_risk")) |> st_drop_geometry() |>
    select(-.data$ri_override)
  ri_labels <- imap(
    x,
    function(x, name){
      new_name <- gsub("ri_|","", name)
      new_name <- gsub("_"," ", new_name)
      new_name <- str_to_sentence(new_name)
      string <- paste0(new_name, ": ", fmt_num(x), "/100", "<br>")
      strong_string <- if_else(rep(new_name, length(string)) == "Overall risk", paste0("<strong>", string, "</strong>" ), string)
      strong_string
    })

  all_labels <- append(name_labels, ri_labels)
  all_labels$ri_override <- if_else(
    !is.na(dat$ri_override),
    paste0("<strong>", "Override risk: ", dat$ri_override, "/100 </strong>", "<br>"),
    "")
  all_labels$override_com <- if_else(
    !is.na(dat$override_comm),
    paste0("<strong>", "Override comment: ", dat$override_comm, "</strong>", "<br>"),
    "")


  label_content <- do.call(paste0, all_labels) |>
    map(HTML)

  pal <- risk_palette()

  ll <- ll |>
    addPolygons(
      data = dat,
      fillColor = ~ pal(dat$overall_risk),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      label = label_content,
      layerId = dat$eu_id, # Used for click events!!
      highlightOptions = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      labelOptions = riLabelOptions()
    )

  ll <- ll |>
    addRiskLegend()

  ll
}


# GGPLOT -------------------

riskSummaryStaticPlot <- function(ri, bounds){

  ggout <- ggplot(ri) +
    geom_sf(aes(fill = .data[["overall_risk"]]), color = "white") +
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

