
#' @title Calculate animal mobility risk
#'
#' @description
#' Calculate the risk of introduction associated with legal international animal
#' commerce entering epidemiological units.
#'
#' Firstly, an aggregated  emission risk is calculated for each destination point.
#'  This is because a destination point can have multiple sources (countries)
#'  giving emission risk.
#'
#' Then, each destination point is allocated to an epidemiological unit. This is done
#' by geospatial means, if an destination point is located inside an epidemiological unit's
#' area it is thus allocated to it.
#'
#' Finally, now that each destination point has an emission risk and has been paired
#' with an epidemiological unit, the aggregated risk score for each epidemiological
#' unit is calculated, giving the final risk of introduction by animal mobility for
#' each epidemiological unit.
#'
#' @param animal_mobility animal moblity validated by [riskintrodata::validate_dataset()]
#' @param emission_risk emission risk dataset from [calc_emission_risk]
#' @param epi_units epidemiological units dataset
#' @param method aggregation method for eu risk
#' @param eu_country_iso3 optional parameter to be used when epidemiological units
#' do not correspond to a country.
#'
#' @returns an `sf` dataset containing the risk of introduction for each of
#' the epidemiological units. The dataset has the following columns:
#' -  `eu_id`: epidemiological units identifier
#' -  `eu_name`: epidemiological units name
#' -  `animal_mobility_risk`: risk of introduction through animal mobility
#' -  `geometry`: epidemiological units geometry `MULTIPOLYGONS`
#'
#' This dataset also has a **number of attributes** that are used in other
#' functions from `riskintroanalysis` to make passing dataset metadata between
#' functions more user-friendly.
#'
#' 1. `flows`: an `sf` dataset containing the flow destination points, each point
#' has an aggregated emission risk score that is weighted by `quantity`. It contains
#' the following columns:
#'    - `d_name`: destination name
#'    - `emission_risk_weighted`: weighted emission risk for that point
#'    - `source_label`: HTLM label to be used in leaflet tooltips.
#'    - `geometry`: destination point geometries `POINTS`
#'
#' 2. `risk_col = "animal_mobility_risk"` used by [plot_risk()]
#' 3. `table_name = "table_name"`used by [plot_risk()]
#' 4. `scale = c(0, 12)` used by [plot_risk()] and [rescale_risk_scores()]
#'
#' @export
#' @example examples/calc_animal_mobility_risk.R
calc_animal_mobility_risk <- function(
    animal_mobility,
    emission_risk,
    epi_units,
    method = c("mean", "min", "max"),
    eu_country_iso3 = NULL
){
  method <- match.arg(method)

  check_dataset_valid(animal_mobility)
  check_dataset_valid(emission_risk)
  check_dataset_valid(epi_units)

  epi_units_iso3 <- eu_country_iso3 %||% get_eu_country(epi_units)$country_iso3

  # Step 1
  flows_risk <- calc_animal_mobility_flows_risk(
    animal_mobility = animal_mobility,
    emission_risk = emission_risk,
    epi_units_iso3 = epi_units_iso3
  )

  # Step 2
  dataset <- calc_animal_mobility_intro_risk(
    animal_mobility_flows = flows_risk,
    epi_units = epi_units,
    method = method
  )


  attr(flows_risk, "risk_col") <- "animal_mobility_flows"
  attr(flows_risk, "risk_col") <- "emission_risk_weighted"
  attr(flows_risk, "scale") <- c(0, 12)
  attr(dataset, "flows") <- flows_risk
  attr(dataset, "risk_col") <- "animal_mobility_risk"
  attr(dataset, "table_name") <- "animal_mobility"
  attr(dataset, "scale") <- c(0,12)
  dataset
}

#' Animal mobility destination point risk
#'
#' Calculate the emission risk associated with each animal mobility flow that enters
#' the epidemiological units (EU). The quantity of each flow associated to a point
#' inside study EUs is aggregated. The emission risk of a point is then weighted by
#' the quantity of animals coming from the origin country.
#'
#' @param animal_mobility the animal mobility dataset
#' @param emission_risk emission risk data set
#' @param epi_units_iso3 the iso3 code for the country in which the study EUs are situated.
#'
#' @returns A dataframe of aggregated and weighted risk for each pointin the study EUs.
#' @noRd
#' @importFrom dplyr distinct first filter select all_of
#' @importFrom sf st_as_sf
calc_animal_mobility_flows_risk <- function(
    animal_mobility,
    emission_risk,
    epi_units_iso3
    ) {
  inflow <- animal_mobility |>
    filter(.data$o_iso3 != !!epi_units_iso3, .data$d_iso3 == !!epi_units_iso3) |>
    select(all_of(c("o_iso3", "o_name", "o_country", "d_name", "quantity")))

  # Manage the geospatial data for each point.
  points <- animal_mobility |>
    select(all_of("d_name"), lng = "d_lng", lat = "d_lat") |>
    distinct() |>
    latlng_to_sf()

  # Calculate the weighted risk per point per risk source
  sum_inflows <- inflow |>
    group_by(across(all_of(c("o_iso3", "o_country", "d_name")))) |>
    summarise(
      quantity = sum(.data$quantity),
      .groups = "drop"
    )

  weighted_inflows <- sum_inflows |>
    mutate(
      .by = "d_name",
      o_country = first(.data$o_country),
      weight = .data$quantity / sum(.data$quantity)
    )

  weighted_inflows_er <- weighted_inflows |>
    left_join(emission_risk, by = c("o_iso3" = "iso3"))

  # Warn if missing emission risk
  missing_emission_risk <- weighted_inflows_er[is.na(weighted_inflows_er$emission_risk), "o_iso3", drop = TRUE]
  if (length(missing_emission_risk > 0)) {
    counts_list <- split(missing_emission_risk, missing_emission_risk) |>
      map(length)
    msg <- paste0(names(counts_list), " missing for ", counts_list, " animal mobility flows.")
    warn_msg <- setNames(msg, rep("*", length(msg)))
    cli_warn(c(
      "!" = "There are missing emission risk scores for the following countries:",
      warn_msg,
      "Create new entries in the emission risk factor table using {.help [{.fun erf_row}](riskintrodata::erf_row)}."
    ))
  }

  weighted_inflow_risk <- weighted_inflows_er |>
    mutate(
      emission_risk_weighted = .data$emission_risk * .data$weight,
      country = if_else(
        is.na(.data$country),
        paste(.data$o_country, "(missing emission risk)"),
        .data$country
      )
    ) |>
    summarise(
      .by = "d_name",
      emission_risk_weighted = sum(.data$emission_risk_weighted),
      source_label = paste0("<li>", .data$country, " (", fmt_num(.data$emission_risk), "/12)", "</li>", collapse = "")
    )

  # Add geometry
  risk_points <- left_join(points, weighted_inflow_risk, by = "d_name")

  risk_points
}

#' Animal mobility risk for each epidemiological unit (EU)
#'
#' Given the output of the function `calc_animal_mobility_point_risk()` (destination
#' points and their risks) and the study EUs, calculate the risk of introduction into each EU.
#'
#' @param animal_mobility_flows output of `calc_animal_mobility_point_risk()`
#' @param epi_units study EUs sf object
#' @param method the aggregation method to use
#'
#' @returns the study EUs with associated introduction risk caluclated from point emission risk
#' @noRd
calc_animal_mobility_intro_risk <- function(
    animal_mobility_flows,
    epi_units,
    method
    ) {
  method_func <- switch(method,
    "mean" = mean,
    "max" = max,
    "min" = min
  )

  # Spatial join with epi_units
  epi_units_risk <- st_join_quiet(epi_units, animal_mobility_flows, join = st_intersects)

  # Aggregation here needs to be up to use input
  risk_per_epi_unit <- epi_units_risk |>
    group_by(across(all_of(c("eu_id", "eu_name")))) |>
    summarise_quiet(
      animal_mobility_risk = safe_stat(
        .data$emission_risk_weighted,
        FUN = method_func,
        NA_value = NA_real_
        ),
      .groups = "drop"
    )

  risk_per_epi_unit
}
