
calc_animal_mobility_risk <- function(
    animal_mobility,
    emission_risk,
    country_iso3,
    epi_units,
    method
){

  points <- calc_animal_mobility_point_risk(
    animal_mobility = animal_mobility,
    emission_risk = emission_risk,
    country_iso3 = country_iso3
  )

  eu <- calc_animal_mobility_eu_risk(
    epi_units = epi_units,
    method = method
  )

  list(
    points = points,
    epi_units = epi_units
  )
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
#' @param country_iso3 the iso3 code for the country in which the study EUs are situated.
#'
#' @returns A dataframe of aggregated and weighted risk for each pointin the study EUs.
#' @export
#' @importFrom dplyr distinct first filter select all_of
#' @importFrom sf st_as_sf
calc_animal_mobility_point_risk <- function(
    animal_mobility,
    emission_risk,
    country_iso3
    ) {

  inflow <- animal_mobility |>
    # Keep only the flows into Epiunit country.
    filter(.data$o_iso3 != country_iso3, .data$d_iso3 == country_iso3) |>
    # Keep required columns
    select(all_of(c("o_iso3", "o_name", "o_country", "d_name", "quantity")))

  # Manage the geospatial data for each point.
  points <- animal_mobility |>
    select(all_of("d_name"), lng = .data$d_lng, lat = .data$d_lat) |>
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
      .by = .data$d_name,
      o_country = first(.data$o_country),
      weight = .data$quantity / sum(.data$quantity)
    )

  weighted_inflow_risk <- weighted_inflows |>
    left_join(emission_risk, by = c("o_iso3" = "iso3")) |>
    mutate(
      emission_risk_weighted = .data$emission_risk * .data$weight,
      country = if_else(
        is.na(.data$country),
        paste(.data$o_country, "(missing emission risk)"),
        .data$country
      )
    ) |>
    summarise(
      .by = .data$d_name,
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
#' @param animal_mobility_points output of `calc_animal_mobility_point_risk()`
#' @param epi_units study EUs sf object
#' @param method the aggregation method to use
#'
#' @returns the study EUs with associated introduction risk caluclated from point emission risk
#' @export
calc_animal_mobility_eu_risk <- function(
    animal_mobility_points,
    epi_units,
    method
    ) {
  method_func <- switch(method,
    "mean" = mean,
    "max" = max,
    "min" = min
  )

  # Spatial join with epi_units
  epi_units_risk <- st_join_quiet(epi_units, animal_mobility_points, join = st_intersects)

  # Aggregation here needs to be up to use input
  risk_per_epi_unit <- epi_units_risk |>
    group_by(across(all_of(c("eu_id", "eu_name")))) |>
    summarise_quiet(
      animal_movement_risk = method_func(.data$emission_risk_weighted, na.rm = TRUE),
      .groups = "drop"
    )

  risk_per_epi_unit
}


# Leaflet ---------------------------------------------------------------------

updateAnimalMobilityPointsLayer <- function(ll, dat) {
  label_content <- paste0(
    "<strong>", dat$d_name, "</strong>", "<br>",
    "Source countries: ",
    "<ul>",
    dat$source_label,
    "</ul>"
  ) |>
    map(HTML)

  ll <- ll |>
    addCircleMarkers(
      data = dat,
      label = label_content,
      fillColor = "purple",
      fillOpacity = 0.6,
      color = "purple",
      opacity = 1,
      stroke = TRUE,
      weight = 2.5,
      radius = 6,
      layerId = ~paste("destination_points", seq_along(d_name)),
      labelOptions = riLabelOptions()
    )

  ll
}


updateAnimalMobilityPolygonLayer <- function(ll, dat) {
  label_content <- paste0(
    dat$eu_name, "<br>",
    "Risk score: ", fmt_num(dat$ri_animal_movement), "/100"
  ) |>
    map(HTML)

  pal <- risk_palette()

  ll <- ll |>
    addPolygons(
      data = dat,
      fillColor = ~ pal(dat$ri_animal_movement),
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
        bringToFront = FALSE # else points are not on top of polygons
      ),
      labelOptions = riLabelOptions()
    )

  ll <- ll |>
    addRiskLegend()

  ll
}

updateAnimalMovementLines <- function(ll, dat, country_iso3){

  dat <- dat |>
    # Keep only the flows into Epiunit country.
    filter(.data$o_iso3 != country_iso3, .data$d_iso3 == country_iso3)

  if(nrow(dat) < 1){
    show_alert(
      title = "Animal mobility data error",
      text  = paste("Animal mobility data contains no international flows into", country_iso3, ". No risk score for animal mobility will be calculated.")
    )
    return(NULL)
  }

  # Create the geospatial lines between ORIGIN and DESTINATION points
  od_lines <- gcIntermediate(
    as.matrix(dat[, c("o_lng", "o_lat")]),
    as.matrix(dat[, c("d_lng", "d_lat")]),
    n = 300,
    addStartEnd = TRUE,
    sp = TRUE,
    breakAtDateLine = FALSE
  )

  # Convert to sf
  od_lines <- st_as_sf(od_lines)

  # Join to original data
  dat$id <- as.character(c(1:nrow(dat)))
  od_lines$id <- as.character(c(1:nrow(od_lines)))
  dat_sf <- inner_join(od_lines, dat, by = "id")

  ll |>
    addPolylines(
      data = dat_sf,
      weight = 3,
      opacity = 0.7,
      label = ~paste0(
        "From: ", "<strong>", o_name, "</strong>", " (", o_country, " - ", o_iso3 ,")", "<br>",
        "To: ", "<strong>", d_name, "</strong>", " (", d_country, " - ", d_iso3 ,")", "<br>",
        "Quantity:", "<strong>", fmt_num(quantity), "</strong>"
      ) |> map(HTML),
      labelOptions = riLabelOptions()
    ) |>
    addCircleMarkers(
      layerId = ~paste("origin_points", seq_along(animal_mobility_id)),
      data = dat_sf,
      lng = ~o_lng,
      lat = ~o_lat,
      fillColor = "orange",
      fillOpacity = 0.6,
      color = "orange",
      opacity = 1,
      stroke = TRUE,
      weight = 2.5,
      radius = 6,
      label = ~ paste0("Origin point: ", "<strong>",  o_name, "</strong>",  "<br>",
                       "Origin country: ", "<strong>", o_country, "</strong>", " (", o_iso3 ,")") |>
        map(HTML),
      labelOptions = riLabelOptions()
    ) |>
    addLegend(
      data = data.frame(
        labels = c("Origin", "Destination", "Approximate path"),
        colors = c("orange", "purple", "blue")),
      title = "Animal mobility",
      colors = ~colors,
      labels = ~labels,
      layerId = "line_point_legend"
    )

}

# static plot -------
roadAnimalMobilityRiskStaticPlot <- function(animal_mobility, animal_mobility_point_risk, eu_animal_mobility_risk, bounds) {

  od_lines <- gcIntermediate(
    as.matrix(animal_mobility[, c("o_lng", "o_lat")]),
    as.matrix(animal_mobility[, c("d_lng", "d_lat")]),
    n = 300,
    addStartEnd = TRUE,
    sp = TRUE,
    breakAtDateLine = FALSE
  )

  # Convert to sf
  od_lines <- st_as_sf(od_lines)

  # Join to original data
  animal_mobility$id <- as.character(c(1:nrow(animal_mobility)))
  od_lines$id <- as.character(c(1:nrow(od_lines)))
  dat_sf <- inner_join(od_lines, animal_mobility, by = "id")



  ggout <- ggplot(eu_animal_mobility_risk) +
    geom_sf(aes(fill = .data[["ri_animal_movement"]]), color = "white") +
    coord_sf()
  ggout <- get_risks_levels_scale(ggout)

  if (isTruthy(bounds)) {
    ggout <- ggout +
      xlim(c(bounds$west, bounds$east)) +
      ylim(c(bounds$south, bounds$north))
  }

  ggout <- ggout +
    geom_sf(
      data = animal_mobility_point_risk,
      size = 2,
      alpha = 0.6,
      color = "purple"
    )

  ggout <- ggout +
    geom_sf(
      data = dat_sf,
      linewidth = 0.5,
      color = "black",
      alpha = .6
    )
  ggout <- ggout  +
    theme(
      legend.position = c(0.85, 0.8)
    )
  ggout <- ggout  +
    labs(
      title = "Animal mobility risks"
    )
  ggout
}
