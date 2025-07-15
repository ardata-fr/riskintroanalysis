#' @title Get Shared Borders Between EUs and Bordering Countries
#'
#' @description
#' This function identifies shared borders between epidemiological units (EUs) and neighbouring countries.
#' It accounts for small discrepancies in border alignments and adjusts accordingly using an
#' algorithm that loops over all overlapping and divergent polygons.
#'
#' This can take some time to run.
#'
#' Distances are calculated using projection crs 6933 and uses s2. However, output remains WGS84.
#'
#' @param epi_units An `sf` object containing the epidemiological units. Should containg only polygons.
#' @param eu_id_col A string specifying the column name in `epi_units` that identifies each EU.
#' @param bordering_countries An `sf` object containing the bordering countries.
#' @param bc_id_col A string specifying the column name in `bordering_countries` that identifies each country.
#'
#' @return An `sf` object containing shared borders between EUs and bordering countries,
#' with calculated border lengths and weights.
#'
#' @export
#' @example examples/border-risk.R
calc_border_lengths <- function(
    epi_units,
    eu_id_col,
    bordering_countries,
    bc_id_col
    ) {
  sf_use_s2(TRUE)

  out_list <- list()
  epi_units <- st_transform(epi_units, crs = 6933)
  epi_units <- st_make_valid(epi_units)
  epi_units_sum <- summarise(epi_units) |> st_fill_holes()

  is_border_eu <- c(st_intersects(epi_units, st_cast(epi_units_sum, "MULTILINESTRING"), sparse = FALSE))
  border_eu <- epi_units[is_border_eu, ]

  bordering_countries <- st_transform(bordering_countries, crs = 6933)
  bordering_countries <- st_make_valid(bordering_countries)

  coords <- matrix(c(
    -180, -90,
    180, -90,
    180, 90,
    -180, 90,
    -180, -90 # Closing the polygon
  ), ncol = 2, byrow = TRUE)
  world_polygon <- st_sfc(st_polygon(list(coords)), crs = 4326) |>
    st_transform(crs = 6933) |>
    st_make_valid()

  # for each bordering_countries check each border_eu

  for (i in seq_len(nrow(bordering_countries))) {
    bc <- bordering_countries[i, ]

    # The unaligned borders create either empty zones or overlapping zones, we
    # find these zones using a world_polygon, these areas are then added or subtracted
    # from the BC polygon (as the EU polygon is considered the true border).

    # EU and BC overlapping polygon -> remove from BC
    overlapped_poly <- st_intersection_quiet(bc, epi_units_sum)

    # !EU and !BC -> add to BC polygon
    empty_poly_world <- st_difference(world_polygon, bc) |> st_difference(epi_units_sum)

    # Remove the largest polygon which is the whole world poly so that we have
    # Only the gaps between the BC and EU
    empty_poly <- empty_poly_world |>
      st_as_sf() |>
      st_cast("POLYGON") |>
      mutate(
        area = st_area(.data$x) |> as.double(),
        rowid = row_number()
      ) |>
      arrange(desc(.data$area)) |>
      filter(row_number() != 1) |>
      rename(geometry = .data$x)

    # # Uncomment to view outcome
    # new_leaflet() |>
    #   clearShapes() |>
    #   addPolygons(data = st_transform(empty_poly, crs =4326), color = "yellow", label = "empty" ) |>
    #   addPolygons(data = st_transform(overlapped_poly, crs =4326), color = "purple" , label = 'overlapping') |>
    #   addPolygons(data = st_transform(epi_units_sum, crs = 4326), color = "green", label = 'EUs') |>
    #   addPolygons(data = st_transform(bc, crs = 4326), color = "red", label = "Bordering Country")

    # Update the BC with empty polygons and overlapping polygons
    # Remove the overlapping parts from BC
    updated_bc <- bc

    if (nrow(overlapped_poly) > 0) {
      updated_bc <- st_difference(bc, overlapped_poly)
    }

    # Add the empty polygons to BC
    if (nrow(empty_poly) > 0) {
      updated_bc <- bind_rows(
        select(updated_bc, all_of("geometry")),
        select(empty_poly, all_of("geometry"))
      ) |>
        summarise() |> # Combine the polygons
        st_make_valid()
    }


    updated_bc_ls <- updated_bc |> st_cast("MULTILINESTRING")

    for (j in seq_len(nrow(border_eu))) {
      eu <- border_eu[j, ]
      # print(paste0('bc = ',bc$ISO3, ' compared to , eu row = ', eu$eu_id))

      # Ensures there are no holes in the EU that can cause issues
      eu <- st_fill_holes(eu)

      # Get linestrings to find shared border
      eu_ls <- st_geometry(eu) |> st_cast("MULTILINESTRING")
      eu_ls_border <- st_intersection_quiet(eu_ls, epi_units_sum)

      # The radius of a circle with the same surface area as the target territory
      equivalent_radius <- sqrt(st_area(eu) / pi)

      # Automatic buffer size, as a fraction of the magnitude of the target territory
      buffer_factor <- .005
      buffer_size <- buffer_factor * equivalent_radius

      # Note, snapping EU border linestring to BC polygon (not vice versa)
      # Snapping covers cases where the borders differ by very small amounts
      eu_ls_border_snapped <- st_snap(eu_ls_border, updated_bc, tolerance = buffer_size)

      # Get intersetction of both linstrings to find shared border
      shared_border_collection <- st_intersection_quiet(eu_ls_border_snapped, updated_bc_ls)

      # At this point we know if there is a shared border
      if (length(shared_border_collection) == 0) {
        next
      }

      # Geomertry from st_intersction can be many different things, need to sort out what to do with each case
      type <- st_geometry_type(shared_border_collection)
      if (type %in% "GEOMETRYCOLLECTION") {
        frontiere_line <- st_collection_extract(shared_border_collection, type = "LINESTRING")
        frontiere_line <- st_union_quiet(frontiere_line)

        # For when there is only one line in the geocollection
        if(inherits(frontiere_line, "sfc_MULTILINESTRING")){
          frontiere_line <- st_line_merge(frontiere_line)
        }
      } else if (type %in% "MULTILINESTRING") {
        frontiere_line <- st_line_merge(shared_border_collection)
      } else if (type %in% "LINESTRING") {
        frontiere_line <- shared_border_collection
      } else {
        next
      }

      # Output is EU to country correspondence table with frontier lines and lengths
      # plus a border ID
      new_row <- st_as_sf(frontiere_line) |>
        rename(geometry = .data$x) |>
        mutate(
          eu_id = as.character(eu[[eu_id_col]]),
          bc_id = as.character(bc[[bc_id_col]]),
          border_length = as.double(st_length(.data[["geometry"]])) / 1000,
          .before = 1
        )

      out_list[[paste0(eu[[eu_id_col]], "-", bc[[bc_id_col]])]] <- new_row
    }
  }
  sf_use_s2(FALSE)

  all_borders <- bind_rows(out_list, .id = "border_id") |> st_transform(crs = 4326)

  # Summarise by eu and country pairs and calcute weights of each border

  out <- all_borders |>
    group_by(across(all_of(c("eu_id", "bc_id")))) |>
    summarise(
      border_length = sum(.data[["border_length"]]),
      .groups = "drop"
    ) |>
    group_by(across(all_of(c("eu_id")))) |>
    mutate(
      weight = .data[["border_length"]] / sum(.data[["border_length"]])
    ) |>
    ungroup()

  out
}

#' Calculate weighted border risk
#'
#' Calculates the risk of introduction for each epidemiological unit based on the risk of
#' emission of neighbouring countries. The score is weighted based on the length of
#' shared borders.
#'
#' @param epi_units epidemiological units dataset
#' @param shared_borders shared borders dataset as outputted by [calc_shared_border_lengths()]
#' @param emission_risk emission risk dataset,
#' @param ... empty
#' @param add_html_lables default FALSE, used for leaflet tooltips in the Shiny app.
#'
#' @export
calc_border_risk <- function(
    epi_units,
    shared_borders,
    emission_risk,
    ...,
    add_html_lables = FALSE
) {
  check_dots_empty()

  borders <- label_borders(
    borders = shared_borders,
    epi_units = epi_units,
    emission_risk = emission_risk
    )

  epi_units_border_risk <- epi_units |>
    mutate(eu_id = as.character(.data$eu_id)) |>
    left_join(st_drop_geometry(borders), by = "eu_id") |>
    group_by(across(all_of("eu_id"))) |>
    summarise_quiet(
      eu_name = first(.data$eu_name),
      eu_id = first(.data$eu_id),
      borders = first(.data$border_length),
      border_risk = sum(.data[["border_risk"]], na.rm = TRUE),
      sources_label = paste0(
        "<li>",
        "<strong>", .data$country, "</strong>", " (", fmt_num(.data$border_length), "km)", "<br>",
        "&emsp;", "Emission risk: ", fmt_num(.data$emission_risk), "/12", "<br>",
        "&emsp;", "Weighted risk: ", fmt_num(.data$border_risk), "/12",
        "</li>",
        collapse = ""
      )
    ) |>
    ungroup() |>
    mutate(
      risk_sources_label =
        paste0(
          "<strong>", .data$eu_name, "</strong>", "<br>",
          "<strong>", "Border risk score: ", fmt_num(.data$border_risk), "/12", "</strong>", "<br>",
          if_else(
            is.na(.data$border_risk),
            "No sources of border risk",
            paste(
              "Risk sources:", "<br>",
              "<ul>", .data$sources_label, "</ul>"
            )
          )
        ) |> map(HTML)
    ) |>
    select(-all_of("sources_label"))

  if (!add_html_lables) {
    epi_units_border_risk <- select(epi_units_border_risk, -all_of("risk_sources_label"))
  }

  list(
    borders = borders,
    epi_units = epi_units_border_risk
  )
}

#' @importFrom shiny HTML
label_borders <- function(borders, epi_units, emission_risk) {
  border_risks <- borders |>
    left_join(emission_risk, by = c("bc_id" = "iso3")) |>
    left_join(epi_units |> st_drop_geometry() |> mutate(eu_id = as.character(.data[["eu_id"]])),
              by = "eu_id"
    ) |>
    mutate(
      border_risk = .data$emission_risk * .data$weight,
      border_label = paste0(
        "<strong>", "&#x61C", .data$eu_name, " - ", .data$country, "</strong>", "<br>",
        "Neighbour emission risk: ", fmt_num(.data$emission_risk), "/12<br>",
        "Weighted emission risk: ", fmt_num(.data$border_risk), "/12<br>",
        "Border length (km): ", fmt_num(.data$border_length)
      ) |>
        map(HTML)
    ) |>
    select(-all_of("eu_name"))
  border_risks
}


# Leaflet ---------------------------------------------------------------

#' @importFrom leaflet addPolygons
updateBorderRisk <- function(ll, dat, borders) {
  pal <- risk_palette()
  pal12 <- risk_palette_12()

  ll <- ll |>
    leaflet::clearShapes()

  ll <- ll |>

    addPolygons(
      data = dat,
      stroke = TRUE,
      weight = 1,
      opacity = 0.6,
      color = "white",
      fillColor = ~ pal(dat$ri_border_risk), # from 0 to 100
      fillOpacity = 0.7,
      label = ~risk_sources_label,
      layerId = dat$eu_id,
      labelOptions = riLabelOptions()
    )

  ll <- ll |>
    addMapPane(name = "borders", zIndex = 401) |>
    addPolylines(
      data = borders,
      color = ~ pal12(emission_risk), # from 0 to 12
      weight = 10,
      opacity = 0.9,
      label = ~border_label,
      layerId = paste(borders$bc_id, "-", borders$eu_id),
      options = pathOptions(pane = "borders"),
      labelOptions = riLabelOptions()
    )

  ll <- ll |>
    addRiskLegend() |>

    # 0 to 12 legend.
    addLegend(
      data = data.frame(
        labels = c(
          "High [9, 12]",
          "Medium [6, 9)",
          "Low [3, 6)",
          "Negligable [0, 3)",
          "No information"
        ),
        colors = pal12(c(11, 8, 3, 1, NA))
      ),
      title = "Frontier risk",
      colors = ~colors,
      labels = ~labels,
      layerId = "frontier_risk"
    )


  ll
}



# Method used in data-raw ------------------------------------------------------

#' Calculate shared border length of two sf objects
#'
#' Does not take into account altitude in final distiance.
#'
#' NB1: that comparisons to Wikipedia show the distance is underestimated by
#' up to 20% (see examples). This depends on the resolution used in the shapefiles.
#' Higher resolution means more irregularities, which means more length.
#'
#' NB2: this does not work with all geoboundaries data. In testing, there was no solid border
#' shared between Libya and Tunisia. However it works with GISCOE data, as used in
#' examples.
#'
#' @param dat an sf object with one line per area to measure the distance along. For example,
#' an sf with a line for the EU and each bordering country.
#' @param eu string, the EU of interest. Borders and their lengths  will be found
#' with all polygons ajacent it this one.
#' @param country_col sting, column of `dat` containing country names, including `eu`
#'
#' @return sf table with the following structure:
#'
#' | Variable    | Type  | Description          |
#' |-------------|-------|----------------------|
#' | `border_id` | str   | Primary Key (PK)     |
#' | `eu_id`     | str   | Foreign Key (FK) referencing `epiunits` |
#' | `country_id`| str   | Foreign Key (FK) referencing `neighbours` |
#' | `length`    | Real  | Length in kilometers |
#' @noRd
#' @examples
#'
#' if (require("giscoR") & require("ggplot2")) {
#'   x <- giscoR::gisco_get_countries(resolution = "01", country = c("TUN", "DZA", "LBY"))
#'   y <- get_border_linestring(x, eu = "TUN", country_col = "ISO3_CODE")
#'
#'   ggplot() +
#'     geom_sf(data = x) +
#'     geom_sf(data = y, aes(color = border_id)) +
#'     geom_sf_label(
#'       data = y,
#'       aes(label = paste0(
#'         "id: ", border_id,
#'         "length: ", round(length), "km"
#'       ))
#'     )
#' }
#'
get_border_linestring <- function(dat, eu, country_col) {
  if (nrow(dat) <= 1) {
    stop("`dat` requires at least 2 rows to compare")
  }
  if (!eu %in% pull(dat, .data[[country_col]])) {
    stop("`country_col` col should be in `dat`")
  }

  non_primary <- filter(dat, .data[[country_col]] != eu)
  primary <- filter(dat, .data[[country_col]] == eu)

  if (nrow(primary) == 0) {
    stop("value of `eu` not found in `dat[[country_col]]`")
  }

  out <- list()
  for (i in seq_len(nrow(non_primary))) {
    non_primary_country <- pull(non_primary, {{ country_col }})[[i]]

    # Combine dataframes from only two countries at a time
    both_countries <- bind_rows(
      non_primary |> filter(row_number() == i),
      primary
    )

    # Get the perimeters of both countries
    both_perimetres <- st_cast(both_countries, "MULTILINESTRING")
    # Check which perimeters overlap (shared borders)
    common_border <- st_intersection_quiet(both_perimetres) |>
      filter(.data[["n.overlaps"]] == 2)
    # If there are multiple shared borders, bring them all onto one line.
    common_border <- summarise(common_border) |>
      # Add ids and length
      mutate(
        border_id = paste0(eu, "-", non_primary_country),
        country_id = non_primary_country,
        length = as.double(st_length(.data[["geometry"]])) / 1000
      )

    out[[i]] <- common_border
  }

  out <- bind_rows(out) |> # one line per country-ue pair
    mutate(eu_id = eu)
  out
}


#' Create Neighbours Table
#'
#' Can take around 10 minutes to run. Should only be used as a table integrated
#' into the RiskIntro `R/sysdata.rda` data.
#'
#' This function is used in the `raw-data/countries.R`.
#'
#' @param countries_sf sf table of all countries
#'
#' @return The `neighbours` table with all the countries in the world and their
#' land neighbours including the linear length of their shared border in km
#'
#' #' | Variable    | Type  | Description          |
#' |-------------|-------|----------------------|
#' | `border_id` | str   | Primary Key (PK)     |
#' | `country_id`     | str   | Foreign Key (FK) referencing `epiunits` |
#' | `neighbour_id`| str   | Foreign Key (FK) referencing `neighbours` |
#' | `border_length`    | Real  | Length in kilometers |
#'
#'
#' @details
#'
#' Comparing the calculated borders to those on Wikipedia:
#'
#' | **Border**      | **Calculated Length (km)** | **Wikipedia Length (km)** | **Difference (km)** | **Percentage Difference (%)** |
#' |------------------|---------------------------|---------------------------|---------------------|-------------------------------|
#' | Austria-Germany  | 551.0                    | 784.0                    | -233.0             | -29.72                        |
#' | Austria-Italy    | 306.0                    | 430.0                    | -124.0             | -28.84                        |
#' | Austria-Hungary  | 206.0                    | 366.0                    | -160.0             | -43.72                        |
#' | Austria-Czechia  | 286.0                    | 362.0                    | -76.0              | -20.99                        |
#' | Austria-Slovenia | 237.0                    | 330.0                    | -93.0              | -28.18                        |
#' | Austria-Switzerland | 125.0                 | 164.0                    | -39.0              | -23.78                        |
#' | Austria-Slovakia | 77.3                     | 91.0                     | -13.7              | -15.05                        |
#' | Austria-Liechtenstein | 28.3                | 35.0                     | -6.7               | -19.14                        |
#'
#' Note: https://r-spatial.org/r/2020/06/17/s2.html
#'
#' @noRd
#' @examples
#'
#' if (interactive()) {
#'   countries_sf <- giscoR::gisco_get_countries(year = "2024") |>
#'     dplyr::select(ISO3 = ISO3_CODE)
#'   neighbours_table <- create_neighbours_table(countries_sf)
#' }
#'
#' @noRd
create_neighbours_table <- function(countries_sf) {
  countries_sf_linestring <- st_cast(countries_sf, "MULTILINESTRING")
  all_countries <- countries_sf_linestring$ISO3

  all_borders <- map(
    all_countries,
    function(country) {
      get_neighbours(country, countries_sf_linestring)
    },
    .progress = "Compiling neighbours table"
  )

  all_borders <- bind_rows(all_borders)
  rownames(all_borders) <- NULL

  all_borders
}

get_neighbours <- function(iso3, countries_sf_linestring) {
  all_other_countries <- countries_sf_linestring$ISO3[countries_sf_linestring$ISO3 != iso3]

  all_borders <- map(
    all_other_countries,
    function(other_country) {
      suppressMessages(
        get_a_neighbour(iso3, other_country, countries_sf_linestring)
      )
    }
  )

  bind_rows(all_borders)
}

get_a_neighbour <- function(pISO3, sISO3, countries_sf_linestring) {
  two_countries_sf <- countries_sf_linestring |>
    filter(.data[["ISO3"]] %in% c(pISO3, sISO3))

  common_border <- st_intersection_quiet(two_countries_sf) |>
    filter(.data[["n.overlaps"]] == 2)

  if (nrow(common_border) == 0) {
    return(NULL)
  }

  common_border_with_ids <- common_border |>
    mutate(
      country_id = pISO3,
      neighbour_id = sISO3,
      border_id = paste0(.data[["country_id"]], "-", .data[["neighbour_id"]]),
      border_length = as.double(st_length(.data[["geometry"]])) / 1000
    ) |>
    st_drop_geometry() |>
    select(all_of(c("country_id", "neighbour_id", "border_id", "border_length")))

  common_border_with_ids
}


get_country_neighbours <- function(eu_country_code) {
  filter(neighbours_table, .data[["country_id"]] == eu_country_code)
}

# static plot -------
borderRiskStaticPlot <- function(epi_units_border_risk, borders_labeled, bounds) {

  ggout <- ggplot(epi_units_border_risk) +
    geom_sf(aes(fill = .data[["ri_border_risk"]]), alpha = .6, color = "white") +
    coord_sf()
  ggout <- get_risks_levels_scale(ggout)

  ggout <- ggout +
    geom_sf(
      data = borders_labeled,
      mapping = aes(color = .data[["emission_risk_scaled"]]),
      linewidth = 2, show.legend = FALSE)

  if (isTruthy(bounds)) {
    ggout <- ggout +
      xlim(c(bounds$west, bounds$east)) +
      ylim(c(bounds$south, bounds$north))
  }
  ggout <- ggout  +
    theme(
      legend.position = c(0.85, 0.8)
    )
  ggout <- ggout +
    labs(
      title = "Borders risks"
    )
  ggout
}
