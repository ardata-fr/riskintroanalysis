#' @title Calculate entry point risk
#' @description
#' Calculating the entry point risk associated with each epidemiological unit.
#'
#' The first step is calculating the points dataset: each
#' point located within an epidemiological unit is linked to one or more source countries from
#' which animals enter into the epidemiological unit. For each point the emission
#' risk of the source countries is averaged. This provides an average emission risk score
#' for each point.
#'
#' The second step is creating the epidemiologcal units introduction risk dataset by
#' joining th points dataset with epidemiological units and
#' finding the average emission risk score for each entry points within each
#' area. This gives a risk of introduction for each epidemiological unit.
#'
#' @param entry_points The entry points dataset as formatted and validated by [apply_mapping()] and
#' [mapping_entry_points()]. This should be an `sf` object containing points and emission risks.
#' @param epi_units The epidemiological units dataset as formatted and validated by [apply_mapping()] and
#' [mapping_epi_units()]. This should be an `sf` object with polygons.
#' @param emission_risk The emission risk dataset as returned by the [calc_emission_risk()]
#' function.
#'
#' @return returns a list of class `ri_analysis` containing two datasets:
#'
#' 1.   Points (name: `points`) dataset contains the aggregated emission risk score for each
#' entry point.
#' 2.   Epidemiological units (name: `epi_units`) dataset containing the weighted
#' average of each of points' emission risk scores within its area, this is the
#' risk of introduction by entry points.
#'
#' @export
#' @importFrom stats na.omit
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select left_join filter bind_rows
calc_entry_point_risk <- function(
    entry_points,
    epi_units,
    emission_risk
    ) {

  entry_point_sources <- entry_points |>
    select(all_of(c("point_id", "sources"))) |>
    st_drop_geometry()
  entry_points <- entry_points |>
    select(-all_of("sources")) |>
    distinct(.data[["point_id"]], .keep_all = TRUE)


  # EPS and EM should never have geometries anyway, but there is a recurring bug
  # that may be caused here.
  entry_point_sources <- st_drop_geometry(entry_point_sources)
  emission_risk <- st_drop_geometry(emission_risk)

  source_emission_risk <-
    left_join(
      entry_point_sources, emission_risk,
      by = c("sources" = "iso3"),
      na_matches = "never",
      relationship = "many-to-one"
    )

  points_emission_risk <-
    left_join(
      entry_points,
      source_emission_risk,
      by = "point_id",
      na_matches = "never"
    )

  if(!inherits(points_emission_risk, "sf")) {
    points_emission_risk <-
      st_as_sf(
        points_emission_risk,
        coords = c("lng", "lat"),
        crs = st_crs(epi_units)
      )
  }

  # Find in which polygon is each point
  eu_ep_intersects <- st_join_quiet(epi_units, points_emission_risk, join = st_intersects)

  # Some points lay outside of the EUs, in this case they are matched to their nearest EU
  unmatched_points <- points_emission_risk |>
    filter(!.data$point_id %in% eu_ep_intersects$point_id)
  ep_eu_nearest <- st_join_quiet(unmatched_points, epi_units, join = st_nearest_feature)|>
    # Drop points geometry
    st_drop_geometry() |>
    # Join polygon geometry back on
    left_join(epi_units |> select("eu_id"), by = "eu_id")

  eu_ep_src_risk <- bind_rows(eu_ep_intersects, ep_eu_nearest)

  risk_per_point <- eu_ep_src_risk |>
    group_by(across(all_of(c("eu_id", "point_id")))) |>
    summarise_quiet(
      point_name = first(.data[["point_name"]]),
      eu_name = first(.data[["eu_name"]]),
      ri_entry_points = safe_stat(.data[["emission_risk"]], FUN = mean),
      point_sources_label = if_else(
        is.na(.data[["ri_entry_points"]]),
        NA,
        paste0(
          first(.data[["point_name"]]),
          ": ",
          paste0(na.omit(.data[["country"]]),
            " (", .data[["mode"]], ") ",
            fmt_num(.data[["emission_risk"]]),
            collapse = ", "
          )
        )
      ),
      .groups = "drop"
      )

  dataset <- risk_per_point |>
    group_by(across(all_of(c("eu_id", "eu_name")))) |>
    summarise_quiet(
      entry_points_risk = safe_stat(.data[["ri_entry_points"]], FUN = max),
      risk_sources = if_else(
        !is.na(.data[["entry_points_risk"]]),
        paste(na.omit(.data[["point_sources_label"]]), collapse = "|||"), # split on ||| in label making for leaflet layer
        "No risk sources"
      ),
      .groups = "drop"
    )

  attr(dataset, "risk_col") <- "entry_points_risk"
  attr(dataset, "table_name") <- "entry_points"
  attr(dataset, "points") <- points_emission_risk
  attr(dataset, "scale") <- c(0,12)
  dataset
}


#' Add source labels to display in leaflet
#'
#' @param ep Entry points table
#'
#' @param eps Entry point sources table
#' @param er Emission risk table
#'
add_source_labels <- function(ep, eps, er) {
  # In case no sources exist yet
  if (all(is.na(eps$source))) {
    return(mutate(ep, source_lab = NA))
  }

  eps_names <- left_join(
      x = filter(eps, !is.na(.data[["source"]])),
      y = select(er, all_of(c("iso3", "country", "emission_risk"))),
      by = c("source" = "iso3"),
      relationship = "many-to-one",
      na_matches = "never"
    ) |>
    filter(!is.na(.data[["country"]])) |>
    mutate(
      dummy = "dummy",
      source_name = paste(
        "<li>",
        paste0(
          .data[["country"]],
          " (", fmt_num(.data[["emission_risk"]]),
          "/12)",
          "</li>"
        )
      )
    )

  eps_labels <- tidyr::pivot_wider(
    eps_names,
    id_cols = all_of("point_id"),
    values_from = all_of("source_name"),
    names_from = all_of("dummy"),
    values_fn = list
  ) |>
    mutate(
      source_lab = map_chr(.data[["dummy"]], function(x) {
        paste(x, collapse = "")
      })
    ) |>
    select(all_of(c("point_id", "source_lab")))

  left_join(ep, eps_labels, by = "point_id", na_matches = "never")
}



# Leaflet ----------------------------------------------------------------
#' @importFrom leaflet addCircleMarkers addLegend addMarkers clearMarkers iconList makeIcon markerOptions
updateEntryPointLayer <- function(ll, dat) {
  iSize <- 16

  entryPointIcons <- iconList(
    ship = makeIcon("icons/ship-solid.svg", "icons/ship-solid.svg", iSize, iSize),
    air = makeIcon("icons/plane-solid.svg", "icons/plane-solid.svg", iSize, iSize),
    land = makeIcon(absolute_path("icons/truck-front-solid.svg"), "icons/truck-front-solid.svg", iSize, iSize),
    contreband = makeIcon("icons/mask-solid.svg", "icons/mask-solid.svg", iSize, iSize),
    walk = makeIcon("icons/person-walking-solid.svg", "icons/person-walking-solid.svg", iSize, iSize),
    na = makeIcon("icons/question-solid.svg", "icons/question-solid.svg", iSize, iSize)
  )

  point_labels <- c("PIF Aerien", "PIF Maritime", "PIF terrestre", "Passage contrebande", "Passage transhumance", "N/A")
  point_images <- c("icons/ship-solid.svg", "icons/plane-solid.svg", "icons/truck-front-solid.svg", "icons/mask-solid.svg", "icons/person-walking-solid.svg", "icons/question-solid.svg")
  point_legend <- HTML(paste0("<img src='", point_images, "'width:18px;height:18px>", point_labels, collapse = "<br/>"))

  dat <- dat |>
    mutate(
      type_icon = case_when(
        .data[["type"]] %in% "PIF Aerien" ~ "air",
        .data[["type"]] %in% "PIF Maritime" ~ "ship",
        .data[["type"]] %in% "PIF terrestre" ~ "land",
        .data[["type"]] %in% "Passage contrebande" ~ "contreband",
        .data[["type"]] %in% "Passage transhumance" ~ "walk",
        # is.na(.data[["type"]]) ~ "na",
        TRUE ~ "na"
      ),
      color = case_when(
        .data[["mode"]] %in% "C" ~ "#f72585",
        .data[["mode"]] %in% "NC" ~ "#7209b7",
        # is.na(.data[["mode"]]) ~ "lightgrey",
        TRUE ~ "lightgrey" )
    )

  label_content <- map(
    paste0(
      "<strong>", dat$point_name, "</strong>", "<br>",
      "Type: ", dat$type, "<br>",
      "Mode: ", dat$mode, "<br>",
      "Source countries: ", "<br>",
      "<ul>", dat$source_lab, "</ul>"
    ),
    HTML
  )

  ll |>
    clearMarkers() |>
    addMarkers(
      data = dat,
      icon = entryPointIcons[dat$type_icon],
      options = markerOptions(
        interactive = FALSE, # Just for decoration
      ),
      layerId = ~ paste0("icon-", point_id),
      labelOptions = riLabelOptions()
    ) |>
    addCircleMarkers(
      data = dat,
      label = label_content,
      fillColor = ~color,
      fillOpacity = 0.8,
      stroke = FALSE,
      radius = 12,
      layerId = ~point_id,
      labelOptions = riLabelOptions()
    ) |>
    addLegend(
      title = "Entry point mode",
      colors = c("#f72585", "#7209b7"),
      labels = c("Legal", "Illegal"),
      layerId = "legend"
    )
}


#' @importFrom leaflet addLegend addPolygons clearShapes colorBin highlightOptions
#' @importFrom stringr fixed
updateEntryPointsPolygonLayer <- function(ll, dat) {
  risk_col <- "ri_entry_points"

  # Hover-over label contents
  label_content <- map(
    paste0(
      "<strong>", dat[["eu_name"]], "</strong>", "<br>",
      "Risk score: ", fmt_num(dat[["ri_entry_points"]]), "/100", "<br>",
      "Contributing entry points:", "<br>",
      "<ul>",
      map(
        dat[["risk_sources"]],
        function(x) {
          split_x <- stringr::str_split_1(x, fixed("|||"))
          paste("<li>", split_x, "</li>", collapse = "")
        }
      ),
      "</ul>"
    ),
    HTML
  )

  pal <- risk_palette()

  ll <- ll |>
    clearShapes() |>
    addPolygons(
      data = dat,
      fillColor = ~ pal(dat[[risk_col]]),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      label = label_content,
      layerId = dat[["eu_id"]], # Used for click events!!
      highlightOptions = highlightOptions(
        weight = 6,
        color = "white",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = FALSE
      )
    )

    ll <- ll |>
    addRiskLegend()
  ll
}

# GGplot -----------------------------------------------------------------------

entryPointsStaticPlot <- function(entry_point_intro_risk, entry_points_with_sources, bounds) {

  ggout <- ggplot(entry_point_intro_risk) +
    geom_sf(aes(fill = .data[["ri_entry_points"]]), color = "white") +
    coord_sf()

  ggout <- ggout  +
    theme(
      legend.position = c(0.85, 0.8)
    )

  # Frame with bounds (use Leaflet boundaries)
  if (isTruthy(bounds)) {
    ggout <- ggout +
      xlim(c(bounds$west, bounds$east)) +
      ylim(c(bounds$south, bounds$north))
  }

  entry_points_with_sources <- st_as_sf(
    entry_points_with_sources,
    coords = c("lng", "lat"),
    crs = st_crs(entry_point_intro_risk),
    remove = FALSE)

  ref_icons <- data.frame(
    type = c("PIF Aerien", "PIF Maritime", "PIF terrestre", "Passage contrebande", "Passage transhumance"),
    unicode_text_symbol = c("\uF21A", "\uF072", "\uF0D1", "\uF6FA", "\uf554")
  )
  entry_points_with_sources <- left_join(
    entry_points_with_sources,
    ref_icons,
    by = c("type")
  )

  ggout <- ggout +
    geom_sf_label(
      data = entry_points_with_sources,
      mapping = aes(label = .data[["unicode_text_symbol"]], fill = .data[["mode"]]),
      family = "Font Awesome",
      size = 11/.pt,
      alpha = .5
    ) +
    scale_fill_manual(
      name = "Introduction risk",
      values = c(C = "#f72585", NC = "#7209b7"),
      labels = c(C = "Legal", "NC" = "Illegal")) +
    guides(fill = guide_legend(override.aes = list(color = "transparent"))) +
    labs(
      title = "Entry Point risk levels for countries"
    )

  ggout
}
