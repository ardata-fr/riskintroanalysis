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
#' @return an `sf` dataset containing the following columns:
#' -  `eu_id`: epidemiological units id (from `epi_units` dataset)
#' -  `eu_name`: epidemiological units name (from `epi_units` dataset)
#' -  `entry_points_risk `: weighted entry point risk score
#' -  `risk_sources`: informative HTML labels to be used in Leaflet plots
#'
#' This dataset also has a **number of attributes** that are used in other
#' functions from `riskintroanalysis` to make passing dataset metadata between
#' functions more user-friendly.
#'
#' 1. `points`: is a `sf` dataset containing describing the entry points and their
#' associated emission risk. It can be easily accessed with [extract_point_risk()]
#' and has the following columns:
#'    - `point_id`:
#'    - `point_name`:
#'    - `mode`:
#'    - `type`:
#'    - `source`:
#'
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
#' @example examples/ri-entry-point-risk.R
calc_entry_point_risk <- function(
    entry_points,
    epi_units,
    emission_risk
    ) {
  er <- select(emission_risk, all_of(c("iso3", "country", "emission_risk")))
  points_er <- left_join(
    entry_points,
    er,
    by = c(sources = "iso3")
  )

  # Warn if missing emission risk
  missing_emission_risk <- points_er[is.na(points_er$emission_risk), "sources", drop = TRUE]
  if (length(missing_emission_risk > 0)) {
    counts_list <- split(missing_emission_risk, missing_emission_risk) |>
      map(length)
    msg <- paste0(names(counts_list), " missing for ", counts_list, " entry points.")
    warn_msg <- setNames(msg, rep("*", length(msg)))
      cli_inform(c(
      "!" = "There are missing emission risk scores for the following countries:",
      warn_msg,
      "Create new entries in the emission risk factor table using {.help [{.fun erf_row}](riskintrodata::erf_row)}."
    ))
  }

  # Emission risk summarised per point
  points_labeled <- points_er |>
    group_by(across(all_of(c("point_id", "point_name")))) |>
    summarise_quiet(
      point_emission_risk = safe_stat(.data$emission_risk, FUN = mean, NA_value = NA_real_),
      points_label =
        paste0(
          "<strong>", first(.data$point_name),"</strong>", ' (', first(.data$point_id), ')', "<br>",
          'Weighted emission risk: ', fmt_num(.data$point_emission_risk), "/12", "<br>",
          'Mode: ', first(.data$mode), "<br>",
          'Type: ', first(.data$type), "<br>",
          'Risk sources (emission risk score):', "<br>",
          '<ul>',
          paste0(
            "<li>", .data$sources, " - ", .data$country, " (", fmt_num(.data$emission_risk), "/12)", "</li>",
            collapse = ""
          ),
          '</ul>'
        ) |>
        map(HTML),
      .groups = "drop"
    )

  # points_labeled |>st_drop_geometry() |> filter(.by = point_id, n()>1) |> View()

  points <- select(points_labeled, -all_of("points_label"))

  # Find which point is in which epi unit
  eu_ep_intersects <- st_join_quiet(epi_units, points, join = st_intersects)

  # Some points lay outside of the EUs, in this case they are matched to their nearest EU
  unmatched_points <- points |>
    filter(!.data$point_id %in% eu_ep_intersects$point_id)

  ep_eu_nearest <- st_join_quiet(unmatched_points, epi_units, join = st_nearest_feature)|>
    # Drop points geometry
    st_drop_geometry() |>
    # Join polygon geometry back on
    left_join(epi_units |> select("eu_id"), by = "eu_id")

  eu_ep_src_risk <- bind_rows(eu_ep_intersects, ep_eu_nearest)

  dataset <- eu_ep_src_risk |>
    group_by(across(all_of(c("eu_id", "eu_name")))) |>
    summarise(
      entry_points_risk = safe_stat(.data$point_emission_risk, FUN = max),
      entry_points_li = paste0(
        "<li>",
        .data$point_name, " (", fmt_num(.data$point_emission_risk), "/12)",
        "</li>",
        collapse = ""
      ),
      .groups = "drop"   # drop the grouping so result is a plain sf
    ) |>
    mutate(
      entry_points_risk_label = paste0(
        "<strong>", .data$eu_name, "</strong> (", .data$eu_id, ")<br>",
        "Intro risk: ", fmt_num(.data$entry_points_risk), "/12<br>",
        "Points (emission risk):<br><ul>",
        .data$entry_points_li,
        "</ul>"
      ) |>
        map(HTML),
      entry_points_li = NULL
    )


  attr(points_labeled, "risk_col") <- "point_emission_risk"
  attr(points_labeled, "scale") <- c(0, 12)
  attr(dataset, "points") <- points_labeled

  attr(dataset, "risk_col") <- "entry_points_risk"
  attr(dataset, "table_name") <- "entry_points"
  attr(dataset, "scale") <- c(0,12)
  dataset
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
