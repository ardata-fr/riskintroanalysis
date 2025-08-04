#' @title Calculate entry point risk
#' @description
#' Calculating the entry point risk associated with each epidemiological unit.
#'
#' Firstly, an aggregated (`points_agg_fun`) emission risk is calculated for each entry point. This is
#' because an entry point can have multiple sources (countries) giving emission
#' risk.
#'
#' Then, each entry point is allocated to an epidemiological unit. This is done
#' by geospatial means, if an entry point is located inside an epidemiological unit's
#' area it is thus allocated to it. Entry points not located inside any epidemiological
#' unit are allocated to the nearest one.
#'
#' Finally, now that each entry point has an emission risk and has been paired
#' with an epidemiological unit, the aggregated (`eu_agg_fun`) risk score for each epidemiological
#' unit is calculated. Giving the final risk of introduction by entry points for
#' each epidemiological unit.
#'
#' @param entry_points The entry points dataset as formatted and validated by
#' [riskintrodata::mapping_entry_points()]. This should be an `sf` object containing points and emission risks.
#' @param epi_units The epidemiological units dataset as formatted and validated by
#' [riskintrodata::mapping_epi_units()]. This should be an `sf` object with polygons.
#' @param emission_risk The emission risk dataset as returned by the [calc_emission_risk()]
#' function.
#' @param points_agg_fun Function used to aggrgate emission risk for each entry
#' point. See description above. Default is [mean()].
#' @param eu_agg_fun Function used to aggregate emission risk scores for each
#' epidemiological unit. Default is [max()].
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
#' 1. `points`: is an `sf` dataset containing the aggregated emission risk score
#' for each point.  It can be easily accessed with [extract_point_risk()]
#' and has the following columns:
#'    - `point_id`: unique identifier for entry points
#'    - `point_name`: names of entry points
#'    - `mode`: legality or illegality of the entry point
#'    - `type`: transport type of the entry point
#'    - `source`: string of concatenated source countries of entry point
#'    - `points_label `: HTML label for use in leaflet tooltips
#'    - Also attributes: `risk_col = "point_emission_risk"` and
#'     `risk_scale = c(0,12)`
#'
#' 2. `risk_col = "entry_points_risk"` used by [plot_risk()]
#' 3. `table_name = "entry_points"`used by [plot_risk()]
#' 4. `scale = c(0, 12)` used by [plot_risk()] and [rescale_risk_scores()]
#'
#' @export
#' @importFrom stats na.omit
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select left_join filter bind_rows
#' @example examples/ri-entry-point-risk.R
calc_entry_point_risk <- function(
    entry_points,
    epi_units,
    emission_risk,
    points_agg_fun = mean,
    eu_agg_fun = max
    ) {

  check_dataset_valid(epi_units)
  check_dataset_valid(entry_points)
  check_dataset_valid(emission_risk)

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
      cli_warn(c(
      "!" = "There are missing emission risk scores for the following countries:",
      warn_msg,
      "Create new entries in the emission risk factor table using {.help [{.fun erf_row}](riskintrodata::erf_row)}."
    ))
  }

  # Emission risk summarised per point
  points_labeled <- points_er |>
    group_by(across(all_of(c("point_id", "point_name")))) |>
    summarise_quiet(
      point_emission_risk = safe_stat(.data$emission_risk, FUN = points_agg_fun, NA_value = NA_real_),
      mode = paste(unique(.data$mode), collapse = ", "),
      type = paste(unique(.data$type), collapse = ", "),
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
      entry_points_risk = safe_stat(.data$point_emission_risk, FUN = eu_agg_fun, NA_value = NA_real_),
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
