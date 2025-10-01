#' @title Calculate entry point risk
#' @description
#'
#'
#' Calculating the risk of introduction through entry points within each
#' epidemiological unit.
#'
#' See the [Entry points analysis](https://astre.gitlab.cirad.fr/riskintro-app/riskintroanalysis/articles/entry-points-analysis.html)
#' article for more information or run `vignette("entry-points-analysis")`
#'
#' @details
#' If an entry point is not within the area of an epidemiological unit, it
#' will be allocated to the nearest one.
#'
#' @param entry_points The entry points dataset as formatted and validated by
#' [riskintrodata::validate_dataset()]. This should be an `sf` object containing points and emission risks.
#' @param epi_units The epidemiological units dataset as formatted and validated by
#' [riskintrodata::validate_dataset()]. This should be an `sf` object with polygons.
#' @param emission_risk The emission risk dataset as returned by the [calc_emission_risk()]
#' function.
#' @param scaling_args list of arguments to pass to [scale_entry_points()]. Accepted
#' arguments are `illegal_factor`, `coef_legal` and `coef_illegal`. Other arguments
#' are handled internally.
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
    scaling_args = list(
      illegal_factor = 3,
      coef_legal = 1,
      coef_illegal = 1,
      max_risk = 100
    )
) {

  # Check valid data ----
  check_dataset_valid(epi_units)
  check_dataset_valid(entry_points)
  check_dataset_valid(emission_risk)

  scaling_args$illegal_factor <- scaling_args$illegal_factor %||% 3
  scaling_args$coef_legal <- scaling_args$coef_legal %||% 1
  scaling_args$coef_illegal <- scaling_args$coef_illegal %||% 1
  scaling_args$max_risk <- scaling_args$max_risk %||% 100

  er <- select(emission_risk, all_of(c("iso3", "country", "emission_risk")))
  points_er <- left_join(
    entry_points,
    er,
    by = c(sources = "iso3")
  )

  # Often a lot of NAs in the emission scores dataset

  # warn NAs -----
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
      if (shinyIsRunning()) {
       "i" = "Create new entries in the Emission Scores interface."
      } else {
        "i" = "Create new entries in the emission risk factor table using {.help [{.fun erf_row}](riskintrodata::erf_row)}."
      }
    ))
  }

  # handle NA -----
  # Replace missing values for rows not in emission scores dataset
  points_er_complete <- points_er |>
    tidyr::replace_na(replace = list("emission_risk" = 12)) |>
    dplyr::mutate(country = if_else(is.na(.data$country), iso3_to_name(.data$sources), .data$country))

  # Step 1: Point Exposure ----
  # This provides one emission risk score per point.
  # Not that the summary is a weighted average based on the severity of the risk
  # of each country. See n_eff_sources()
  point_exposures <- points_er_complete |>
    group_by(across(all_of(c("point_id", "point_name", "mode", "type")))) |>
    summarise_quiet(
      point_exposure = safe_stat(.data$emission_risk, FUN = n_eff_sources, NA_value = NA_real_),
      points_label =
        paste0(
          "<strong>", first(.data$point_name),"</strong>", "<br>",
          'Risk exposure: ', fmt_num(.data$point_exposure), "<br>",
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

  attr(point_exposures, "leaflet_labels") <- point_exposures$points_label
  points <- select(point_exposures, -all_of("points_label"))
  point_exposures$points_label <- NULL

  # Step 2: Effective number of controlled/not-controlled points -----
  # Allocate each point to an epi_unit and then sum the risk exposures

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

  eu_ep_exposures <- bind_rows(eu_ep_intersects, ep_eu_nearest) |>
    st_drop_geometry() |>
    filter(!is.na(.data$mode))

  total_exposure <- eu_ep_exposures |>
    mutate(mode = factor(mode, levels = c("C", "NC"))) |>
    group_by(across(all_of(c("eu_id", "eu_name", "mode")))) |>
    summarise(
      total_exposure = safe_stat(.data$point_exposure, FUN = sum, NA_value = NA_real_),
      .groups = "drop"
      ) |>
    tidyr::pivot_wider(
      id_cols = all_of(c("eu_id", "eu_name")),
      names_from = all_of("mode"),
      values_from = all_of("total_exposure"),
      names_prefix = 'exposure_'
    )

  # pivot_wider will not pivot NC if there are no
  # values of NC in names_from
  if (!"exposure_C" %in% colnames(total_exposure)) {
    total_exposure$exposure_C <- 0
  }
  if (!"exposure_NC" %in% colnames(total_exposure)) {
    total_exposure$exposure_NC <- 0
  }

  # Step 3: Scaling the risk of introduction -----

  introduction_risk <- total_exposure |>
    tidyr::replace_na(list(
      "exposure_C" = 0,
      "exposure_NC" = 0
    )) |>
    mutate(
      entry_points_risk = scale_entry_points(
        x_legal = .data$exposure_C,
        x_illegal = .data$exposure_NC,
        illegal_factor = scaling_args$illegal_factor,
        coef_legal = scaling_args$coef_legal,
        coef_illegal = scaling_args$coef_illegal,
        max_risk = scaling_args$max_risk
      )
    ) |>
    select(-all_of("eu_name"))

  dataset <- left_join(
    epi_units, introduction_risk,
    by = "eu_id", na_matches = "never",
    relationship = "one-to-one"
  )

  attr(point_exposures, "risk_col") <- "point_exposure"
  attr(point_exposures, "scale") <- c(0, scaling_args$max_risk)
  attr(dataset, "points") <- point_exposures

  attr(dataset, "risk_col") <- "entry_points_risk"
  attr(dataset, "table_name") <- "entry_points"
  attr(dataset, "scale") <- c(0,scaling_args$max_risk)
  dataset
}


#' Effective number of sources of an entry point
#'
#' Equivalent number of sources of maximum emission score.
#'
#' Each source contributes a quantity between 0 and 1, equal to the ratio between its
#' emission score and the maximum possible score.
#'
#' @param x Numeric vector. Risk of emission scores of source countries.
#' @param x_max Positive number. Maximum possible emission score for a country
#' @keywords internal
n_eff_sources <- function(x, x_max = 12) {
  stopifnot(
    length(x_max) == 1,
    x_max > 0
  )
  sum(x) / x_max
}

# out <- filter(points_er, point_id == "ep-00001")


#' Scale risk of entry points
#'
#' Scale the effective numbers of legal and illegal sources into the risk scale. Part
#' of the analysis of entry point risk.
#'
#' See the [Entry points analysis](https://astre.gitlab.cirad.fr/riskintro-app/riskintroanalysis/articles/entry-points-analysis.html)
#' article for more information or run `vignette("entry-points-analysis")`
#'
#'
#' @param x_legal Numeric vector. Effective numbers of sources for each legal entry point.
#' @param x_illegal Numeric vector. Effective numbers of sources for each illegal entry point.
#' @param illegal_factor (lambda) Number > 1. Relative risk of an illegal entry point with respect
#'   to a legal one.
#' @param coef_legal (alpha) Number > 1. Scaling factor of legal sources in the latent scale.
#' @param coef_illegal (beta) Number > 1. Scaling factor of illegal sources in the latent scale.
#' @param max_risk (M) Number > 1. Maximum assymptotically atteinable risk.
#' @examples
#' library(ggplot2)
#' library(tidyr)
#' case_dat <- tidyr::expand_grid(
#'   xl = 0:5,
#'   xi = 0:5
#' )
#' case_dat |>
#'   transform(
#'     risk = scale_entry_points(xl, xi, 3, 1, 1, 100)
#'   ) |>
#'   ggplot(aes(xl, xi, fill = risk)) +
#'   geom_raster() +
#'   coord_fixed() +
#'   scale_fill_viridis_c(option = "D") +
#'   labs(
#'     x = "N legal eps",
#'     y = "N illegal eps",
#'     fill = "Risk"
#'   )
#' @export
scale_entry_points <- function(
    x_legal,
    x_illegal,
    illegal_factor = 3,
    coef_legal = 1,
    coef_illegal = 1,
    max_risk = 12
    ) {

  stopifnot(
    length(illegal_factor) == 1,
    length(coef_legal) == 1,
    length(coef_illegal) == 1,
    length(max_risk) == 1,
    all(x_legal >= 0),
    all(x_illegal >= 0),
    illegal_factor > 1,
    coef_legal > 0,
    coef_illegal > 0,
    max_risk > 0
  )

  ## Transform the effective number of legal sources into the risk scale,
  ## apply the illegal scaling factor inversely and back-transform to the latent scale
  latent_legal_risk <- inv_sigmoid(sigmoid(coef_legal * x_legal) / illegal_factor)

  eff_n_illegal <- coef_illegal * x_illegal
  ## Combine with the effective number of illegal sources
  latent_risk <- latent_legal_risk + eff_n_illegal

  ## Scale into the risk scale
  ans <- max_risk * sigmoid(latent_risk)
  ans
}
