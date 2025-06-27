#' @title Build the weighted emission risk table
#'
#' @description
#' This function calculates the overall emission risk score (`emission_risk`) for a set of countries
#' based on four domains:
#'
#' - **Epidemiological status**: Time since the last outbreak (`sc_epistatus`).
#' - **Surveillance measures**: Effectiveness of implemented surveillance strategies (`sc_survmeasures`).
#' - **Control measures**: Effectiveness of disease control measures (`sc_control`).
#' - **Animal commerce movements**: Risk from commerce and movement of animals (`sc_commerce`).
#'
#' Intermediate scores for each domain are calculated internally:
#'
#' - **`sc_epistatus`**: The epidemiological status score is based on the time since the last outbreak.
#'    If the disease has not been detected in the last 5 years (\eqn{x > 5}), the score is 0. If the disease is
#'    currently present (\eqn{x = 0}), the score is 3. An exponential decay model smooths the scoring over time:
#'    \deqn{S = 3 \times \exp\left(-x \frac{\log(2)}{5}\right)}
#'
#' - **`sc_survmeasures`**: Surveillance measures are scored based on the absence of effective measures.
#'    Scores range from 0 (all measures implemented) to 3 (no measures deployed). The default coefficients are:
#'    - **Active surveillance**: 0.5
#'    - **Passive surveillance**: 0.5
#'    - **Risk-based surveillance**: 0.75
#'    - **Mandatory reporting**: 0.25
#'
#' - **`sc_control`**: Control measures are scored similarly, ranging from 0 (all measures implemented) to 3 (no measures deployed).
#'    Default coefficients are:
#'    - **Border control**: 1
#'    - **Culling at outbreak sites**: 0.5
#'    - **Culling around outbreak sites**: 0.5
#'    - **Movement zoning and restrictions**: 0.75
#'    - **Ring vaccination around outbreak sites**: 0.25
#'
#' - **`sc_commerce`**: The risk score for animal commerce movements can take values of 0, 1, or 3, depending on
#'    the risk level identified for trade and movement activities.
#'
#' The overall emission risk score is the weighted sum of these components and is ensured to fall in the range (0, 12].
#'
#' @param dat A data frame containing risk factor data (default is `emission_risk_factors`).
#' @param weights A named list of weights corresponding to the columns in `dat`. Default weights are applied if not provided.
#' @return A data frame containing:
#' - Intermediate scores for each risk domain (`sc_epistatus`, `sc_survmeasures`, `sc_control`, and `sc_commerce`).
#' - The overall emission risk score (`emission_risk`).
#'
#' @family emission_risk_calculation
get_weighted_emission_risk <- function(
    dat, weights) {
  # Refer to data-raw/emission-risk-defaults.R
  risk_factor_cols <- names(weights)

  if (any(!risk_factor_cols %in% colnames(dat))) {
    stop("Weight names should be the names of the `dat` column names")
  }

  weighted_emission_risk <- dat |>
    # Weighted risk factors ----
    mutate(
      across(
        all_of(risk_factor_cols),
        function(x) {
          weights[[cur_column()]] * rm_na(x)
        }
      )
    ) |>
    # Calculate scores from risk factors ----
    mutate(
      sc_survmeasures =
        .data[["targeted_surveillance"]] + .data[["general_surveillance"]] +
          .data[["screening"]] + .data[["disease_notification"]],
      sc_control =
        .data[["precautions_at_the_borders"]] + .data[["slaughter"]] +
          .data[["selective_killing_and_disposal"]] + .data[["zoning"]] +
          .data[["official_vaccination"]],
      sc_commerce = case_when(
        .data[["commerce_illegal"]] == 0 & .data[["commerce_legal"]] == 0 ~ 0,
        .data[["commerce_illegal"]] == 1 & .data[["commerce_legal"]] == 0 ~ 3,
        .data[["commerce_illegal"]] == 0 & .data[["commerce_legal"]] == 1 ~ 1,
        .data[["commerce_illegal"]] == 1 & .data[["commerce_legal"]] == 1 ~ 4,
        TRUE ~ NA_integer_
      )
    ) |>
    calc_epistatus("last_outbreak_end_date") |>
    # Calculate overall emission_risk from scores! (finally) ----
    mutate(
      emission_risk =
        .data[["sc_survmeasures"]] +
          .data[["sc_control"]] +
          rm_na(.data[["sc_epistatus"]],replace_na = 0) +
          rm_na(.data[["sc_commerce"]], replace_na = 0)
    ) |>
    select(all_of(
      c("iso3", "country", "disease", "animal_category", "species","data_source",
        "sc_survmeasures", "sc_control", "sc_commerce", "sc_epistatus", "emission_risk"
      )))
  weighted_emission_risk
}

#' @title Calculate epidemiological status risk score (`sc_epistatus`)
#'
#' @description
#' This function calculates the epidemiological status score (`sc_epistatus`) based on the time since the last outbreak.
#' The formula is:
#'
#' \deqn{S = 3 \times (1 - \frac{x}{10}) \quad \text{for } x \leq 5}
#'
#' If the disease has not been detected in the country in the last 5 years (\eqn{x > 5}),
#' the score is \eqn{S = 0}. If the disease is currently present in the country (\eqn{x = 0}),
#' the score is \eqn{S = 3}.
#'
#' To smooth the scoring over the years, an exponential decay model is used:
#'
#' \deqn{S = 3 \times \exp\left(-x \frac{\log(2)}{5}\right)}
#' @param dat data.frame like object to add `sc_epistatus` column to.
#' @param x string of colname, date for last outbreak.
#' @return same as input data with an extra column containing the surveillence
#' score, where surveillence is in range \deqn{ S \in  (0, 3]}
#' @family emission_risk_calculation
#' @noRd
#' @examples
#' test_data <- data.frame(
#'   date_last_outbreak = c(
#'     Sys.Date(),
#'     Sys.Date() - 365 * 1,
#'     Sys.Date() - 365 * 1.5,
#'     Sys.Date() - 365 * 2,
#'     Sys.Date() - 365 * 2.5,
#'     Sys.Date() - 365 * 3,
#'     Sys.Date() - 365 * 3.5,
#'     Sys.Date() - 365 * 4,
#'     Sys.Date() - 365 * 4.5,
#'     Sys.Date() - 365 * 4.6,
#'     Sys.Date() - 365 * 4.7,
#'     Sys.Date() - 365 * 4.8,
#'     Sys.Date() - 365 * 4.9,
#'     Sys.Date() - 365 * 4.95,
#'     Sys.Date() - 365 * 4.99,
#'     Sys.Date() - 365 * 4.999999999999999999,
#'     Sys.Date() - 365 * 5,
#'     Sys.Date() - 365 * 5.5,
#'     NA # Missing date
#'   )
#' )
#'
#' calc_epistatus(test_data, "date_last_outbreak")
#'
calc_epistatus <- function(dat, x) {
  dat |>
    mutate(
      years_since_outbreak = as.numeric(lubridate::today() - as.Date(.data[[x]])) * (1 / 365),
      sc_epistatus = case_when(
        is.na(.data[["years_since_outbreak"]]) ~ NA,
        .data[["years_since_outbreak"]] <= 0 ~ 3,
        .data[["years_since_outbreak"]] >= 5 ~ 0,
        # TRUE ~ 3 * exp(-.data[["years_since_outbreak"]] * log(2) / 5)
        TRUE ~ 3 * exp(-.data[["years_since_outbreak"]] * log(2) / 5) * (1 - .data[["years_since_outbreak"]] / 5)
      ),
      years_since_outbreak = NULL
    )
}
