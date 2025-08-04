#' @title Build the weighted emission risk table
#'
#' @description
#' This function calculates the overall emission risk score (`emission_risk`) for a set of countries
#' based on four domains (each one counts for a certain number of points towards the risk of introduction out of 12 by default) :
#'
#' - **Epidemiological status** (3/12):  Time since the last outbreak (`sc_epistatus`).
#' - **Surveillance measures** (2/12): Effectiveness of implemented surveillance strategies (`sc_survmeasures`).
#' - **Control measures** (3/12): Effectiveness of disease control measures (`sc_control`).
#' - **Animal commerce movements** (4/12): Risk from commerce and movement of animals (`sc_commerce`).
#'
#' The emission risk is the sum of each of the above.
#'
#' Each of the scores is calculated from the emission risk factors and some are weighted
#' based on the emission risk factor weights (`weights` parameter).
#'
#' - **`sc_epistatus`**: The epidemiological status score is based on the time since the last outbreak
#'    and accounds for 3 out of 12 of the final emission risk score.
#'    If the disease has not been detected in the last 5 years (\eqn{x > 5}), the score is 0. If the disease is
#'    currently present (\eqn{x = 0}), the score is 3. An exponential decay model smooths the scoring over time:
#'    \deqn{S = 3 \times \exp\left(-x \frac{\log(2)}{5}\right)}
#'
#' - **`sc_survmeasures`**: Surveillance measures are scored based on the absence
#'    of effective measures and accounts for 2 out of 12 og the final emission risk
#'    score. The following risk factors contribute
#'    to this score:
#'    - Active surveillance
#'    - Passive surveillance
#'    - Risk-based surveillance
#'    - Mandatory reporting
#'
#' - **`sc_control`**: Control measures are scored similarly, and account for
#'    3 out of 12 of the emission risk score. The following risk factors contribute
#'    to this score:
#'    - Border control
#'    - Culling at outbreak sites
#'    - Culling around outbreak sites
#'    - Movement zoning and restrictions
#'    - Ring vaccination around outbreak sites
#'
#' - **`sc_commerce`**: The risk score for animal commerce movements can take
#' values of 0, 1, 3, or 4. It is the sum of the following:
#'    - 0 means there is no legal or illegal trade
#'    - legal trade present adds 1 to this score
#'    - illegal trade present adds 3 to this score
#'
#' The overall emission risk score is the sum of each weighted risk factor,
#' the final emission risk score is in the range of (0, 12].
#'
#' @param emission_risk_factors A data frame containing risk factor data. Generally,
#' this data will come from [riskintrodata::get_wahis_erf()]. The dataset should be validated and
#' have `table_name` attribute equal to `"emission_risk_factors"`.
#' @param weights A named list of weights corresponding to the following columns in
#' `emission_risk_factors` (and their default weights):
#'
#'  - `disease_notification` (0.25)
#'  - `targeted_surveillance` (0.5)
#'  - `general_surveillance` (0.5)
#'  - `screening` (0.75)
#'  - `precautions_at_the_borders` (1)
#'  - `slaughter` (0.5)
#'  - `selective_killing_and_disposal` (0.5)
#'  - `zoning` (0.75)
#'  - `official_vaccination` (0.25)
#'
#'  The sum of the weights should add up to exactly 5, as these factors correspond
#'  to the weights contributing to `sc_survmeasures` and `sc_control`.
#'
#' @param keep_scores whether to keep or drop `sc_*` columns, `emission_risk` column
#' is always kept.
#' @return A tibble containing the following columns:
#' -  `iso3` identifis the country
#' - `country` country name (from emission risk factors dataset)
#' - `disease` disease being studied (from emission risk factors dataset)
#' - `animal_category ` animal_category  being studied (from emission risk factors dataset)
#' - `species` species being studied (from emission risk factors dataset)
#' - `data_source` data source for emission risk factors (from emission risk factors dataset)
#' - `sc_survmeasures` as detailed above.
#' - `sc_control` as detailed above.
#' - `sc_commerce` as detailed above.
#' - `sc_epistatus` as detailed above.
#' - `emission_risk` as detailed above.
#'
#' This dataset also has a **number of attributes** that are used in other
#' functions from `riskintroanalysis` to make passing dataset metadata between
#' functions more user-friendly. Used mainly in used by [plot_risk()] and
#'  [rescale_risk_scores()].
#'
#' -  `table_name = "emission_risk_scores"`
#' -  `risk_col = "emission_risk"`
#' -  `scale = c(0, 12)`
#' -  `table_validated = TRUE`
#'
#' @examples
#' library(riskintrodata)
#' library(riskintroanalysis)
#'
#' wahis_erf <- get_wahis_erf(
#'   disease = "Avian infectious laryngotracheitis",
#'   animal_category = "Domestic",
#'   species = "Birds"
#' )
#'
#' emission_risk_table <- calc_emission_risk(
#'   emission_risk_factors = wahis_erf
#' )
#' @family emission_risk_calculation
#' @importFrom riskintrodata get_erf_weights
#' @export
calc_emission_risk <- function(
    emission_risk_factors,
    weights = get_erf_weights(),
    keep_scores = TRUE
) {

  cli_abort_if_not(
    "{.arg emission_risk_factors} dataset does not have {.val table_name} attribute" = !is.null(attr(emission_risk_factors, "table_name")),
    "{.arg weights} should sum to 5, see doc: {.help [{.fun calc_emission_risk}](riskintroanalysis::calc_emission_risk)}" = sum(unlist(weights)) == 5L,
    "{.arg weights} should have length 9, see doc: {.help [{.fun calc_emission_risk}](riskintroanalysis::calc_emission_risk)}" = length(weights) == 9L,
    "{.arg emission_risk_factors} has no rows" = nrow(emission_risk_factors) > 0
  )
  if (attr(emission_risk_factors, "table_name") != "emission_risk_factors") {
    cli_abort(paste(
      "{.arg emission_risk_factors} dataset attribute {.arg table_name} is",
      "\"{attr(emission_risk_factors, \"table_name\")}\" and should be \"emission_risk_factors\""
      ))
  }

  # Refer to data-raw/emission-risk-defaults.R
  risk_factor_cols <- names(weights)

  if (any(!risk_factor_cols %in% colnames(emission_risk_factors))) {
    stop("Weight names should be the names of the `dat` column names")
  }

  # Weighted risk factors ----
  out <- emission_risk_factors |>
  mutate(
    across(
      all_of(risk_factor_cols),
      function(x) {
        weights[[cur_column()]] * rm_na(x)
      }
    )
  )

  out <- out |>
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
  )

  out <- out |>
    calc_epistatus("last_outbreak_end_date")

  out <- out |>
    mutate(
    emission_risk =
      .data[["sc_survmeasures"]] +
      .data[["sc_control"]] +
      rm_na(.data[["sc_epistatus"]],replace_na = 0) +
      rm_na(.data[["sc_commerce"]], replace_na = 0)
  )

  if (keep_scores) {
    keep_cols <-c(
      "iso3", "country", "disease", "animal_category", "species","data_source",
      "sc_survmeasures", "sc_control", "sc_commerce", "sc_epistatus", "emission_risk"
      )
  } else {
    keep_cols <-c(
      "iso3", "country", "disease", "animal_category", "species","data_source",
      "emission_risk"
    )
  }

  out <- out |>
    select(all_of(keep_cols))

  attr(out, "table_name") <- "emission_risk_scores"
  attr(out, "risk_col") <- "emission_risk"
  attr(out, "scale") <- c(0, 12)
  out
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
calc_epistatus <- function(dat, x) {
  cli_abort_if_not("Column {.var {x}} not found in dataset" = x %in% colnames(dat))
  dat |>
    mutate(
      years_since_outbreak = as.numeric(as.Date(Sys.time()) - as.Date(.data[[x]])) * (1 / 365),
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
