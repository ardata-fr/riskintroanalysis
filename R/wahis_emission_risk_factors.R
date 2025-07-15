
#' Get WAHIS Emission Risk Factors Dataset
#'
#' Helper function for getting the WAHIS emission risk factors dataset. As most
#' analysis done require filtering for one type of each of diease, species and
#' animal_category, this function is a helper for that.
#'
#' For dataset documentation see: [riskintrodata::wahis_emission_risk_factors]
#'
#' @param disease filter dataset for one or more disease
#' @param species filter dataset for one or more species
#' @param animal_category filter dataset for one or more animal_category
#'
#' @return the emission risk factorts dataset as documented here [riskintrodata::wahis_emission_risk_factors]
#' @export
#' @importFrom dplyr filter
#' @importFrom riskintrodata validate_table_content
get_wahis_erf <- function(
    disease = character(),
    species = character(),
    animal_category = character()
){

  filters <- list()

  if (length(disease) > 0) {
    filters <- append(
      filters,
      rlang::expr(disease %in% !!disease)
    )
  }

  if (length(species) > 0) {
    filters <- append(
      filters,
      rlang::expr(species %in% !!species)
    )
  }

  if (length(animal_category) > 0) {
    filters <- append(
      filters,
      rlang::expr(animal_category %in% !!animal_category)
    )
  }

  x <- riskintrodata::wahis_emission_risk_factors |>
    dplyr::filter(!!!filters)

  x <- validate_table_content(x, "emission_risk_factors")
  x <- validate_table_content_cli_msg(x)
  x
}


#' Create an Emission Risk Factors (ERF) Row
#'
#' Constructs and validates a single row of emission risk factor data for a given country, species, and disease context.
#' This function serves as a robust helper to generate new entries in the emission risk factors dataset, ensuring
#' compatibility with subsequent analytical workflows in the [`riskintroanalysis`] package.
#'
#' The function performs input validation and data cleaning (e.g., coercing numeric fields to integer) and assigns
#' appropriate data types. It ensures the output meets the schema expected by the emission risk scoring system, which
#' uses this data to calculate risk scores across four weighted domains:
#'
#' - Epidemiological status (e.g., `last_outbreak_end_date`)
#' - Surveillance measures (e.g., `disease_notification`, `targeted_surveillance`, `general_surveillance`, `screening`)
#' - Control measures (e.g., `slaughter`, `zoning`, `official_vaccination`)
#' - Animal commerce (e.g., `commerce_illegal`, `commerce_legal`)
#'
#' The resulting row includes domain-specific fields that will be used by scoring algorithms such as `build_emission_risk_table()`
#' to derive intermediate and final emission risk values.
#'
#' @param iso3 Character. ISO3 country code (e.g., `"FRA"`).
#' @param country Character. Country name.
#' @param disease Character. Disease name.
#' @param animal_category Character. A high-level category label (e.g., `"livestock"`, `"wildlife"`).
#' @param species Character. Target animal species.
#' @param disease_notification Integer (0 or 1). Is disease notification mandatory?
#' @param targeted_surveillance Integer (0 or 1). Is targeted surveillance applied?
#' @param general_surveillance Integer (0 or 1). Is general surveillance applied?
#' @param screening Integer (0 or 1). Are screening measures in place?
#' @param precautions_at_the_borders Integer (0 or 1). Are there border precautions?
#' @param slaughter Integer (0 or 1). Is emergency slaughter used as a control measure?
#' @param selective_killing_and_disposal Integer (0 or 1). Are infected animals selectively killed and disposed?
#' @param zoning Integer (0 or 1). Is zoning or compartmentalization used?
#' @param official_vaccination Integer (0 or 1). Is official vaccination used?
#' @param last_outbreak_end_date Date or character convertible to Date. The date when the last known outbreak ended.
#' @param commerce_illegal Integer (0, 1, or 3). Risk score for illegal animal commerce (higher is riskier).
#' @param commerce_legal Integer (0, 1, or 3). Risk score for legal animal commerce.
#' @param data_source Character. Describes the data source, defaulting to the user name and current date.
#'
#' @return A 1-row [tibble::tibble] object with cleaned, validated, and structured emission risk factor data.
#'         The object includes an attribute `"datatype" = "erf_table"` for downstream compatibility.
#'
#' @details
#' The returned tibble is validated against the expected schema using `validate_table_content()` and
#' `validate_table_content_cli_msg()`. Any coercions or type fixes are handled internally via helper functions like
#' `if_numeric_to_int()`.
#'
#' @seealso [build_emission_risk_table()] for generating weighted emission risk scores from ERF data.
#'
#' @examples
#' erf_row(
#'   iso3 = "FRA",
#'   country = "France",
#'   disease = "ASF",
#'   animal_category = "livestock",
#'   species = "pig",
#'   disease_notification = 1,
#'   targeted_surveillance = 1,
#'   general_surveillance = 0,
#'   screening = 1,
#'   precautions_at_the_borders = 1,
#'   slaughter = 1,
#'   selective_killing_and_disposal = 0,
#'   zoning = 1,
#'   official_vaccination = 0,
#'   last_outbreak_end_date = "2020-05-01",
#'   commerce_illegal = 1,
#'   commerce_legal = 1
#' )
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom riskintrodata validate_table_content validate_table_content_cli_msg
erf_row <- function(
    iso3,
    country,
    disease,
    animal_category,
    species,
    disease_notification = 0L,
    targeted_surveillance = 0L,
    general_surveillance = 0L,
    screening = 0L,
    precautions_at_the_borders = 0L,
    slaughter = 0L,
    selective_killing_and_disposal = 0L,
    zoning = 0L,
    official_vaccination = 0L,
    last_outbreak_end_date = as.Date("01/01/1900"),
    commerce_illegal = 0L,
    commerce_legal = 0L,
    data_source = paste0("User ", Sys.info()[["user"]], " - ", Sys.Date())
){



  x <- tibble::tibble(
    iso3 = iso3,
    country = country,
    disease = disease,
    animal_category = animal_category,
    species = species,
    disease_notification = if_numeric_to_int(disease_notification),
    targeted_surveillance = if_numeric_to_int(targeted_surveillance),
    general_surveillance = if_numeric_to_int(general_surveillance),
    screening = if_numeric_to_int(screening),
    precautions_at_the_borders = if_numeric_to_int(precautions_at_the_borders),
    slaughter = if_numeric_to_int(slaughter),
    selective_killing_and_disposal = if_numeric_to_int(selective_killing_and_disposal),
    zoning = if_numeric_to_int(zoning),
    official_vaccination = if_numeric_to_int(official_vaccination),
    last_outbreak_end_date = if_not_date(last_outbreak_end_date),
    commerce_illegal = if_numeric_to_int(commerce_illegal),
    commerce_legal = if_numeric_to_int(commerce_legal),
    data_source = data_source
  )

  status <- validate_table_content(x, table_name = "emission_risk_factors")
  dataset <- validate_table_content_cli_msg(status)

  attr(dataset, "datatype") <- "erf_table"
  dataset
}

if_numeric_to_int <- function(x){
  ifelse(class(x) %in% c("numeric", "logical") , as.integer(x), x)
}

if_not_date <- function(x) {
  if (!inherits(x, "Date")) {
    as.Date(x)
  } else {
    x
  }
}
