test_that("calc_emission_risk calculates emission risk correctly", {
  library(riskintroanalysis)
  library(riskintrodata)
  test_erf <- tibble::tibble(
    iso3 = c("TUN", "DZA", "MAR"),
    country = c("Tunisia", "Algeria", "Morocco"),
    disease = "Test Disease",
    animal_category = "Domestic",
    species = "Cattle",
    disease_notification = c(1, 0, 1),
    targeted_surveillance = c(1, 0, 1),
    general_surveillance = c(0, 1, 1),
    screening = c(0, 1, 0),
    precautions_at_the_borders = c(1, 0, 1),
    slaughter = c(0, 1, 0),
    selective_killing_and_disposal = c(1, 0, 1),
    zoning = c(0, 1, 0),
    official_vaccination = c(1, 0, 1),
    last_outbreak_end_date = as.Date(c("2020-01-01", "2015-01-01", "2022-01-01")),
    commerce_illegal = c(0, 1, 0),
    commerce_legal = c(1, 0, 1),
    data_source = "Test data"
  )
  
  attr(test_erf, "table_name") <- "emission_risk_factors"
  
  result <- calc_emission_risk(test_erf)
  
  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "table_name"), "emission_risk_scores")
  expect_equal(attr(result, "risk_col"), "emission_risk")
  expect_equal(attr(result, "scale"), c(0, 12))
  
  expect_true(all(c("iso3", "country", "disease", "animal_category", "species", 
                   "data_source", "emission_risk") %in% colnames(result)))
  
  expect_true(all(result$emission_risk >= 0 & result$emission_risk <= 12))
  expect_equal(nrow(result), 3)
  expect_equal(result$iso3, c("TUN", "DZA", "MAR"))
})

test_that("calc_emission_risk with keep_scores = FALSE only keeps essential columns", {
  test_erf <- tibble::tibble(
    iso3 = c("TUN", "DZA"),
    country = c("Tunisia", "Algeria"),
    disease = "Test Disease",
    animal_category = "Domestic",
    species = "Cattle",
    disease_notification = c(1, 0),
    targeted_surveillance = c(1, 0),
    general_surveillance = c(0, 1),
    screening = c(0, 1),
    precautions_at_the_borders = c(1, 0),
    slaughter = c(0, 1),
    selective_killing_and_disposal = c(1, 0),
    zoning = c(0, 1),
    official_vaccination = c(1, 0),
    last_outbreak_end_date = as.Date(c("2020-01-01", "2015-01-01")),
    commerce_illegal = c(0, 1),
    commerce_legal = c(1, 0),
    data_source = "Test data"
  )
  
  attr(test_erf, "table_name") <- "emission_risk_factors"
  
  result <- calc_emission_risk(test_erf, keep_scores = FALSE)
  
  expected_cols <- c("iso3", "country", "disease", "animal_category", "species", "data_source", "emission_risk")
  expect_equal(sort(colnames(result)), sort(expected_cols))
  
  expect_false("sc_survmeasures" %in% colnames(result))
  expect_false("sc_control" %in% colnames(result))
  expect_false("sc_commerce" %in% colnames(result))
  expect_false("sc_epistatus" %in% colnames(result))
})

test_that("calc_emission_risk validates input correctly", {
  test_erf <- tibble::tibble(
    iso3 = c("TUN"),
    country = c("Tunisia"),
    disease = "Test Disease",
    animal_category = "Domestic",
    species = "Cattle",
    disease_notification = c(1),
    targeted_surveillance = c(1),
    general_surveillance = c(0),
    screening = c(0),
    precautions_at_the_borders = c(1),
    slaughter = c(0),
    selective_killing_and_disposal = c(1),
    zoning = c(0),
    official_vaccination = c(1),
    last_outbreak_end_date = as.Date(c("2020-01-01")),
    commerce_illegal = c(0),
    commerce_legal = c(1),
    data_source = "Test data"
  )
  
  expect_error(calc_emission_risk(test_erf), "does not have.*table_name.*attribute")
  
  attr(test_erf, "table_name") <- "wrong_table_name"
  expect_error(calc_emission_risk(test_erf), "should be \"emission_risk_factors\"")
  
  attr(test_erf, "table_name") <- "emission_risk_factors"
  
  wrong_weights <- list(disease_notification = 1, targeted_surveillance = 1)
  expect_error(calc_emission_risk(test_erf, weights = wrong_weights), "should sum to 5")
  
  wrong_length_weights <- riskintrodata::get_erf_weights()
  wrong_length_weights$extra_weight <- 0.1
  expect_error(calc_emission_risk(test_erf, weights = wrong_length_weights), "should sum to 5")
  
  empty_erf <- test_erf[0, ]
  expect_error(calc_emission_risk(empty_erf), "has no rows")
})

test_that("calc_emission_risk handles commerce scores correctly", {
  test_erf <- tibble::tibble(
    iso3 = c("A", "B", "C", "D"),
    country = c("Country A", "Country B", "Country C", "Country D"),
    disease = "Test Disease",
    animal_category = "Domestic",
    species = "Cattle",
    disease_notification = rep(0, 4),
    targeted_surveillance = rep(0, 4),
    general_surveillance = rep(0, 4),
    screening = rep(0, 4),
    precautions_at_the_borders = rep(0, 4),
    slaughter = rep(0, 4),
    selective_killing_and_disposal = rep(0, 4),
    zoning = rep(0, 4),
    official_vaccination = rep(0, 4),
    last_outbreak_end_date = as.Date(rep("1900-01-01", 4)),
    commerce_illegal = c(0, 1, 0, 1),
    commerce_legal = c(0, 0, 1, 1),
    data_source = "Test data"
  )
  
  attr(test_erf, "table_name") <- "emission_risk_factors"
  
  result <- calc_emission_risk(test_erf)
  
  expect_equal(result$sc_commerce, c(0, 3, 1, 4))
})

test_that("calc_epistatus calculates epidemiological status correctly", {
  test_data <- tibble::tibble(
    last_outbreak_end_date = c(
      Sys.Date(),
      Sys.Date() - 365 * 1,
      Sys.Date() - 365 * 2.5,
      Sys.Date() - 365 * 5,
      Sys.Date() - 365 * 10,
      as.Date(NA)
    )
  )
  
  result <- riskintroanalysis:::calc_epistatus(test_data, "last_outbreak_end_date")
  
  expect_equal(result$sc_epistatus[1], 3)
  expect_true(result$sc_epistatus[2] < 3 & result$sc_epistatus[2] > 0)
  expect_true(result$sc_epistatus[3] < result$sc_epistatus[2])
  expect_equal(result$sc_epistatus[4], 0)
  expect_equal(result$sc_epistatus[5], 0)
  expect_true(is.na(result$sc_epistatus[6]))
  
  expect_error(riskintroanalysis:::calc_epistatus(test_data, "nonexistent_column"), "Column.*not found")
})

test_that("calc_emission_risk uses default weights correctly", {
  test_erf <- tibble::tibble(
    iso3 = "TUN",
    country = "Tunisia",
    disease = "Test Disease",
    animal_category = "Domestic",
    species = "Cattle",
    disease_notification = 1,
    targeted_surveillance = 1,
    general_surveillance = 1,
    screening = 1,
    precautions_at_the_borders = 1,
    slaughter = 1,
    selective_killing_and_disposal = 1,
    zoning = 1,
    official_vaccination = 1,
    last_outbreak_end_date = as.Date("1900-01-01"),
    commerce_illegal = 0,
    commerce_legal = 0,
    data_source = "Test data"
  )
  
  attr(test_erf, "table_name") <- "emission_risk_factors"
  
  result <- calc_emission_risk(test_erf)
  
  expected_survmeasures <- 0.25 + 0.5 + 0.5 + 0.75
  expected_control <- 1 + 0.5 + 0.5 + 0.75 + 0.25
  expected_total <- expected_survmeasures + expected_control + 0 + 0
  
  expect_equal(result$sc_survmeasures, expected_survmeasures)
  expect_equal(result$sc_control, expected_control)
  expect_equal(result$sc_commerce, 0)
  expect_equal(result$sc_epistatus, 0)
  expect_equal(result$emission_risk, expected_total)
})