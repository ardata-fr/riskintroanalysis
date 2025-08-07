test_that("plot_emission_risk creates static ggplot map", {
  library(riskintroanalysis)
  library(riskintrodata)
  skip_if_not_installed("ggplot2")
  
  test_emission_risk <- tibble::tibble(
    iso3 = c("TUN", "DZA", "MAR"),
    country = c("Tunisia", "Algeria", "Morocco"),
    disease = "Test Disease",
    animal_category = "Domestic",
    species = "Cattle",
    emission_risk = c(2.5, 7.8, 4.2),
    data_source = "Test data"
  )
  
  result <- plot_emission_risk(test_emission_risk)
  
  expect_s3_class(result, "ggplot")
  
  expect_true("GeomSf" %in% class(result$layers[[1]]$geom))
  
  expect_equal(result$labels$fill, "emission_risk")
  
  expect_s3_class(result$theme, "theme")
})

test_that("plot_emission_risk handles missing data gracefully", {
  skip_if_not_installed("ggplot2")
  
  test_emission_risk <- tibble::tibble(
    iso3 = c("TUN", "FAKE"),
    country = c("Tunisia", "Fake Country"),
    disease = "Test Disease",
    animal_category = "Domestic",
    species = "Cattle",
    emission_risk = c(2.5, 7.8),
    data_source = "Test data"
  )
  
  result <- plot_emission_risk(test_emission_risk)
  
  expect_s3_class(result, "ggplot")
  expect_no_error(print(result))
})

test_that("plot_emission_risk_interactive creates leaflet map", {
  skip_if_not_installed("leaflet")
  
  test_emission_risk <- tibble::tibble(
    iso3 = c("TUN", "DZA", "MAR"),
    country = c("Tunisia", "Algeria", "Morocco"),
    disease = "Test Disease",
    animal_category = "Domestic", 
    species = "Cattle",
    emission_risk = c(2.5, 7.8, 4.2),
    sc_epistatus = c(1.2, 2.1, 0.8),
    sc_survmeasures = c(0.5, 1.2, 0.3),
    sc_control = c(0.8, 2.5, 1.1),
    sc_commerce = c(0, 2, 2),
    data_source = "Test data"
  )
  
  result <- plot_emission_risk_interactive(test_emission_risk)
  
  expect_s3_class(result, "leaflet")
  expect_true("x" %in% names(result))
  expect_true("dependencies" %in% names(result))
})

test_that("plot_emission_risk_interactive uses custom leaflet map", {
  skip_if_not_installed("leaflet")
  
  test_emission_risk <- tibble::tibble(
    iso3 = c("TUN"),
    country = c("Tunisia"),
    disease = "Test Disease",
    animal_category = "Domestic",
    species = "Cattle", 
    emission_risk = c(2.5),
    sc_epistatus = c(1.2),
    sc_survmeasures = c(0.5),
    sc_control = c(0.8),
    sc_commerce = c(0),
    data_source = "Test data"
  )
  
  custom_map <- leaflet::leaflet()
  result <- plot_emission_risk_interactive(test_emission_risk, ll = custom_map)
  
  expect_s3_class(result, "leaflet")
})

test_that("plot_emission_risk_interactive handles missing component scores", {
  skip_if_not_installed("leaflet")
  
  test_emission_risk <- tibble::tibble(
    iso3 = c("TUN", "DZA"),
    country = c("Tunisia", "Algeria"),
    disease = "Test Disease",
    animal_category = "Domestic",
    species = "Cattle",
    emission_risk = c(2.5, NA),
    sc_epistatus = c(1.2, NA),
    sc_survmeasures = c(0.5, NA),
    sc_control = c(0.8, NA), 
    sc_commerce = c(0, NA),
    data_source = c("Test data", "Test data")
  )
  
  result <- plot_emission_risk_interactive(test_emission_risk)
  
  expect_s3_class(result, "leaflet")
  expect_no_error(print(result))
})

test_that("plot functions work with empty datasets", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("leaflet")
  
  empty_emission_risk <- tibble::tibble(
    iso3 = character(0),
    country = character(0),
    disease = character(0),
    animal_category = character(0),
    species = character(0),
    emission_risk = numeric(0),
    sc_epistatus = numeric(0),
    sc_survmeasures = numeric(0),
    sc_control = numeric(0),
    sc_commerce = numeric(0),
    data_source = character(0)
  )
  
  static_result <- plot_emission_risk(empty_emission_risk)
  expect_s3_class(static_result, "ggplot")
  
  interactive_result <- plot_emission_risk_interactive(empty_emission_risk)
  expect_s3_class(interactive_result, "leaflet")
})