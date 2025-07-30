test_that("rescale_risk_scores properly rescales secondary datasets", {
  library(sf)
  library(dplyr)
  library(riskintrodata)
  library(riskintroanalysis)

  # Create simple test epidemiological units
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2", "EU3"),
      EU_NAME = c("Epi Unit 1", "Epi Unit 2", "Epi Unit 3"),
      geometry = st_sfc(
        st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
        st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
        st_polygon(list(matrix(c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0), ncol = 2, byrow = TRUE)))
      ),
      stringsAsFactors = FALSE
    ),
    crs = 4326
  )

  # Apply mapping to prepare and validate dataset
  epi_units <- apply_mapping(
    epi_units_raw,
    mapping = mapping_epi_units(
      eu_id = "EU_ID",
      eu_name = "EU_NAME",
      geometry = "geometry"
    ),
    validate = TRUE
  )

  # Create test entry points - one row per source
  entry_points_raw <- data.frame(
    POINT_ID = c("EP1", "EP2", "EP3", "EP3", "EP4"),
    POINT_NAME = c("Entry Point 1", "Entry Point 2", "Entry Point 3", "Entry Point 3", "Entry Point 4"),
    LONGITUDE = c(0.5, 1.5, 2.5, 2.5, 0.2), # EP1 in EU1, EP2 in EU2, EP3 in EU3 (2 rows), EP4 in EU1
    LATITUDE = c(0.5, 0.5, 0.5, 0.5, 0.8),
    MODE = c("C", "NC", "C", "C", "NC"), # Legal/Illegal
    TYPE = c("AIR", "SEA", "BC", "BC", "CC"),
    SOURCES = c("SRC1", "SRC2", "SRC1", "SRC2", "SRC1"), # One source per row
    stringsAsFactors = FALSE
  )

  # Apply mapping to prepare and validate entry points
  entry_points <- apply_mapping(
    entry_points_raw,
    mapping = mapping_entry_points(
      point_name = "POINT_NAME",
      lng = "LONGITUDE",
      lat = "LATITUDE",
      mode = "MODE",
      type = "TYPE",
      sources = "SOURCES"
    ),
    validate = TRUE
  )

  # Create test emission risk factors
  emission_risk_factors <- bind_rows(
    erf_row(
      iso3 = "SRC1",
      country = "Source Country 1",
      disease = "Avian infectious laryngotracheitis",
      animal_category = "Domestic",
      species = "Birds",
      disease_notification = 0,
      targeted_surveillance = 0,
      general_surveillance = 0,
      screening = 1,
      precautions_at_the_borders = 0,
      slaughter = 1,
      selective_killing_and_disposal = 0,
      zoning = 1,
      official_vaccination = 1,
      last_outbreak_end_date = as.Date("30/06/1945"),
      commerce_illegal = 0L,
      commerce_legal = 0L
    ),
    erf_row(
      iso3 = "SRC2",
      country = "Source Country 2",
      disease = "Avian infectious laryngotracheitis",
      animal_category = "Domestic",
      species = "Birds",
      disease_notification = TRUE,
      targeted_surveillance = 1,
      general_surveillance = 0,
      screening = 1,
      precautions_at_the_borders = 0,
      slaughter = 1,
      selective_killing_and_disposal = 1,
      zoning = 1,
      official_vaccination = 1,
      last_outbreak_end_date = as.Date("30/06/2019"),
      commerce_illegal = 1L,
      commerce_legal = 1
    )
  )

  emission_risk_table <- calc_emission_risk(emission_risk_factors)

  # Step 1: Calculate entry point risk
  entry_points_original <- calc_entry_point_risk(
    entry_points = entry_points,
    epi_units = epi_units,
    emission_risk = emission_risk_table
  )

  # Test that original data is on 0-12 scale
  expect_equal(attr(entry_points_original, "scale"), c(0, 12))
  extracted_borders_original <- extract_point_risk(entry_points_original)
  expect_true(all(entry_points_original$entry_points_risk <= 12, na.rm = TRUE))

  # Rescale to 0-100
  entry_points_scaled <- rescale_risk_scores(
    entry_points_original,
    to = c(0, 100),
    method = "linear"
  )

  # Test that main dataset is properly rescaled
  expect_equal(attr(entry_points_scaled, "scale"), c(0, 100))
  expect_true(all(entry_points_scaled$entry_points_risk <= 100, na.rm = TRUE))

  # Test that secondary dataset (borders attribute) is also rescaled
  extracted_borders_scaled <- extract_point_risk(entry_points_scaled)
  expect_true(all(extracted_borders_scaled$point_emission_risk <= 100, na.rm = TRUE))

  # Test that the scale attributes on the secondary dataset are updated
  expect_equal(attr(extracted_borders_scaled, "scale"), c(0, 100))

  # Test that the relationship between main and secondary data is preserved
  # (i.e., both should be scaled by the same factor)
  if (any(entry_points_original$entry_points_risk > 0, na.rm = TRUE) &&
      any(extracted_borders_original$point_emission_risk > 0, na.rm = TRUE)) {

    original_ratio <- entry_points_original$entry_points_risk[1] / extracted_borders_original$point_emission_risk[1]
    scaled_ratio <- entry_points_scaled$entry_points_risk[1] / extracted_borders_scaled$point_emission_risk[1]

    expect_equal(original_ratio, scaled_ratio, tolerance = 1e-10)
  }
})

test_that("rescale_risk_scores handles missing secondary datasets gracefully", {
  library(sf)
  library(dplyr)

  # Create a simple dataset without secondary datasets
  simple_data <- st_as_sf(
    data.frame(
      id = 1:3,
      risk_score = c(2, 5, 8),
      geometry = st_sfc(
        st_point(c(0, 0)),
        st_point(c(1, 1)),
        st_point(c(2, 2))
      )
    ),
    crs = 4326
  )

  # Set attributes manually
  attr(simple_data, "risk_col") <- "risk_score"
  attr(simple_data, "scale") <- c(0, 12)
  attr(simple_data, "table_name") <- "test"

  # Should work without errors even when no secondary datasets exist
  expect_no_error({
    rescaled_simple <- rescale_risk_scores(
      simple_data,
      to = c(0, 100),
      method = "linear"
    )
  })

  rescaled_simple <- rescale_risk_scores(
    simple_data,
    to = c(0, 100),
    method = "linear"
  )

  expect_equal(attr(rescaled_simple, "scale"), c(0, 100))
  expect_true(all(rescaled_simple$risk_score <= 100))
})
