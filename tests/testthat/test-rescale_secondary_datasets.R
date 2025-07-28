test_that("rescale_risk_scores properly rescales secondary datasets", {
  library(sf)
  library(dplyr)
  library(riskintrodata)
  
  # Create simple test data for border risk
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2"),
      EU_NAME = c("Epi Unit 1", "Epi Unit 2"),
      geometry = st_sfc(
        st_polygon(list(matrix(c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0), ncol = 2, byrow = TRUE))),
        st_polygon(list(matrix(c(2, 0, 4, 0, 4, 2, 2, 2, 2, 0), ncol = 2, byrow = TRUE)))
      )
    ),
    crs = 4326
  )
  
  epi_units <- apply_mapping(
    epi_units_raw,
    mapping = mapping_epi_units(
      eu_id = "EU_ID",
      eu_name = "EU_NAME",
      geometry = "geometry"
    ),
    validate = TRUE
  )
  
  bordering_countries <- st_as_sf(
    data.frame(
      iso3 = "BC1",
      country = "Border Country 1",
      geometry = st_sfc(
        st_polygon(list(matrix(c(2, 0, 6, 0, 6, 4, 2, 4, 2, 0), ncol = 2, byrow = TRUE)))
      )
    ),
    crs = 4326
  )
  
  # Create emission risk factors and calculate risk
  emission_risk_factors <- erf_row(
    iso3 = "BC1",
    country = "Border Country 1", 
    disease = "Test Disease",
    animal_category = "Domestic",
    species = "Cattle",
    disease_notification = 1,
    targeted_surveillance = 1,
    general_surveillance = 0,
    screening = 1,
    precautions_at_the_borders = 1,
    slaughter = 1,
    selective_killing_and_disposal = 1,
    zoning = 1,
    official_vaccination = 1,
    last_outbreak_end_date = as.Date("2020-01-01"),
    commerce_illegal = 0L,
    commerce_legal = 0L
  )
  
  emission_risk_table <- calc_emission_risk(emission_risk_factors)
  
  shared_borders <- calc_border_lengths(
    epi_units = epi_units,
    eu_id_col = "eu_id",
    bordering_countries = bordering_countries,
    bc_id_col = "iso3"
  )
  
  ri_border_original <- calc_border_risk(
    epi_units = epi_units,
    shared_borders = shared_borders,
    emission_risk = emission_risk_table
  )
  
  # Test that original data is on 0-12 scale
  expect_equal(attr(ri_border_original, "scale"), c(0, 12))
  extracted_borders_original <- extract_border(ri_border_original)
  expect_true(all(extracted_borders_original$border_risk <= 12, na.rm = TRUE))
  
  # Rescale to 0-100
  ri_border_scaled <- rescale_risk_scores(
    ri_border_original,
    to = c(0, 100),
    method = "linear"
  )
  
  # Test that main dataset is properly rescaled
  expect_equal(attr(ri_border_scaled, "scale"), c(0, 100))
  expect_true(all(ri_border_scaled$border_risk <= 100, na.rm = TRUE))
  
  # Test that secondary dataset (borders attribute) is also rescaled
  extracted_borders_scaled <- extract_border(ri_border_scaled)
  expect_true(all(extracted_borders_scaled$border_risk <= 100, na.rm = TRUE))
  
  # Test that the scale attributes on the secondary dataset are updated
  expect_equal(attr(extracted_borders_scaled, "scale"), c(0, 100))
  
  # Test that the relationship between main and secondary data is preserved
  # (i.e., both should be scaled by the same factor)
  if (any(ri_border_original$border_risk > 0, na.rm = TRUE) && 
      any(extracted_borders_original$border_risk > 0, na.rm = TRUE)) {
    
    original_ratio <- ri_border_original$border_risk[1] / extracted_borders_original$border_risk[1]
    scaled_ratio <- ri_border_scaled$border_risk[1] / extracted_borders_scaled$border_risk[1]
    
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