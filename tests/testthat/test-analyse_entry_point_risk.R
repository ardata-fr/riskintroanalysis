test_that("Complete entry point risk analysis workflow works", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Create simple test epidemiological units
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2", "EU3"),
      EU_NAME = c("Epi Unit 1", "Epi Unit 2", "Epi Unit 3"),
      geometry = st_sfc(
        st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2,
          byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
          ncol = 2,
          byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0),
          ncol = 2,
          byrow = TRUE
        )))
      ),
      stringsAsFactors = FALSE
    ),
    crs = 4326
  )

  # Apply mapping to prepare and validate dataset
  epi_units <- validate_dataset_content(
    x = epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |>
    extract_dataset()

  # Create test entry points - one row per source
  entry_points_raw <- data.frame(
    POINT_ID = c("EP1", "EP2", "EP3", "EP3", "EP4"),
    POINT_NAME = c(
      "Entry Point 1",
      "Entry Point 2",
      "Entry Point 3",
      "Entry Point 3",
      "Entry Point 4"
    ),
    LONGITUDE = c(0.5, 1.5, 2.5, 2.5, 0.2), # EP1 in EU1, EP2 in EU2, EP3 in EU3 (2 rows), EP4 in EU1
    LATITUDE = c(0.5, 0.5, 0.5, 0.5, 0.8),
    MODE = c("C", "NC", "C", "C", "NC"), # Legal/Illegal
    TYPE = c("AIR", "SEA", "BC", "BC", "CC"),
    SOURCES = c("SRC1", "SRC2", "SRC1", "SRC2", "SRC1"), # One source per row
    stringsAsFactors = FALSE
  )

  # Apply mapping to prepare and validate entry points
  entry_points <- validate_dataset_content(
    x = entry_points_raw,
    table_name = "entry_points",
    point_name = "POINT_NAME",
    lng = "LONGITUDE",
    lat = "LATITUDE",
    mode = "MODE",
    type = "TYPE",
    sources = "SOURCES"
  ) |>
    extract_dataset()

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
  ri_entry_points <- calc_entry_point_risk(
    entry_points = entry_points,
    epi_units = epi_units,
    emission_risk = emission_risk_table
  )

  # Test ri_entry_points structure
  expect_s3_class(ri_entry_points, "sf")
  expect_true("eu_id" %in% names(ri_entry_points))
  expect_true("eu_name" %in% names(ri_entry_points))
  expect_true("entry_points_risk" %in% names(ri_entry_points))
  expect_true("entry_points_risk_label" %in% names(ri_entry_points))

  # Test attributes
  expect_equal(attr(ri_entry_points, "risk_col"), "entry_points_risk")
  expect_equal(attr(ri_entry_points, "table_name"), "entry_points")
  expect_equal(attr(ri_entry_points, "scale"), c(0, 12))
  expect_s3_class(attr(ri_entry_points, "points"), "sf")

  # Test risk values are in valid range
  expect_true(all(
    ri_entry_points$entry_points_risk >= 0 &
      ri_entry_points$entry_points_risk <= 12,
    na.rm = TRUE
  ))

  # Test extract_point_risk function
  extracted_points <- expect_no_error(extract_point_risk(ri_entry_points))

  expect_s3_class(extracted_points, "sf")
  expect_true("point_id" %in% names(extracted_points))
  expect_true("point_name" %in% names(extracted_points))
  expect_true("point_emission_risk" %in% names(extracted_points))
  expect_true("mode" %in% names(extracted_points))
  expect_true("type" %in% names(extracted_points))
  expect_true("points_label" %in% names(extracted_points))

  # Test extract_point_risk attributes
  expect_equal(attr(extracted_points, "risk_col"), "point_emission_risk")
  expect_equal(attr(extracted_points, "scale"), c(0, 12))

  # Test that all epi units are present in result
  expect_equal(nrow(ri_entry_points), 3)
  expect_setequal(ri_entry_points$eu_id, c("EU1", "EU2", "EU3"))

  # Test that points were allocated correctly
  # EU1 should have 2 entry points (EP1 and EP4), EU2 should have 1 (EP2), EU3 should have 1 (EP3)
  extracted_points_df <- st_drop_geometry(extracted_points)
  expect_equal(nrow(extracted_points_df), 4)
  expect_setequal(
    extracted_points_df$point_id,
    c("ep-001", "ep-002", "ep-003", "ep-004")
  )

  # Step 2: Test plotting functionality
  # Test static plotting
  static_plot <- plot_risk(ri_entry_points, interactive = FALSE)
  expect_s3_class(static_plot, "ggplot")

  # Test interactive plotting
  interactive_plot <- plot_risk(ri_entry_points, interactive = TRUE)
  expect_s3_class(interactive_plot, c("leaflet", "htmlwidget"))

  # Step 3: Test risk rescaling
  ri_entry_points_scaled <- rescale_risk_scores(
    ri_entry_points,
    to = c(0, 100),
    method = "linear"
  )

  expect_true(all(
    ri_entry_points_scaled$entry_points_risk >= 0 &
      ri_entry_points_scaled$entry_points_risk <= 100,
    na.rm = TRUE
  ))
  expect_equal(attr(ri_entry_points_scaled, "scale"), c(0, 100))

  # Check that secondary dataset (points) has been scaled
  scaled_points <- extract_point_risk(ri_entry_points_scaled)
  expect_equal(attr(scaled_points, "scale"), c(0, 100))

  # Test static plotting with scaled data
  static_plot_scaled <- plot_risk(ri_entry_points_scaled, interactive = FALSE)
  expect_s3_class(static_plot_scaled, "ggplot")

  # Test interactive plotting with scaled data
  interactive_plot_scaled <- plot_risk(
    ri_entry_points_scaled,
    interactive = TRUE
  )
  expect_s3_class(interactive_plot_scaled, c("leaflet", "htmlwidget"))
})


test_that("Entry point risk analysis aggregation functions work correctly", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Create simple test setup
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1"),
      EU_NAME = c("Epi Unit 1"),
      geometry = st_sfc(
        st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2,
          byrow = TRUE
        )))
      ),
      stringsAsFactors = FALSE
    ),
    crs = 4326
  )

  epi_units <- validate_dataset_content(
    x = epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |>
    extract_dataset()

  # Create entry points with different risk sources - testing aggregation
  # EP1 has multiple sources (LOW and HIGH), EP2 has single source (MED)
  entry_points_raw <- data.frame(
    POINT_ID = c("EP1", "EP1", "EP2"),
    POINT_NAME = c("Entry Point 1", "Entry Point 1", "Entry Point 2"),
    LONGITUDE = c(0.3, 0.3, 0.7),
    LATITUDE = c(0.3, 0.3, 0.7),
    MODE = c("C", "C", "C"),
    TYPE = c("AIR", "AIR", "SEA"),
    SOURCES = c("LOW", "HIGH", "MED"), # One source per row
    stringsAsFactors = FALSE
  )

  entry_points <- validate_dataset_content(
    x = entry_points_raw,
    table_name = "entry_points",
    point_name = "POINT_NAME",
    lng = "LONGITUDE",
    lat = "LATITUDE",
    mode = "MODE",
    type = "TYPE",
    sources = "SOURCES"
  ) |>
    extract_dataset()

  # Create emission risk factors with different risk levels
  emission_risk_factors <- bind_rows(
    erf_row(
      iso3 = "LOW",
      country = "Low Risk Country",
      disease = "Avian infectious laryngotracheitis",
      animal_category = "Domestic",
      species = "Birds",
      disease_notification = 1,
      targeted_surveillance = 1,
      general_surveillance = 1,
      screening = 1,
      precautions_at_the_borders = 1,
      slaughter = 1,
      selective_killing_and_disposal = 1,
      zoning = 1,
      official_vaccination = 1,
      last_outbreak_end_date = as.Date("30/06/1945"),
      commerce_illegal = 0L,
      commerce_legal = 0L
    ),
    erf_row(
      iso3 = "HIGH",
      country = "High Risk Country",
      disease = "Avian infectious laryngotracheitis",
      animal_category = "Domestic",
      species = "Birds",
      disease_notification = 0,
      targeted_surveillance = 0,
      general_surveillance = 0,
      screening = 0,
      precautions_at_the_borders = 0,
      slaughter = 0,
      selective_killing_and_disposal = 0,
      zoning = 0,
      official_vaccination = 0,
      last_outbreak_end_date = as.Date("30/06/2023"),
      commerce_illegal = 1L,
      commerce_legal = 1L
    ),
    erf_row(
      iso3 = "MED",
      country = "Medium Risk Country",
      disease = "Avian infectious laryngotracheitis",
      animal_category = "Domestic",
      species = "Birds",
      disease_notification = 1,
      targeted_surveillance = 0,
      general_surveillance = 1,
      screening = 0,
      precautions_at_the_borders = 1,
      slaughter = 0,
      selective_killing_and_disposal = 1,
      zoning = 0,
      official_vaccination = 1,
      last_outbreak_end_date = as.Date("30/06/2020"),
      commerce_illegal = 0L,
      commerce_legal = 1L
    )
  )

  emission_risk_table <- calc_emission_risk(emission_risk_factors)

  # Test mean aggregation for points (default)
  ri_mean <- calc_entry_point_risk(
    entry_points = entry_points,
    epi_units = epi_units,
    emission_risk = emission_risk_table,
    points_agg_fun = mean,
    eu_agg_fun = mean
  )

  points_mean <- extract_point_risk(ri_mean)

  # EP1 should have mean of LOW and HIGH risks
  # EP2 should have MED risk
  expect_true(nrow(points_mean) == 2)

  # Test max aggregation for epidemiological units
  ri_max <- calc_entry_point_risk(
    entry_points = entry_points,
    epi_units = epi_units,
    emission_risk = emission_risk_table,
    points_agg_fun = mean,
    eu_agg_fun = max
  )

  # Test min aggregation for epidemiological units
  ri_min <- calc_entry_point_risk(
    entry_points = entry_points,
    epi_units = epi_units,
    emission_risk = emission_risk_table,
    points_agg_fun = mean,
    eu_agg_fun = min
  )

  # Max should be >= Min
  expect_true(ri_max$entry_points_risk >= ri_min$entry_points_risk)
})


test_that("Entry point risk analysis works with real sample data", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  skip_if_not(
    system.file(
      package = "riskintrodata",
      "samples",
      "tunisia",
      "epi_units",
      "tunisia_adm2_raw.gpkg"
    ) !=
      "",
    "Sample data not available"
  )

  skip_if_not(
    system.file(
      package = "riskintrodata",
      "samples",
      "tunisia",
      "entry_points",
      "BORDER_CROSSING_POINTS.csv"
    ) !=
      "",
    "Sample entry points data not available"
  )

  # Load real sample epidemiological units
  tunisia_raw <- sf::read_sf(system.file(
    package = "riskintrodata",
    "samples",
    "tunisia",
    "epi_units",
    "tunisia_adm2_raw.gpkg"
  ))

  tunisia <- validate_dataset_content(
    x = tunisia_raw,
    table_name = "epi_units",
    eu_name = "NAME_2",
    geometry = "geom"
  ) |>
    extract_dataset()

  # Load real sample entry points
  entry_points_raw <- readr::read_csv(
    system.file(
      package = "riskintrodata",
      "samples",
      "tunisia",
      "entry_points",
      "BORDER_CROSSING_POINTS.csv"
    ),
    show_col_types = FALSE
  )

  entry_points <- validate_dataset_content(
    x = entry_points_raw,
    table_name = "entry_points",
    point_name = "NAME",
    lng = "LONGITUDE_X",
    lat = "LATITUDE_Y",
    mode = "MODE",
    type = "TYPE",
    sources = "SOURCES"
  ) |>
    extract_dataset()

  # Create test emission risk for Algeria and Libya (common neighbors)
  emission_risk_factors <- bind_rows(
    erf_row(
      iso3 = "DZA",
      country = "Algeria",
      disease = "Avian infectious laryngotracheitis",
      animal_category = "Domestic",
      species = "Birds",
      disease_notification = 0,
      targeted_surveillance = 1,
      general_surveillance = 0,
      screening = 1,
      precautions_at_the_borders = 1,
      slaughter = 1,
      selective_killing_and_disposal = 1,
      zoning = 1,
      official_vaccination = 1,
      last_outbreak_end_date = as.Date("30/06/2023"),
      commerce_illegal = 0L,
      commerce_legal = 0L
    ),
    erf_row(
      iso3 = "LBY",
      country = "Libya",
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
      commerce_illegal = 0L,
      commerce_legal = 1
    )
  )

  emission_risk_table <- calc_emission_risk(emission_risk_factors)

  # Calculate entry point risk

  expect_warning(
    ri_entry_points <- calc_entry_point_risk(
      entry_points = entry_points,
      epi_units = tunisia,
      emission_risk = emission_risk_table
    )
  )

  # Test with real data
  expect_s3_class(ri_entry_points, "sf")
  expect_gt(nrow(ri_entry_points), 0)
  expect_true(all(
    ri_entry_points$entry_points_risk >= 0 &
      ri_entry_points$entry_points_risk <= 12,
    na.rm = TRUE
  ))

  # Test extract_point_risk with real data
  extracted_points <- extract_point_risk(ri_entry_points)
  expect_s3_class(extracted_points, "sf")
  expect_gt(nrow(extracted_points), 0)

  # Test plotting works with real data
  static_plot <- plot_risk(ri_entry_points, interactive = FALSE)
  expect_s3_class(static_plot, "ggplot")

  interactive_plot <- plot_risk(ri_entry_points, interactive = TRUE)
  expect_s3_class(interactive_plot, c("leaflet", "htmlwidget"))

  # Test rescaling with real data
  ri_scaled <- rescale_risk_scores(
    ri_entry_points,
    to = c(0, 100),
    method = "linear"
  )
  expect_true(all(
    ri_scaled$entry_points_risk >= 0 & ri_scaled$entry_points_risk <= 100,
    na.rm = TRUE
  ))
})


test_that("Entry point risk analysis handles missing emission risk gracefully", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Create simple test setup
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1"),
      EU_NAME = c("Epi Unit 1"),
      geometry = st_sfc(
        st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2,
          byrow = TRUE
        )))
      ),
      stringsAsFactors = FALSE
    ),
    crs = 4326
  )

  epi_units <- validate_dataset_content(
    x = epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |>
    extract_dataset()

  # Create entry points with sources that don't have emission risk data
  entry_points_raw <- data.frame(
    POINT_ID = c("EP1", "EP2"),
    POINT_NAME = c("Entry Point 1", "Entry Point 2"),
    LONGITUDE = c(0.3, 0.7),
    LATITUDE = c(0.3, 0.7),
    MODE = c("C", "C"),
    TYPE = c("AIR", "SEA"),
    SOURCES = c("KNOWN", "MISSING"), # MISSING has no emission risk data
    stringsAsFactors = FALSE
  )

  entry_points <- validate_dataset_content(
    x = entry_points_raw,
    table_name = "entry_points",
    point_name = "POINT_NAME",
    lng = "LONGITUDE",
    lat = "LATITUDE",
    mode = "MODE",
    type = "TYPE",
    sources = "SOURCES"
  ) |>
    extract_dataset()

  # Create emission risk factors for only one source
  emission_risk_factors <- erf_row(
    iso3 = "KNOWN",
    country = "Known Country",
    disease = "Avian infectious laryngotracheitis",
    animal_category = "Domestic",
    species = "Birds",
    disease_notification = 1,
    targeted_surveillance = 1,
    general_surveillance = 1,
    screening = 1,
    precautions_at_the_borders = 1,
    slaughter = 1,
    selective_killing_and_disposal = 1,
    zoning = 1,
    official_vaccination = 1,
    last_outbreak_end_date = as.Date("30/06/1945"),
    commerce_illegal = 0L,
    commerce_legal = 0L
  )

  emission_risk_table <- calc_emission_risk(emission_risk_factors)

  # Should warn about missing emission risk but still complete
  expect_warning(
    ri_entry_points <- calc_entry_point_risk(
      entry_points = entry_points,
      epi_units = epi_units,
      emission_risk = emission_risk_table
    )
  )

  # Should still produce valid output
  expect_s3_class(ri_entry_points, "sf")
  expect_equal(nrow(ri_entry_points), 1)
  expect_true("entry_points_risk" %in% names(ri_entry_points))

  # Points with missing emission risk should have NA
  points <- extract_point_risk(ri_entry_points)
  expect_true(any(is.na(points$point_emission_risk)))
})


test_that("Entry point allocation to epidemiological units works correctly", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Create epidemiological units with specific geometry
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2"),
      EU_NAME = c("Epi Unit 1", "Epi Unit 2"),
      geometry = st_sfc(
        # EU1: square from (0,0) to (1,1)
        st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2,
          byrow = TRUE
        ))),
        # EU2: square from (2,0) to (3,1) - separated from EU1
        st_polygon(list(matrix(
          c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0),
          ncol = 2,
          byrow = TRUE
        )))
      ),
      stringsAsFactors = FALSE
    ),
    crs = 4326
  )

  epi_units <- validate_dataset_content(
    x = epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |>
    extract_dataset()

  # Create entry points: one inside EU1, one inside EU2, one outside both (should go to nearest)
  entry_points_raw <- data.frame(
    POINT_ID = c("EP_IN_EU1", "EP_IN_EU2", "EP_OUTSIDE"),
    POINT_NAME = c("Point in EU1", "Point in EU2", "Point Outside"),
    LONGITUDE = c(0.5, 2.5, 1.5), # 0.5 is in EU1, 2.5 is in EU2, 1.5 is between them
    LATITUDE = c(0.5, 0.5, 0.5),
    MODE = c("C", "C", "C"),
    TYPE = c("AIR", "SEA", "BC"),
    SOURCES = c("SRC1", "SRC1", "SRC1"),
    stringsAsFactors = FALSE
  )

  entry_points <- validate_dataset_content(
    x = entry_points_raw,
    table_name = "entry_points",
    point_name = "POINT_NAME",
    lng = "LONGITUDE",
    lat = "LATITUDE",
    mode = "MODE",
    type = "TYPE",
    sources = "SOURCES"
  ) |>
    extract_dataset()

  # Create emission risk
  emission_risk_factors <- erf_row(
    iso3 = "SRC1",
    country = "Source Country 1",
    disease = "Avian infectious laryngotracheitis",
    animal_category = "Domestic",
    species = "Birds",
    disease_notification = 1,
    targeted_surveillance = 1,
    general_surveillance = 1,
    screening = 1,
    precautions_at_the_borders = 1,
    slaughter = 1,
    selective_killing_and_disposal = 1,
    zoning = 1,
    official_vaccination = 1,
    last_outbreak_end_date = as.Date("30/06/1945"),
    commerce_illegal = 0L,
    commerce_legal = 0L
  )

  emission_risk_table <- calc_emission_risk(emission_risk_factors)

  # Calculate entry point risk
  ri_entry_points <- calc_entry_point_risk(
    entry_points = entry_points,
    epi_units = epi_units,
    emission_risk = emission_risk_table
  )

  # Both EU1 and EU2 should have risk values (EP_OUTSIDE should be allocated to nearest)
  expect_equal(nrow(ri_entry_points), 2)
  expect_setequal(ri_entry_points$eu_id, c("EU1", "EU2"))

  # All should have non-NA risk values
  expect_true(all(!is.na(ri_entry_points$entry_points_risk)))

  # Extract points to verify allocation
  points <- extract_point_risk(ri_entry_points)
  expect_equal(nrow(points), 3)
  expect_setequal(points$point_id, c("ep-001", "ep-002", "ep-003"))
})
