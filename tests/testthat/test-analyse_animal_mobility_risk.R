test_that("Complete animal mobility risk analysis workflow works", {
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
  epi_units <- validate_dataset(
    x = epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |>
    extract_dataset()

  # Create test animal mobility data
  # Flows from external countries (SRC1, SRC2) to destinations within study area (TUN)
  animal_mobility_raw <- data.frame(
    O_ISO3 = c("SRC1", "SRC1", "SRC2", "SRC2", "SRC1"),
    O_NAME = c("Origin 1", "Origin 1", "Origin 2", "Origin 2", "Origin 1"),
    O_COUNTRY = c(
      "Source Country 1",
      "Source Country 1",
      "Source Country 2",
      "Source Country 2",
      "Source Country 1"
    ),
    O_LNG = c(-1.0, -1.0, -2.0, -2.0, -1.0),
    O_LAT = c(0.5, 0.5, 0.5, 0.5, 0.5),
    D_ISO3 = c("TUN", "TUN", "TUN", "TUN", "TUN"),
    D_NAME = c("Dest 1", "Dest 2", "Dest 1", "Dest 3", "Dest 2"),
    D_LNG = c(0.5, 1.5, 0.5, 2.5, 1.5),
    D_LAT = c(0.5, 0.5, 0.5, 0.5, 0.5),
    QUANTITY = c(100, 200, 150, 75, 50),
    stringsAsFactors = FALSE
  )

  # Apply mapping to prepare and validate animal mobility data
  animal_mobility <- validate_dataset(
    x = animal_mobility_raw,
    table_name = "animal_mobility",
    o_iso3 = "O_ISO3",
    o_name = "O_NAME",
    o_lng = "O_LNG",
    o_lat = "O_LAT",
    d_iso3 = "D_ISO3",
    d_name = "D_NAME",
    d_lng = "D_LNG",
    d_lat = "D_LAT",
    quantity = "QUANTITY"
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

  # Step 1: Calculate animal mobility risk
  ri_animal_mobility <- calc_animal_mobility_risk(
    animal_mobility = animal_mobility,
    emission_risk = emission_risk_table,
    epi_units = epi_units,
    method = "mean",
    # required when using synthetic data that does not correspond to an actual country
    eu_country_iso3 = "TUN"
  )

  # Test ri_animal_mobility structure
  expect_s3_class(ri_animal_mobility, "sf")
  expect_true("eu_id" %in% names(ri_animal_mobility))
  expect_true("eu_name" %in% names(ri_animal_mobility))
  expect_true("animal_mobility_risk" %in% names(ri_animal_mobility))

  # Test attributes
  expect_equal(attr(ri_animal_mobility, "risk_col"), "animal_mobility_risk")
  expect_equal(attr(ri_animal_mobility, "table_name"), "animal_mobility")
  expect_equal(attr(ri_animal_mobility, "scale"), c(0, 12))
  expect_s3_class(attr(ri_animal_mobility, "flows"), "sf")

  # Test risk values are in valid range
  expect_true(all(
    ri_animal_mobility$animal_mobility_risk >= 0 &
      ri_animal_mobility$animal_mobility_risk <= 12,
    na.rm = TRUE
  ))

  # Test extract_flow_risk function
  extracted_flows <- expect_no_error(extract_flow_risk(ri_animal_mobility))

  expect_s3_class(extracted_flows, "sf")
  expect_true("d_name" %in% names(extracted_flows))
  expect_true("emission_risk_weighted" %in% names(extracted_flows))
  expect_true("source_label" %in% names(extracted_flows))

  # Test that all epi units are present in result
  expect_equal(nrow(ri_animal_mobility), 3)
  expect_setequal(ri_animal_mobility$eu_id, c("EU1", "EU2", "EU3"))

  # Test that flows were allocated correctly to destinations
  extracted_flows_df <- st_drop_geometry(extracted_flows)
  expect_equal(nrow(extracted_flows_df), 3) # Should have 3 unique destinations
  expect_setequal(extracted_flows_df$d_name, c("Dest 1", "Dest 2", "Dest 3"))

  # Step 2: Test plotting functionality
  # Test static plotting
  static_plot <- plot_risk(ri_animal_mobility, interactive = FALSE)
  expect_s3_class(static_plot, "ggplot")

  # Test interactive plotting
  interactive_plot <- plot_risk(ri_animal_mobility, interactive = TRUE)
  expect_s3_class(interactive_plot, c("leaflet", "htmlwidget"))

  # Step 3: Test risk rescaling
  ri_animal_mobility_scaled <- rescale_risk_scores(
    ri_animal_mobility,
    to = c(0, 100),
    method = "linear"
  )

  expect_true(all(
    ri_animal_mobility_scaled$animal_mobility_risk >= 0 &
      ri_animal_mobility_scaled$animal_mobility_risk <= 100,
    na.rm = TRUE
  ))
  expect_equal(attr(ri_animal_mobility_scaled, "scale"), c(0, 100))

  # Check that secondary dataset (flows) has been scaled
  scaled_flows <- extract_flow_risk(ri_animal_mobility_scaled)
  expect_equal(attr(scaled_flows, "scale"), c(0, 100))

  # Test static plotting with scaled data
  static_plot_scaled <- plot_risk(
    ri_animal_mobility_scaled,
    interactive = FALSE
  )
  expect_s3_class(static_plot_scaled, "ggplot")

  # Test interactive plotting with scaled data
  interactive_plot_scaled <- plot_risk(
    ri_animal_mobility_scaled,
    interactive = TRUE
  )
  expect_s3_class(interactive_plot_scaled, c("leaflet", "htmlwidget"))
})


test_that("Animal mobility risk aggregation methods work correctly", {
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
          c(0, 0, 2, 0, 2, 1, 0, 1, 0, 0),
          ncol = 2,
          byrow = TRUE
        )))
      ),
      stringsAsFactors = FALSE
    ),
    crs = 4326
  )

  epi_units <- validate_dataset(
    x = epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |>
    extract_dataset()

  # Create animal mobility with multiple destinations in same EU - testing aggregation
  animal_mobility_raw <- data.frame(
    O_ISO3 = c("LOW", "HIGH", "MED"),
    O_NAME = c("Low Risk Origin", "High Risk Origin", "Med Risk Origin"),
    O_COUNTRY = c("Low Risk Country", "High Risk Country", "Med Risk Country"),
    O_LNG = c(-1.0, -2.0, -3.0),
    O_LAT = c(0.5, 0.5, 0.5),
    D_ISO3 = c("TUN", "TUN", "TUN"),
    D_NAME = c("Dest 1", "Dest 2", "Dest 1"), # Dest 1 receives from LOW and MED, Dest 2 from HIGH
    D_LNG = c(0.5, 1.5, 0.5), # Both destinations within EU1
    D_LAT = c(0.5, 0.5, 0.5),
    QUANTITY = c(100, 200, 50), # Different quantities for weighting
    stringsAsFactors = FALSE
  )

  animal_mobility <- validate_dataset(
    x = animal_mobility_raw,
    table_name = "animal_mobility",
    o_iso3 = "O_ISO3",
    o_name = "O_NAME",
    o_lng = "O_LNG",
    o_lat = "O_LAT",
    d_iso3 = "D_ISO3",
    d_name = "D_NAME",
    d_lng = "D_LNG",
    d_lat = "D_LAT",
    quantity = "QUANTITY"
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

  # Test mean aggregation method
  ri_mean <- calc_animal_mobility_risk(
    animal_mobility = animal_mobility,
    emission_risk = emission_risk_table,
    epi_units = epi_units,
    method = "mean",
    eu_country_iso3 = "TUN"
  )

  # Test max aggregation method
  ri_max <- calc_animal_mobility_risk(
    animal_mobility = animal_mobility,
    emission_risk = emission_risk_table,
    epi_units = epi_units,
    method = "max",
    eu_country_iso3 = "TUN"
  )

  # Test min aggregation method
  ri_min <- calc_animal_mobility_risk(
    animal_mobility = animal_mobility,
    emission_risk = emission_risk_table,
    epi_units = epi_units,
    method = "min",
    eu_country_iso3 = "TUN"
  )

  # Max should be >= Mean >= Min
  expect_true(ri_max$animal_mobility_risk >= ri_mean$animal_mobility_risk)
  expect_true(ri_mean$animal_mobility_risk >= ri_min$animal_mobility_risk)

  # Test that flows are properly aggregated by destination and weighted by quantity
  flows_mean <- extract_flow_risk(ri_mean)
  expect_equal(nrow(flows_mean), 2) # Should have 2 unique destinations
  expect_setequal(st_drop_geometry(flows_mean)$d_name, c("Dest 1", "Dest 2"))
})


test_that("Animal mobility risk analysis works with real sample data", {
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
      "animal_mobility",
      "ANIMAL_MOBILITY_raw.csv"
    ) !=
      "",
    "Sample animal mobility data not available"
  )

  # Load real sample epidemiological units
  tunisia_raw <- sf::read_sf(system.file(
    package = "riskintrodata",
    "samples",
    "tunisia",
    "epi_units",
    "tunisia_adm2_raw.gpkg"
  ))

  tunisia <- validate_dataset(
    x = tunisia_raw,
    table_name = "epi_units",
    eu_name = "NAME_2",
    geometry = "geom"
  ) |>
    extract_dataset()

  # Load real sample animal mobility data
  animal_mobility_raw <- readr::read_csv(
    system.file(
      package = "riskintrodata",
      "samples",
      "tunisia",
      "animal_mobility",
      "ANIMAL_MOBILITY_raw.csv"
    ),
    show_col_types = FALSE
  )

  animal_mobility <- validate_dataset(
    x = animal_mobility_raw,
    table_name = "animal_mobility",
    o_name = "ORIGIN_NAME",
    o_lng = "ORIGIN_LONGITUDE_X",
    o_lat = "ORIGIN_LATITUDE_Y",
    d_name = "DESTINATION_NAME",
    d_lng = "DESTINATION_LONGITUDE_X",
    d_lat = "DESTINATION_LATITUDE_Y",
    quantity = "HEADCOUNT"
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

  # Calculate animal mobility risk

  expect_warning(
    ri_animal_mobility <- calc_animal_mobility_risk(
      animal_mobility = animal_mobility,
      emission_risk = emission_risk_table,
      epi_units = tunisia,
      method = "mean"
    )
  )

  # Test with real data
  expect_s3_class(ri_animal_mobility, "sf")
  expect_gt(nrow(ri_animal_mobility), 0)
  expect_true(all(
    ri_animal_mobility$animal_mobility_risk >= 0 &
      ri_animal_mobility$animal_mobility_risk <= 12,
    na.rm = TRUE
  ))

  # Test extract_flow_risk with real data
  extracted_flows <- extract_flow_risk(ri_animal_mobility)
  expect_s3_class(extracted_flows, "sf")
  expect_gt(nrow(extracted_flows), 0)

  # Test plotting works with real data
  static_plot <- plot_risk(ri_animal_mobility, interactive = FALSE)
  expect_s3_class(static_plot, "ggplot")

  interactive_plot <- plot_risk(ri_animal_mobility, interactive = TRUE)
  expect_s3_class(interactive_plot, c("leaflet", "htmlwidget"))

  # Test rescaling with real data
  ri_scaled <- rescale_risk_scores(
    ri_animal_mobility,
    to = c(0, 100),
    method = "linear"
  )
  expect_true(all(
    ri_scaled$animal_mobility_risk >= 0 & ri_scaled$animal_mobility_risk <= 100,
    na.rm = TRUE
  ))
})

test_that("Animal mobility quantity weighting works correctly", {
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

  epi_units <- validate_dataset(
    x = epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |>
    extract_dataset()

  # Create animal mobility where same destination receives from different sources with different quantities
  animal_mobility_raw <- data.frame(
    O_ISO3 = c("LOW", "HIGH"),
    O_NAME = c("Low Risk Origin", "High Risk Origin"),
    O_COUNTRY = c("Low Risk Country", "High Risk Country"),
    O_LNG = c(-1.0, -2.0),
    O_LAT = c(0.5, 0.5),
    D_ISO3 = c("TUN", "TUN"),
    D_NAME = c("Dest 1", "Dest 1"), # Same destination
    D_LNG = c(0.5, 0.5),
    D_LAT = c(0.5, 0.5),
    QUANTITY = c(900, 100), # Heavy weighting toward LOW risk source
    stringsAsFactors = FALSE
  )

  animal_mobility <- validate_dataset(
    x = animal_mobility_raw,
    table_name = "animal_mobility",
    o_iso3 = "O_ISO3",
    o_name = "O_NAME",
    o_lng = "O_LNG",
    o_lat = "O_LAT",
    d_iso3 = "D_ISO3",
    d_name = "D_NAME",
    d_lng = "D_LNG",
    d_lat = "D_LAT",
    quantity = "QUANTITY"
  ) |> extract_dataset()

  # Create emission risk factors with very different risk levels
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
    )
  )

  emission_risk_table <- calc_emission_risk(emission_risk_factors)

  # Calculate risk
  ri_animal_mobility <- calc_animal_mobility_risk(
    animal_mobility = animal_mobility,
    emission_risk = emission_risk_table,
    epi_units = epi_units,
    method = "mean",
    eu_country_iso3 = "TUN"
  )

  # Get the flows to check weighting
  flows <- extract_flow_risk(ri_animal_mobility)

  # The weighted risk should be closer to LOW risk due to quantity weighting (900 vs 100)
  # Weight for LOW = 900/(900+100) = 0.9
  # Weight for HIGH = 100/(900+100) = 0.1
  # Expected weighted risk = (LOW_risk * 0.9) + (HIGH_risk * 0.1)

  low_risk <- emission_risk_table[
    emission_risk_table$iso3 == "LOW",
    "emission_risk",
    drop = TRUE
  ]
  high_risk <- emission_risk_table[
    emission_risk_table$iso3 == "HIGH",
    "emission_risk",
    drop = TRUE
  ]
  expected_weighted_risk <- (low_risk * 0.9) + (high_risk * 0.1)

  expect_equal(
    flows$emission_risk_weighted,
    expected_weighted_risk,
    tolerance = 0.01
  )
})

