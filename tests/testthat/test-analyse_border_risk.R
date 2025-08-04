test_that("Complete border risk analysis workflow works", {
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

  # Use real sample data for more realistic testing
  tunisia_raw <- sf::read_sf(system.file(
    package = "riskintrodata",
    "samples",
    "tunisia",
    "epi_units",
    "tunisia_adm2_raw.gpkg"
  ))

  # Apply mapping to prepare and validate dataset
  epi_units <- validate_dataset_content(
    x = tunisia_raw,
    table_name = "epi_units",
    eu_name = "NAME_2",
    geometry = "geom"
  ) |>
    extract_dataset()

  # Create test emission risk factors for neighboring countries
  emission_risk_factors <- bind_rows(
    erf_row(
      iso3 = "DZA",
      country = "Algeria",
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
      commerce_illegal = 1L,
      commerce_legal = 1
    )
  )

  emission_risk_table <- calc_emission_risk(emission_risk_factors)

  # Step 1: Calculate border lengths using real Tunisia data
  shared_borders <- calc_border_lengths(
    epi_units = epi_units,
    neighbours = c("LBY")
  )

  # Test shared_borders structure
  expect_s3_class(shared_borders, "sf")
  expect_true("eu_id" %in% names(shared_borders))
  expect_true("bc_id" %in% names(shared_borders))
  expect_true("border_length" %in% names(shared_borders))
  expect_true("weight" %in% names(shared_borders))
  expect_true(all(shared_borders$weight >= 0 & shared_borders$weight <= 1))

  # Test that weights sum to 1 for each EU (only for EUs that have borders)
  weight_sums <- shared_borders |>
    st_drop_geometry() |>
    group_by(eu_id) |>
    summarise(total_weight = sum(weight), .groups = "drop")
  expect_true(all(abs(weight_sums$total_weight - 1) < 1e-10))

  # Step 2: Calculate border risk
  ri_border <- calc_border_risk(
    epi_units = epi_units,
    shared_borders = shared_borders,
    emission_risk = emission_risk_table
  )

  # Test ri_border structure
  expect_s3_class(ri_border, "sf")
  expect_true("eu_id" %in% names(ri_border))
  expect_true("eu_name" %in% names(ri_border))
  expect_true("border_risk" %in% names(ri_border))

  # Test attributes
  expect_equal(attr(ri_border, "risk_col"), "border_risk")
  expect_equal(attr(ri_border, "table_name"), "border_risk")
  expect_equal(attr(ri_border, "scale"), c(0, 12))
  expect_s3_class(attr(ri_border, "borders"), "sf")

  # Test risk values are in valid range
  expect_true(all(
    ri_border$border_risk >= 0 & ri_border$border_risk <= 12,
    na.rm = TRUE
  ))

  extracted_borders <- expect_no_error(extract_border(ri_border))

  # Test ri_border structure
  expect_s3_class(extracted_borders, "sf")
  expect_true("eu_id" %in% names(extracted_borders))
  expect_true("bc_id" %in% names(extracted_borders))
  expect_true("border_length" %in% names(extracted_borders))
  expect_true("border_risk" %in% names(extracted_borders))
  expect_true("border_label" %in% names(extracted_borders))

  # Test extract_border function
  expect_equal(attr(extracted_borders, "risk_col"), "border_risk")
  expect_equal(attr(extracted_borders, "table_name"), "shared_borders")
  expect_equal(attr(extracted_borders, "scale"), c(0, 12))

  # Step 3: Test plotting functionality
  # Test static plotting
  static_plot <- plot_risk(ri_border, interactive = FALSE)
  expect_s3_class(static_plot, "ggplot")

  # Test interactive plotting
  interactive_plot <- plot_risk(ri_border, interactive = TRUE)
  expect_s3_class(interactive_plot, c("leaflet", "htmlwidget"))

  # Test risk rescaling
  ri_border_scaled <- rescale_risk_scores(
    ri_border,
    to = c(0, 100),
    method = "linear"
  )

  expect_true(all(
    ri_border_scaled$border_risk >= 0 & ri_border_scaled$border_risk <= 100,
    na.rm = TRUE
  ))
  expect_equal(attr(ri_border_scaled, "scale"), c(0, 100))

  borders <- extract_border(ri_border)
  expect_equal(attr(borders, "risk_col"), "border_risk")
  expect_equal(attr(borders, "table_name"), "shared_borders")
  expect_equal(attr(borders, "scale"), c(0, 12))

  # Check secondary dataset has been scaled.
  scaled_borders <- extract_border(ri_border_scaled)
  expect_equal(attr(scaled_borders, "scale"), c(0, 100))

  # Test static plotting
  static_plot <- plot_risk(ri_border_scaled, interactive = FALSE)
  expect_s3_class(static_plot, "ggplot")

  # Test interactive plotting
  interactive_plot <- plot_risk(ri_border_scaled, interactive = TRUE)
  expect_s3_class(interactive_plot, c("leaflet", "htmlwidget"))
})


test_that("Border risk analysis works with real sample data", {
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

  # Load real sample data
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

  # Get a subset of neighbouring countries for faster testing
  test_neighbours <- c("LBY")
  bordering_countries <- riskintrodata::world_sf |>
    filter(iso3 %in% test_neighbours)

  # Create test emission risk
  emission_risk_factors <- erf_row(
    iso3 = "LBY",
    country = "Border Country 2",
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

  emission_risk_table <- calc_emission_risk(emission_risk_factors)

  # Calculate shared borders (this may take a while)
  shared_borders <- calc_border_lengths(
    epi_units = tunisia,
    eu_country_iso3 = "TUN",
    neighbours = test_neighbours
  )

  # Calculate border risk
  ri_border <- calc_border_risk(
    epi_units = tunisia,
    shared_borders = shared_borders,
    emission_risk = emission_risk_table
  )

  # Test with real data
  expect_s3_class(ri_border, "sf")
  expect_gt(nrow(ri_border), 0)
  expect_true(all(
    ri_border$border_risk >= 0 & ri_border$border_risk <= 12,
    na.rm = TRUE
  ))

  # Test plotting works with real data
  static_plot <- plot_risk(ri_border, interactive = FALSE)
  expect_s3_class(static_plot, "ggplot")

  interactive_plot <- plot_risk(ri_border, interactive = TRUE)
  expect_s3_class(interactive_plot, c("leaflet", "htmlwidget"))
})
