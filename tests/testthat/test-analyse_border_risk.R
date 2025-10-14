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
  epi_units <- validate_dataset(
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

  tunisia <- validate_dataset(
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


test_that("BR-001: Landlocked country without coastline (Zimbabwe)", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Create synthetic landlocked country with multiple neighbors
  # Zimbabwe has 4 neighbors: Zambia (N), Mozambique (E), South Africa (S), Botswana (W)
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2", "EU3", "EU4"),
      EU_NAME = c("North Region", "East Region", "South Region", "West Region"),
      geometry = st_sfc(
        # North
        st_polygon(list(matrix(
          c(0, 1, 1, 1, 1, 2, 0, 2, 0, 1),
          ncol = 2, byrow = TRUE
        ))),
        # East
        st_polygon(list(matrix(
          c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
          ncol = 2, byrow = TRUE
        ))),
        # South
        st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2, byrow = TRUE
        ))),
        # West
        st_polygon(list(matrix(
          c(0, 0, 0, 1, 1, 1, 1, 0, 0, 0),
          ncol = 2, byrow = TRUE
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

  # Create emission risk for 4 neighbors with different risk levels
  emission_risk_factors <- bind_rows(
    erf_row(
      iso3 = "ZMB",
      country = "Zambia",
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
      last_outbreak_end_date = as.Date("30/06/2010"),
      commerce_illegal = 0L,
      commerce_legal = 0L
    ),
    erf_row(
      iso3 = "MOZ",
      country = "Mozambique",
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
      iso3 = "ZAF",
      country = "South Africa",
      disease = "Avian infectious laryngotracheitis",
      animal_category = "Domestic",
      species = "Birds",
      disease_notification = 1,
      targeted_surveillance = 1,
      general_surveillance = 0,
      screening = 1,
      precautions_at_the_borders = 1,
      slaughter = 1,
      selective_killing_and_disposal = 0,
      zoning = 1,
      official_vaccination = 1,
      last_outbreak_end_date = as.Date("30/06/2020"),
      commerce_illegal = 0L,
      commerce_legal = 1L
    ),
    erf_row(
      iso3 = "BWA",
      country = "Botswana",
      disease = "Avian infectious laryngotracheitis",
      animal_category = "Domestic",
      species = "Birds",
      disease_notification = 1,
      targeted_surveillance = 0,
      general_surveillance = 1,
      screening = 1,
      precautions_at_the_borders = 0,
      slaughter = 1,
      selective_killing_and_disposal = 1,
      zoning = 0,
      official_vaccination = 1,
      last_outbreak_end_date = as.Date("30/06/2018"),
      commerce_illegal = 0L,
      commerce_legal = 0L
    )
  )

  emission_risk_table <- calc_emission_risk(emission_risk_factors)

  # Create shared borders - simulate that each EU borders one country
  shared_borders_raw <- st_as_sf(
    data.frame(
      eu_id = c("EU1", "EU2", "EU3", "EU4"),
      bc_id = c("ZMB", "MOZ", "ZAF", "BWA"),
      border_length = c(100, 150, 120, 80),
      geometry = st_sfc(
        st_linestring(matrix(c(0, 2, 1, 2), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(2, 0, 2, 1), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(0, 0, 0, 1), ncol = 2, byrow = TRUE))
      ),
      stringsAsFactors = FALSE
    ),
    crs = 4326
  )

  # Calculate weights
  shared_borders <- shared_borders_raw |>
    group_by(eu_id) |>
    mutate(weight = border_length / sum(border_length)) |>
    ungroup()

  attr(shared_borders, "table_name") <- "shared_borders"

  # Calculate border risk
  ri_border <- calc_border_risk(
    epi_units = epi_units,
    shared_borders = shared_borders,
    emission_risk = emission_risk_table
  )

  # Test structure
  expect_s3_class(ri_border, "sf")
  expect_equal(nrow(ri_border), 4)
  expect_true(all(c("eu_id", "eu_name", "border_risk") %in% names(ri_border)))

  # Test that all risk values are in valid range
  expect_true(all(ri_border$border_risk >= 0 & ri_border$border_risk <= 12))

  # Test that risk is calculated for all EUs
  expect_true(all(!is.na(ri_border$border_risk)))

  # Test attributes
  expect_equal(attr(ri_border, "risk_col"), "border_risk")
  expect_equal(attr(ri_border, "scale"), c(0, 12))

  # Verify border extraction works
  extracted_borders <- extract_border(ri_border)
  expect_s3_class(extracted_borders, "sf")
  expect_equal(nrow(extracted_borders), 4)
})


test_that("BR-002: Coastal country (Tunisia)", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Tunisia has both land borders (Algeria, Libya) and coastline
  # Create synthetic coastal country
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2", "EU3"),
      EU_NAME = c("Coastal North", "Coastal South", "Inland"),
      geometry = st_sfc(
        # Coastal north - borders Algeria and sea
        st_polygon(list(matrix(
          c(0, 1, 1, 1, 1, 2, 0, 2, 0, 1),
          ncol = 2, byrow = TRUE
        ))),
        # Coastal south - borders Libya and sea
        st_polygon(list(matrix(
          c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
          ncol = 2, byrow = TRUE
        ))),
        # Inland - no sea border
        st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2, byrow = TRUE
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

  # Create emission risk for 2 land neighbors
  emission_risk_factors <- bind_rows(
    erf_row(
      iso3 = "DZA",
      country = "Algeria",
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
      last_outbreak_end_date = as.Date("30/06/2015"),
      commerce_illegal = 0L,
      commerce_legal = 0L
    ),
    erf_row(
      iso3 = "LBY",
      country = "Libya",
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

  # Shared borders only on land (not coastline)
  shared_borders_raw <- st_as_sf(
    data.frame(
      eu_id = c("EU1", "EU2", "EU3"),
      bc_id = c("DZA", "LBY", "DZA"),
      border_length = c(200, 180, 50),
      geometry = st_sfc(
        st_linestring(matrix(c(0, 2, 1, 2), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(2, 0, 2, 1), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(0, 0, 0.5, 0), ncol = 2, byrow = TRUE))
      ),
      stringsAsFactors = FALSE
    ),
    crs = 4326
  )

  shared_borders <- shared_borders_raw |>
    group_by(eu_id) |>
    mutate(weight = border_length / sum(border_length)) |>
    ungroup()

  attr(shared_borders, "table_name") <- "shared_borders"

  # Calculate border risk
  ri_border <- calc_border_risk(
    epi_units = epi_units,
    shared_borders = shared_borders,
    emission_risk = emission_risk_table
  )

  # Tests
  expect_s3_class(ri_border, "sf")
  expect_equal(nrow(ri_border), 3)
  expect_true(all(ri_border$border_risk >= 0 & ri_border$border_risk <= 12))

  # Coastal EUs should have border risk from land neighbors
  expect_true(all(!is.na(ri_border$border_risk)))
})


test_that("BR-003: Island country without land borders (Madagascar)", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Madagascar has no land borders, only coastline
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2"),
      EU_NAME = c("North Province", "South Province"),
      geometry = st_sfc(
        st_polygon(list(matrix(
          c(0, 1, 1, 1, 1, 2, 0, 2, 0, 1),
          ncol = 2, byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2, byrow = TRUE
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

  # No neighbors for island country
  emission_risk_factors <- erf_row(
    iso3 = "MDG",
    country = "Madagascar",
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
    last_outbreak_end_date = as.Date("30/06/2015"),
    commerce_illegal = 0L,
    commerce_legal = 0L
  )

  emission_risk_table <- calc_emission_risk(emission_risk_factors)

  # Empty shared borders for island
  shared_borders <- st_as_sf(
    data.frame(
      eu_id = character(0),
      bc_id = character(0),
      border_length = numeric(0),
      weight = numeric(0),
      geometry = st_sfc(crs = 4326)
    ),
    crs = 4326
  )

  attr(shared_borders, "table_name") <- "shared_borders"

  # Calculate border risk with no borders
  ri_border <- calc_border_risk(
    epi_units = epi_units,
    shared_borders = shared_borders,
    emission_risk = emission_risk_table
  )

  # Tests - island should have zero border risk
  expect_s3_class(ri_border, "sf")
  expect_equal(nrow(ri_border), 2)
  expect_true(all(ri_border$border_risk == 0, na.rm = TRUE))

  # Verify attributes
  expect_equal(attr(ri_border, "risk_col"), "border_risk")
  expect_equal(attr(ri_border, "scale"), c(0, 12))
})


test_that("BR-004: Missing emission data for one neighbor", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Simulate Zimbabwe without South Africa emission data
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2"),
      EU_NAME = c("North Border", "South Border"),
      geometry = st_sfc(
        st_polygon(list(matrix(
          c(0, 1, 1, 1, 1, 2, 0, 2, 0, 1),
          ncol = 2, byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2, byrow = TRUE
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

  # Only Zambia emission data, missing South Africa
  emission_risk_factors <- erf_row(
    iso3 = "ZMB",
    country = "Zambia",
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
    last_outbreak_end_date = as.Date("30/06/2010"),
    commerce_illegal = 0L,
    commerce_legal = 0L
  )

  emission_risk_table <- calc_emission_risk(emission_risk_factors)

  # Borders with both Zambia and South Africa
  shared_borders_raw <- st_as_sf(
    data.frame(
      eu_id = c("EU1", "EU2"),
      bc_id = c("ZMB", "ZAF"),
      border_length = c(100, 120),
      geometry = st_sfc(
        st_linestring(matrix(c(0, 2, 1, 2), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE))
      ),
      stringsAsFactors = FALSE
    ),
    crs = 4326
  )

  shared_borders <- shared_borders_raw |>
    group_by(eu_id) |>
    mutate(weight = border_length / sum(border_length)) |>
    ungroup()

  attr(shared_borders, "table_name") <- "shared_borders"

  # Calculate border risk - should handle missing emission data
  ri_border <- calc_border_risk(
    epi_units = epi_units,
    shared_borders = shared_borders,
    emission_risk = emission_risk_table
  )

  # Tests
  expect_s3_class(ri_border, "sf")
  expect_equal(nrow(ri_border), 2)

  # EU1 should have risk from Zambia
  eu1_risk <- ri_border[ri_border$eu_id == "EU1", "border_risk", drop = TRUE]
  expect_true(eu1_risk > 0)

  # EU2 borders South Africa (no emission data), should have NA or 0 risk
  eu2_risk <- ri_border[ri_border$eu_id == "EU2", "border_risk", drop = TRUE]
  expect_true(is.na(eu2_risk) | eu2_risk == 0)
})


test_that("BR-005: Missing emission data for multiple neighbors", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Zimbabwe without South Africa and Zambia emission data
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2", "EU3"),
      EU_NAME = c("North", "South", "East"),
      geometry = st_sfc(
        st_polygon(list(matrix(
          c(0, 1, 1, 1, 1, 2, 0, 2, 0, 1),
          ncol = 2, byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2, byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
          ncol = 2, byrow = TRUE
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

  # Only Mozambique data, missing ZAF and ZMB
  emission_risk_factors <- erf_row(
    iso3 = "MOZ",
    country = "Mozambique",
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

  emission_risk_table <- calc_emission_risk(emission_risk_factors)

  # Borders with ZMB (missing), ZAF (missing), and MOZ (present)
  shared_borders_raw <- st_as_sf(
    data.frame(
      eu_id = c("EU1", "EU2", "EU3"),
      bc_id = c("ZMB", "ZAF", "MOZ"),
      border_length = c(100, 120, 150),
      geometry = st_sfc(
        st_linestring(matrix(c(0, 2, 1, 2), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(2, 0, 2, 1), ncol = 2, byrow = TRUE))
      ),
      stringsAsFactors = FALSE
    ),
    crs = 4326
  )

  shared_borders <- shared_borders_raw |>
    group_by(eu_id) |>
    mutate(weight = border_length / sum(border_length)) |>
    ungroup()

  attr(shared_borders, "table_name") <- "shared_borders"

  # Calculate border risk
  ri_border <- calc_border_risk(
    epi_units = epi_units,
    shared_borders = shared_borders,
    emission_risk = emission_risk_table
  )

  # Tests
  expect_s3_class(ri_border, "sf")
  expect_equal(nrow(ri_border), 3)

  # Only EU3 (Mozambique border) should have risk > 0
  eu3_risk <- ri_border[ri_border$eu_id == "EU3", "border_risk", drop = TRUE]
  expect_true(eu3_risk > 0)

  # EU1 and EU2 should have NA or 0 risk
  eu1_risk <- ri_border[ri_border$eu_id == "EU1", "border_risk", drop = TRUE]
  eu2_risk <- ri_border[ri_border$eu_id == "EU2", "border_risk", drop = TRUE]
  expect_true(is.na(eu1_risk) | eu1_risk == 0)
  expect_true(is.na(eu2_risk) | eu2_risk == 0)
})
