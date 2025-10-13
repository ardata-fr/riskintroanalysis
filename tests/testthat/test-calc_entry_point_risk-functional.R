test_that("Entry points tunisia on example data with no entry points rows", {
  library(dplyr)

  entry_points_fp <-
    system.file(
      package = "riskintrodata",
      "samples",
      "tunisia",
      "entry_points",
      "BORDER_CROSSING_POINTS.csv"
    )

  entry_points <- readr::read_csv(entry_points_fp)

  entry_points <- validate_dataset(
    x = entry_points,
    table_name = "entry_points",
    point_name = "NAME",
    lng = "LONGITUDE_X",
    lat = "LATITUDE_Y",
    mode = "MODE",
    type = "TYPE",
    sources = "SOURCES"
  ) |>
    extract_dataset()

  entry_points <- entry_points[0, ]

  tunisia_raw <- sf::read_sf(system.file(
    package = "riskintrodata",
    "samples",
    "tunisia",
    "epi_units",
    "tunisia_adm2_raw.gpkg"
  ))

  # Apply mapping to prepare colnames and validate dataset

  tunisia <- expect_no_error({

    validate_dataset(
      x = tunisia_raw,
      table_name = "epi_units",
      eu_name = "NAME_2",
      geometry = "geom"
    ) |>
      extract_dataset()

  })

  algeria <- riskintrodata::erf_row(
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
  )

  libya <- riskintrodata::erf_row(
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

  wahis_erf <- riskintrodata::get_wahis_erf(
    disease = "Avian infectious laryngotracheitis",
    animal_category = "Domestic",
    species = "Birds"
  )

  emission_risk_factors <- dplyr::bind_rows(
    algeria,
    libya,
    wahis_erf
  )

  expect_warning({
    emission_risk_table <- calc_emission_risk(
      emission_risk_factors = emission_risk_factors
    )
  })

  expect_warning({
    ri_entry_points <- calc_entry_point_risk(
      entry_points = entry_points,
      epi_units = tunisia,
      emission_risk = emission_risk_table
    )
  })

  expect_warning(expect_warning({
    gg <- plot_risk(ri_entry_points)
    gg
  }))

  expect_warning({
    extract_point_risk(ri_entry_points)
  })

  expect_warning(expect_warning({
    plot_risk(ri_entry_points, interactive = TRUE)
  }))
})

test_that("CU-001: Basic calculation with complete data", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Create test epidemiological unit
  epi_units_raw <- st_as_sf(
    data.frame(
      eu_id = "EU1",
      eu_name = "Test Unit",
      geometry = st_sfc(
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
    table_name = "epi_units"
  ) |>
    extract_dataset()

  # Create entry points:
  # - 2 controlled points: [12,3] and [3,9,3] (emission scores)
  # - 1 uncontrolled point: [3]
  entry_points_raw <- data.frame(
    POINT_ID = c("EP1", "EP1", "EP2", "EP2", "EP2", "EP3"),
    POINT_NAME = c("Point 1", "Point 1", "Point 2", "Point 2", "Point 2", "Point 3"),
    LONGITUDE = c(0.2, 0.2, 0.5, 0.5, 0.5, 0.8),
    LATITUDE = c(0.2, 0.2, 0.5, 0.5, 0.5, 0.8),
    MODE = c("C", "C", "C", "C", "C", "NC"),
    TYPE = c("AIR", "AIR", "SEA", "SEA", "SEA", "BC"),
    SOURCES = c("SRC_12", "SRC_3", "SRC_3B", "SRC_9", "SRC_3C", "SRC_3D"),
    stringsAsFactors = FALSE
  )

  entry_points <- validate_dataset(
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

  # Create emission risk factors with exact scores
  emission_scores <- tibble::tribble(
    ~iso3, ~country, ~emission_risk,
    "SRC_12", "Source 12", 12,
    "SRC_3", "Source 3", 3,
    "SRC_3B", "Source 3B", 3,
    "SRC_9", "Source 9", 9,
    "SRC_3C", "Source 3C", 3,
    "SRC_3D", "Source 3D", 3
  )
  attr(emission_scores, "risk_col") <- "emission_risk"
  attr(emission_scores, "table_validated") <- TRUE
  attr(emission_scores, "table_name") <- "emission_risk_scores"
  attr(emission_scores, "ri_dataset") <- TRUE

  # Calculate entry point risk with default parameters M=100, α=1, β=1, λ=3
  res <- calc_entry_point_risk(
    entry_points = entry_points,
    epi_units = epi_units,
    emission_risk = emission_scores,
    scaling_args = list(
      illegal_factor = 3,  # λ = 3
      coef_legal = 1,      # α = 1
      coef_illegal = 1,    # β = 1
      max_risk = 100       # M = 100
    )
  )

  points <- extract_point_risk(res)

  # Verify exposures: 1.25, 1.25, 0.25
  # EP1: (12 + 3) / 12 = 1.25
  # EP2: (3 + 9 + 3) / 12 = 1.25
  # EP3: 3 / 12 = 0.25
  expect_equal(points$point_exposure, c(1.25, 1.25, 0.25), tolerance = 0.0001)

  # Verify effective numbers: xc = 2.5, xu = 0.25
  # Verify equivalent uncontrolled points: x = 0.83
  # Verify result: 39.33
  expect_equal(res$entry_points_risk[1], 39.33, tolerance = 0.0001)
})


test_that("CU-002: Unit without entry points", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # No entry points
  entry_points_raw <- data.frame(
    POINT_ID = character(0),
    POINT_NAME = character(0),
    LONGITUDE = numeric(0),
    LATITUDE = numeric(0),
    MODE = character(0),
    TYPE = character(0),
    SOURCES = character(0),
    stringsAsFactors = FALSE
  )

  status <- validate_dataset(
    x = entry_points_raw,
    table_name = "entry_points",
    point_name = "POINT_NAME",
    lng = "LONGITUDE",
    lat = "LATITUDE",
    mode = "MODE",
    type = "TYPE",
    sources = "SOURCES"
  )

  expect_no_error(extract_dataset(status))
})


test_that("CU-003: Only controlled entry points", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Create test epidemiological unit
  epi_units_raw <- st_as_sf(
    data.frame(
      eu_id = "EU1",
      eu_name = "Controlled Only Unit",
      geometry = st_sfc(
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
    table_name = "epi_units"
  ) |>
    extract_dataset()

  # 3 controlled points with emission scores
  # [12], [6,6], [3,3,3,3]
  entry_points_raw <- tibble::tribble(
    ~POINT_ID, ~POINT_NAME, ~LONGITUDE, ~LATITUDE, ~MODE, ~TYPE, ~SOURCES,
    "EP1",   "Point 1",        0.2,       0.2,   "C", "AIR", "SRC_12",
    "EP2",   "Point 2",        0.5,       0.5,   "C", "SEA", "SRC_6",
    "EP2",   "Point 2",        0.5,       0.5,   "C", "SEA", "SRC_6",
    "EP3",   "Point 3",        0.8,       0.8,   "C",  "BC", "SRC_3",
    "EP3",   "Point 3",        0.8,       0.8,   "C",  "BC", "SRC_3",
    "EP3",   "Point 3",        0.8,       0.8,   "C",  "BC", "SRC_3",
    "EP3",   "Point 3",        0.8,       0.8,   "C",  "BC", "SRC_3"
  )
  entry_points <- validate_dataset(
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

  # Create emission scores table
  emission_scores <- tibble::tribble(
    ~iso3, ~country, ~emission_risk,
    "SRC_12", "Source 12", 12,
    "SRC_6", "Source 6", 6,
    "SRC_3", "Source 3", 3
  )
  attr(emission_scores, "risk_col") <- "emission_risk"
  attr(emission_scores, "table_validated") <- TRUE
  attr(emission_scores, "table_name") <- "emission_risk_scores"
  attr(emission_scores, "ri_dataset") <- TRUE

  ri_entry_points <- calc_entry_point_risk(
    entry_points = entry_points,
    epi_units = epi_units,
    emission_risk = emission_scores,
    scaling_args = list(
      illegal_factor = 3,
      coef_legal = 1,
      coef_illegal = 1,
      max_risk = 100
    )
  )

  points <- extract_point_risk(ri_entry_points)

  # Verify exposures: 1.0, 1.0, 1.0
  # EP1: 12/12 = 1.0, EP2: (6+6)/12 = 1.0, EP3: (3+3+3+3)/12 = 1.0
  expect_equal(points$point_exposure, c(1.0, 1.0, 1.0), tolerance = 0.01)

  # xc = 3.0, xu = 0, x = 0.62, result = 41.06
  expect_equal(ri_entry_points$exposure_C, 3)
  expect_equal(ri_entry_points$exposure_NC, 0)
  expect_equal(ri_entry_points$entry_points_risk[1], 30.2, tolerance = 0.01)
})


test_that("CU-004: Only uncontrolled entry points", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  epi_units_raw <- st_as_sf(
    data.frame(
      eu_id = "EU1",
      eu_name = "Uncontrolled Only Unit",
      geometry = st_sfc(
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
    table_name = "epi_units"
  ) |>
    extract_dataset()

  # 4 uncontrolled points [12], [9], [6], [3]
  entry_points_raw <- data.frame(
    POINT_ID = c("EP1", "EP2", "EP3", "EP4"),
    POINT_NAME = c("Point 1", "Point 2", "Point 3", "Point 4"),
    LONGITUDE = c(0.2, 0.4, 0.6, 0.8),
    LATITUDE = c(0.2, 0.4, 0.6, 0.8),
    MODE = c("NC", "NC", "NC", "NC"),
    TYPE = c("BC", "CC", "TC", "BC"),
    SOURCES = c("SRC_12", "SRC_9", "SRC_6", "SRC_3"),
    stringsAsFactors = FALSE
  )

  entry_points <- validate_dataset(
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

  # Create emission scores table
  emission_scores <- tibble::tribble(
    ~iso3, ~country, ~emission_risk,
    "SRC_12", "Source 12", 12,
    "SRC_9", "Source 9", 9,
    "SRC_6", "Source 6", 6,
    "SRC_3", "Source 3", 3
  )
  attr(emission_scores, "risk_col") <- "emission_risk"
  attr(emission_scores, "table_validated") <- TRUE
  attr(emission_scores, "table_name") <- "emission_risk_scores"
  attr(emission_scores, "ri_dataset") <- TRUE

  ri_entry_points <- calc_entry_point_risk(
    entry_points = entry_points,
    epi_units = epi_units,
    emission_risk = emission_scores,
    scaling_args = list(
      illegal_factor = 3,
      coef_legal = 1,
      coef_illegal = 1,
      max_risk = 100
    )
  )

  points <- extract_point_risk(ri_entry_points)

  # Verify exposures: 1.0, 0.75, 0.5, 0.25
  # EP1: 12/12 = 1.0, EP2: 9/12 = 0.75, EP3: 6/12 = 0.5, EP4: 3/12 = 0.25
  expect_equal(points$point_exposure, c(1.0, 0.75, 0.5, 0.25), tolerance = 0.0001)

  # xc = 0, xu = 2.5, x = 2.5, result = 92.83
  expect_equal(ri_entry_points$exposure_C, 0)
  expect_equal(ri_entry_points$exposure_NC, 2.5)
  expect_equal(ri_entry_points$entry_points_risk[1], 84.83, tolerance = 0.0001)
})


test_that("CU-005: Balanced controlled/uncontrolled mix", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  epi_units_raw <- st_as_sf(
    data.frame(
      eu_id = "EU1",
      eu_name = "Balanced Mix Unit",
      geometry = st_sfc(
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
    table_name = "epi_units"
  ) |>
    extract_dataset()

  # 3 controlled: [12,6], [9,3,3], [8]
  # 2 uncontrolled: [12,4], [6]
  entry_points_raw <- data.frame(
    POINT_ID = c("EP1", "EP1", "EP2", "EP2", "EP2", "EP3", "EP4", "EP4", "EP5"),
    POINT_NAME = c("Point 1", "Point 1", "Point 2", "Point 2", "Point 2", "Point 3", "Point 4", "Point 4", "Point 5"),
    LONGITUDE = c(0.1, 0.1, 0.3, 0.3, 0.3, 0.5, 0.7, 0.7, 0.9),
    LATITUDE = c(0.1, 0.1, 0.3, 0.3, 0.3, 0.5, 0.7, 0.7, 0.9),
    MODE = c("C", "C", "C", "C", "C", "C", "NC", "NC", "NC"),
    TYPE = c("AIR", "AIR", "SEA", "SEA", "SEA", "BC", "CC", "CC", "TC"),
    SOURCES = c("SRC_12", "SRC_6", "SRC_9", "SRC_3A", "SRC_3B", "SRC_8", "SRC_12B", "SRC_4", "SRC_6B"),
    stringsAsFactors = FALSE
  )

  entry_points <- validate_dataset(
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

  # Create emission scores table
  emission_scores <- tibble::tribble(
    ~iso3, ~country, ~emission_risk,
    "SRC_12", "Source 12", 12,
    "SRC_6", "Source 6", 6,
    "SRC_9", "Source 9", 9,
    "SRC_3A", "Source 3A", 3,
    "SRC_3B", "Source 3B", 3,
    "SRC_8", "Source 8", 8,
    "SRC_12B", "Source 12B", 12,
    "SRC_4", "Source 4", 4,
    "SRC_6B", "Source 6B", 6
  )
  attr(emission_scores, "risk_col") <- "emission_risk"
  attr(emission_scores, "table_validated") <- TRUE
  attr(emission_scores, "table_name") <- "emission_risk_scores"
  attr(emission_scores, "ri_dataset") <- TRUE

  ri_entry_points <- calc_entry_point_risk(
    entry_points = entry_points,
    epi_units = epi_units,
    emission_risk = emission_scores,
    scaling_args = list(
      illegal_factor = 3,
      coef_legal = 1,
      coef_illegal = 1,
      max_risk = 100
    )
  )

  points <- extract_point_risk(ri_entry_points)

  # Verify controlled exposures: 1.5, 1.25, 0.67
  # EP1: (12+6)/12 = 1.5, EP2: (9+3+3)/12 = 1.25, EP3: 8/12 = 0.67
  controlled_points <- points[points$mode == "C", ]
  expect_equal(controlled_points$point_exposure, c(1.5, 1.25, 8/12), tolerance = 0.01)

  # Verify uncontrolled exposures: 1.33, 0.5
  # EP4: (12+4)/12 = 1.33, EP5: 6/12 = 0.5
  uncontrolled_points <- points[points$mode == "NC", ]
  expect_equal(uncontrolled_points$point_exposure, c((12+4)/12, 0.5), tolerance = 0.01)

  # xc = 3.42, xu = 1.83, x = 2.48, result = 84.5

  expect_equal(ri_entry_points$exposure_C[1], 3.42, tolerance = 0.01)
  expect_equal(ri_entry_points$exposure_NC[1], 1.83, tolerance = 0.01)
  expect_equal(ri_entry_points$entry_points_risk[1], 84.5, tolerance = 0.01)
})


test_that("CU-006: Unbalanced mix - controlled majority", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  epi_units_raw <- st_as_sf(
    data.frame(
      eu_id = "EU1",
      eu_name = "Controlled Majority Unit",
      geometry = st_sfc(
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
    table_name = "epi_units"
  ) |>
    extract_dataset()

  # 8 controlled: [12], [9,6], [3,3,6], [12], [8,4], [2,10], [7], [5,5,2]
  # 2 uncontrolled: [12], [8,4]
  entry_points_raw <- data.frame(
    POINT_ID = c("EP1", "EP2", "EP2", "EP3", "EP3", "EP3", "EP4", "EP5", "EP5", "EP6", "EP6", "EP7", "EP8", "EP8", "EP8", "EP9", "EP10", "EP10"),
    POINT_NAME = c("Point 1", "Point 2", "Point 2", "Point 3", "Point 3", "Point 3", "Point 4", "Point 5", "Point 5", "Point 6", "Point 6", "Point 7", "Point 8", "Point 8", "Point 8", "Point 9", "Point 10", "Point 10"),
    LONGITUDE = c(0.1, 0.2, 0.2, 0.3, 0.3, 0.3, 0.4, 0.5, 0.5, 0.6, 0.6, 0.7, 0.8, 0.8, 0.8, 0.1, 0.9, 0.9),
    LATITUDE = c(0.1, 0.2, 0.2, 0.3, 0.3, 0.3, 0.4, 0.5, 0.5, 0.6, 0.6, 0.7, 0.8, 0.8, 0.8, 0.9, 0.1, 0.1),
    MODE = c("C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "NC", "NC", "NC"),
    TYPE = c("AIR", "SEA", "SEA", "BC", "BC", "BC", "AIR", "CC", "CC", "TC", "TC", "BC", "AIR", "AIR", "AIR", "CC", "BC", "BC"),
    SOURCES = c("SRC_12A", "SRC_9", "SRC_6A", "SRC_3A", "SRC_3B", "SRC_6B", "SRC_12B", "SRC_8", "SRC_4", "SRC_2", "SRC_10", "SRC_7", "SRC_5A", "SRC_5B", "SRC_2B", "SRC_12C", "SRC_8B", "SRC_4B"),
    stringsAsFactors = FALSE
  )

  entry_points <- validate_dataset(
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

  # Create emission scores table
  emission_scores <- tibble::tribble(
    ~iso3, ~country, ~emission_risk,
    "SRC_12A", "Source 12A", 12,
    "SRC_9", "Source 9", 9,
    "SRC_6A", "Source 6A", 6,
    "SRC_3A", "Source 3A", 3,
    "SRC_3B", "Source 3B", 3,
    "SRC_6B", "Source 6B", 6,
    "SRC_12B", "Source 12B", 12,
    "SRC_8", "Source 8", 8,
    "SRC_4", "Source 4", 4,
    "SRC_2", "Source 2", 2,
    "SRC_10", "Source 10", 10,
    "SRC_7", "Source 7", 7,
    "SRC_5A", "Source 5A", 5,
    "SRC_5B", "Source 5B", 5,
    "SRC_2B", "Source 2B", 2,
    "SRC_12C", "Source 12C", 12,
    "SRC_8B", "Source 8B", 8,
    "SRC_4B", "Source 4B", 4
  )
  attr(emission_scores, "risk_col") <- "emission_risk"
  attr(emission_scores, "table_validated") <- TRUE
  attr(emission_scores, "table_name") <- "emission_risk_scores"
  attr(emission_scores, "ri_dataset") <- TRUE

  ri_entry_points <- calc_entry_point_risk(
    entry_points = entry_points,
    epi_units = epi_units,
    emission_risk = emission_scores,
    scaling_args = list(
      illegal_factor = 3,
      coef_legal = 1,
      coef_illegal = 1,
      max_risk = 100
    )
  )

  # xc = 1.0 + 1.25 + 1.0 + 1.0 + 1.0 + 1.0 + 0.58 + 1.0 = 7.83
  # xu = 1.0 + 1.0 = 2.0
  # x = 2.69, result = 87.31
  expect_equal(ri_entry_points$exposure_C, 7.83, tolerance = 0.01)
  expect_equal(ri_entry_points$exposure_NC, 2, tolerance = 0.01)
  expect_equal(ri_entry_points$entry_points_risk[1], 87.31, tolerance = 0.01)
})


test_that("CU-007: Unbalanced mix - uncontrolled majority", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  epi_units_raw <- st_as_sf(
    data.frame(
      eu_id = "EU1",
      eu_name = "Uncontrolled Majority Unit",
      geometry = st_sfc(
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
    table_name = "epi_units"
  ) |>
    extract_dataset()

  entry_points_raw <- tibble::tribble(
    ~POINT_ID, ~POINT_NAME, ~LONGITUDE, ~LATITUDE, ~MODE, ~TYPE,  ~SOURCES,
    # 2 controlled: [12,8], [6]
    "EP1",   "Point 1",        0.1,       0.1,   "C", "AIR",   "SRC_12",
    "EP1",   "Point 1",        0.1,       0.1,   "C", "AIR",   "SRC_8",
    "EP2",   "Point 2",        0.3,       0.3,   "C", "SEA",   "SRC_6",
    # 6 uncontrolled: [12], [9], [6,6], [4,8], [10,2], [7,5]
    "EP3",   "Point 3",        0.4,       0.4,  "NC",  "BC",   "SRC_12",
    "EP4",   "Point 4",        0.5,       0.5,  "NC",  "CC",   "SRC_9",
    "EP5",   "Point 5",        0.6,       0.6,  "NC",  "TC",   "SRC_6",
    "EP5",   "Point 5",        0.6,       0.6,  "NC",  "TC",   "SRC_6",
    "EP6",   "Point 6",        0.7,       0.7,  "NC",  "BC",   "SRC_4",
    "EP6",   "Point 6",        0.7,       0.7,  "NC",  "BC",   "SRC_8",
    "EP7",   "Point 7",        0.7,       0.7,  "NC",  "BC",   "SRC_10",
    "EP7",   "Point 7",        0.7,       0.7,  "NC",  "BC",   "SRC_2",
    "EP8",   "Point 8",        0.8,       0.8,  "NC",  "CC",   "SRC_7",
    "EP8",   "Point 8",        0.8,       0.8,  "NC",  "CC",   "SRC_5"
  )

  # Create emission scores table
  emission_scores <- tibble::tribble(
    ~iso3, ~country, ~emission_risk,
    "SRC_12", "Source 12", 12,
    "SRC_8", "Source 8", 8,
    "SRC_6", "Source 6", 6,
    "SRC_9", "Source 9", 9,
    "SRC_4", "Source 4", 4,
    "SRC_10", "Source 10", 10,
    "SRC_2", "Source 2", 2,
    "SRC_7", "Source 7", 7,
    "SRC_5", "Source 5", 5
  )

  attr(emission_scores, "risk_col") <- "emission_risk"
  attr(emission_scores, "table_validated") <- TRUE
  attr(emission_scores, "table_name") <- "emission_risk_scores"
  attr(emission_scores, "ri_dataset") <- TRUE

  entry_points <- validate_dataset(
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

  ri_entry_points <- calc_entry_point_risk(
    entry_points = entry_points,
    epi_units = epi_units,
    emission_risk = emission_scores,
    scaling_args = list(
      illegal_factor = 3,
      coef_legal = 1,
      coef_illegal = 1,
      max_risk = 100
    )
  )

  # xc = 1.67 + 0.5 = 2.17
  # xu = 1.0 + 0.75 + 1.0 + 1.0 + 1.0 + 1.0 = 5.75
  # x = 6.29, result = 99.63
  expect_equal(ri_entry_points$exposure_C[1], 2.17, tolerance = 0.01)
  expect_equal(ri_entry_points$exposure_NC[1], 5.75, tolerance = 0.01)
  expect_equal(ri_entry_points$entry_points_risk[1], 99.63, tolerance = 0.05)
})


test_that("CU-008: Maximum emission scores", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  epi_units_raw <- st_as_sf(
    data.frame(
      eu_id = "EU1",
      eu_name = "Maximum Scores Unit",
      geometry = st_sfc(
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
    table_name = "epi_units"
    ) |>
    extract_dataset()

  # 2 controlled: [12,12] and [12]
  # 1 uncontrolled: [12]
  entry_points_raw <- data.frame(
    POINT_ID = c("EP1", "EP1", "EP2", "EP3"),
    POINT_NAME = c("Point 1", "Point 1", "Point 2", "Point 3"),
    LONGITUDE = c(0.2, 0.2, 0.5, 0.8),
    LATITUDE = c(0.2, 0.2, 0.5, 0.8),
    MODE = c("C", "C", "C", "NC"),
    TYPE = c("AIR", "AIR", "SEA", "BC"),
    SOURCES = c("SRC_12A", "SRC_12B", "SRC_12C", "SRC_12D"),
    stringsAsFactors = FALSE
  )

  entry_points <- validate_dataset(
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

  # Create emission scores table
  emission_scores <- tibble::tribble(
    ~iso3, ~country, ~emission_risk,
    "SRC_12A", "Source 12A", 12,
    "SRC_12B", "Source 12B", 12,
    "SRC_12C", "Source 12C", 12,
    "SRC_12D", "Source 12D", 12
  )
  attr(emission_scores, "risk_col") <- "emission_risk"
  attr(emission_scores, "table_validated") <- TRUE
  attr(emission_scores, "table_name") <- "emission_risk_scores"
  attr(emission_scores, "ri_dataset") <- TRUE

  ri_entry_points <- calc_entry_point_risk(
    entry_points = entry_points,
    epi_units = epi_units,
    emission_risk = emission_scores,
    scaling_args = list(
      illegal_factor = 3,
      coef_legal = 1,
      coef_illegal = 1,
      max_risk = 100
    )
  )

  # xc = 2.0 + 1.0 = 3.0,
  # xu = 1.0
  # x = 1.62, result = 66.96
  expect_equal(ri_entry_points$exposure_C[1], 3, tolerance = 0.01)
  expect_equal(ri_entry_points$exposure_NC[1], 1, tolerance = 0.01)
  expect_equal(ri_entry_points$entry_points_risk[1], 66.96, tolerance = 0.01)
})


test_that("CU-009: Minimum emission scores", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  epi_units_raw <- st_as_sf(
    data.frame(
      eu_id = "EU1",
      eu_name = "Minimum Scores Unit",
      geometry = st_sfc(
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
    table_name = "epi_units"
  ) |>
    extract_dataset()

  # 4 controlled: [1], [2,1], [1,1,1], [2]
  # 3 uncontrolled: [1,2], [1], [2,1]
  entry_points_raw <- data.frame(
    POINT_ID = c("EP1", "EP2", "EP2", "EP3", "EP3", "EP3", "EP4", "EP5", "EP5", "EP6", "EP7", "EP7"),
    POINT_NAME = c("Point 1", "Point 2", "Point 2", "Point 3", "Point 3", "Point 3", "Point 4", "Point 5", "Point 5", "Point 6", "Point 7", "Point 7"),
    LONGITUDE = c(0.1, 0.2, 0.2, 0.3, 0.3, 0.3, 0.4, 0.6, 0.6, 0.8, 0.9, 0.9),
    LATITUDE = c(0.1, 0.2, 0.2, 0.3, 0.3, 0.3, 0.4, 0.6, 0.6, 0.8, 0.9, 0.9),
    MODE = c("C", "C", "C", "C", "C", "C", "C", "NC", "NC", "NC", "NC", "NC"),
    TYPE = c("AIR", "SEA", "SEA", "BC", "BC", "BC", "CC", "TC", "TC", "BC", "CC", "CC"),
    SOURCES = c("SRC_1A", "SRC_2A", "SRC_1B", "SRC_1C", "SRC_1D", "SRC_1E", "SRC_2B", "SRC_1F", "SRC_2C", "SRC_1G", "SRC_2D", "SRC_1H"),
    stringsAsFactors = FALSE
  )

  entry_points <- validate_dataset(
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

  # Create emission scores table
  emission_scores <- tibble::tribble(
    ~iso3, ~country, ~emission_risk,
    "SRC_1A", "Source 1A", 1,
    "SRC_2A", "Source 2A", 2,
    "SRC_1B", "Source 1B", 1,
    "SRC_1C", "Source 1C", 1,
    "SRC_1D", "Source 1D", 1,
    "SRC_1E", "Source 1E", 1,
    "SRC_2B", "Source 2B", 2,
    "SRC_1F", "Source 1F", 1,
    "SRC_2C", "Source 2C", 2,
    "SRC_1G", "Source 1G", 1,
    "SRC_2D", "Source 2D", 2,
    "SRC_1H", "Source 1H", 1
  )
  attr(emission_scores, "risk_col") <- "emission_risk"
  attr(emission_scores, "table_validated") <- TRUE
  attr(emission_scores, "table_name") <- "emission_risk_scores"
  attr(emission_scores, "ri_dataset") <- TRUE

  ri_entry_points <- calc_entry_point_risk(
    entry_points = entry_points,
    epi_units = epi_units,
    emission_risk = emission_scores,
    scaling_args = list(
      illegal_factor = 3,
      coef_legal = 1,
      coef_illegal = 1,
      max_risk = 100
    )
  )

  # xc = 0.083 + 0.25 + 0.25 + 0.167 = 0.75
  # xu = 0.25 + 0.083 + 0.25 = 0.583
  # x = 0.82, result = 38.99
  expect_equal(ri_entry_points$exposure_C[1], 0.75, tolerance = 0.01)
  expect_equal(ri_entry_points$exposure_NC[1], 0.583, tolerance = 0.01)
  expect_equal(ri_entry_points$entry_points_risk[1], 38.99, tolerance = 0.01)
})


test_that("CU-010: High exposure variability", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  epi_units_raw <- st_as_sf(
    data.frame(
      eu_id = "EU1",
      eu_name = "High Variability Unit",
      geometry = st_sfc(
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
    table_name = "epi_units"
  ) |>
    extract_dataset()

  # 2 controlled: [12,12,12] (3.0), [1] (0.083)
  # 2 uncontrolled: [12,9,6] (2.25), [2] (0.167)
  entry_points_raw <- data.frame(
    POINT_ID = c("EP1", "EP1", "EP1", "EP2", "EP3", "EP3", "EP3", "EP4"),
    POINT_NAME = c("Point 1", "Point 1", "Point 1", "Point 2", "Point 3", "Point 3", "Point 3", "Point 4"),
    LONGITUDE = c(0.2, 0.2, 0.2, 0.5, 0.7, 0.7, 0.7, 0.9),
    LATITUDE = c(0.2, 0.2, 0.2, 0.5, 0.7, 0.7, 0.7, 0.9),
    MODE = c("C", "C", "C", "C", "NC", "NC", "NC", "NC"),
    TYPE = c("AIR", "AIR", "AIR", "SEA", "BC", "BC", "BC", "CC"),
    SOURCES = c("SRC_12A", "SRC_12B", "SRC_12C", "SRC_1", "SRC_12D", "SRC_9", "SRC_6", "SRC_2"),
    stringsAsFactors = FALSE
  )

  entry_points <- validate_dataset(
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

  # Create emission scores table
  emission_scores <- tibble::tribble(
    ~iso3, ~country, ~emission_risk,
    "SRC_12A", "Source 12A", 12,
    "SRC_12B", "Source 12B", 12,
    "SRC_12C", "Source 12C", 12,
    "SRC_1", "Source 1", 1,
    "SRC_12D", "Source 12D", 12,
    "SRC_9", "Source 9", 9,
    "SRC_6", "Source 6", 6,
    "SRC_2", "Source 2", 2
  )
  attr(emission_scores, "risk_col") <- "emission_risk"
  attr(emission_scores, "table_validated") <- TRUE
  attr(emission_scores, "table_name") <- "emission_risk_scores"
  attr(emission_scores, "ri_dataset") <- TRUE

  ri_entry_points <- calc_entry_point_risk(
    entry_points = entry_points,
    epi_units = epi_units,
    emission_risk = emission_scores,
    scaling_args = list(
      illegal_factor = 3,
      coef_legal = 1,
      coef_illegal = 1,
      max_risk = 100
    )
  )

  # xc = 3.083,
  # xu = 2.417
  # x = 3.0 (approx), result = 90.91
  expect_equal(ri_entry_points$exposure_C[1], 3.083, tolerance = 0.01)
  expect_equal(ri_entry_points$exposure_NC[1], 2.417, tolerance = 0.01)
  expect_equal(ri_entry_points$entry_points_risk[1], 90.91, tolerance = 0.01)
})

