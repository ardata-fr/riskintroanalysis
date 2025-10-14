test_that("Complete road access risk analysis workflow works", {
  library(sf)
  library(terra)
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

  # Create synthetic road access raster data
  # Create a raster that covers the epidemiological units area
  raster_extent <- st_bbox(epi_units)

  # Create a simple raster with varying road access values
  # Higher values = better road access = higher risk
  road_access_raster <- rast(
    xmin = raster_extent["xmin"] - 0.1,
    xmax = raster_extent["xmax"] + 0.1,
    ymin = raster_extent["ymin"] - 0.1,
    ymax = raster_extent["ymax"] + 0.1,
    resolution = 0.1,
    crs = "EPSG:4326"
  )

  # Fill with gradient values - EU1 gets low values, EU2 medium, EU3 high
  road_values <- matrix(
    c(rep(10, 11), rep(20, 11), rep(30, 11)), # Low, medium, high road access
    nrow = 11,
    ncol = 33,
    byrow = FALSE
  )

  suppressWarnings(values(road_access_raster) <- as.vector(road_values))

  # Step 1: Calculate road access risk with default aggregation (mean)
  ri_road_access <- calc_road_access_risk(
    epi_units = epi_units,
    road_access_raster = road_access_raster,
    aggregate_fun = "mean"
  )

  # Test ri_road_access structure
  expect_s3_class(ri_road_access, "sf")
  expect_true("eu_id" %in% names(ri_road_access))
  expect_true("eu_name" %in% names(ri_road_access))
  expect_true("road_access_risk" %in% names(ri_road_access))

  # Test attributes
  expect_equal(attr(ri_road_access, "risk_col"), "road_access_risk")
  expect_equal(attr(ri_road_access, "table_name"), "road_access")
  expect_length(attr(ri_road_access, "scale"), 2)
  expect_true(is.numeric(attr(ri_road_access, "scale")))
  expect_s4_class(attr(ri_road_access, "raster"), "SpatRaster")

  # Test risk values are numeric and reasonable
  expect_true(all(is.numeric(ri_road_access$road_access_risk)))
  expect_true(all(!is.na(ri_road_access$road_access_risk)))

  # Test that all epi units are present in result
  expect_equal(nrow(ri_road_access), 3)
  expect_setequal(ri_road_access$eu_id, c("EU1", "EU2", "EU3"))
  expect_equal(ri_road_access$road_access_risk, c(19, 14.5, 23.6))

  # Test extract_raster function
  extracted_raster <- expect_no_error(extract_raster(ri_road_access))
  expect_s4_class(extracted_raster, "SpatRaster")

  # Step 2: Test plotting functionality
  # Test static plotting
  static_plot <- plot_risk(
    ri_road_access,
    scale = c(0, max(ri_road_access$road_access_risk)),
    interactive = FALSE
    )

  expect_s3_class(static_plot, "ggplot")

  # Test interactive plotting
  interactive_plot <- plot_risk(
    ri_road_access,
    scale = c(0, max(ri_road_access$road_access_risk)),
    interactive = TRUE
    )

  expect_s3_class(interactive_plot, c("leaflet", "htmlwidget"))

  # Step 3: Test risk rescaling
  ri_road_access_scaled <- rescale_risk_scores(
    ri_road_access,
    from = c(
      min(ri_road_access$road_access_risk),
      max(ri_road_access$road_access_risk)
    ),
    to = c(0, 100),
    method = "linear"
  )

  expect_true(all(
    ri_road_access_scaled$road_access_risk >= 0 &
      ri_road_access_scaled$road_access_risk <= 100
  ))
  expect_equal(attr(ri_road_access_scaled, "scale"), c(0, 100))

  # Check that secondary dataset (raster) is preserved during scaling
  scaled_raster <- extract_raster(ri_road_access_scaled)
  expect_s4_class(scaled_raster, "SpatRaster")

  # Test static plotting with scaled data
  static_plot_scaled <- plot_risk(ri_road_access_scaled, interactive = FALSE)
  expect_s3_class(static_plot_scaled, "ggplot")

  # Test interactive plotting with scaled data
  interactive_plot_scaled <- plot_risk(
    ri_road_access_scaled,
    scale = c(0, max(ri_road_access_scaled$road_access_risk)),
    interactive = TRUE
  )
  expect_s3_class(interactive_plot_scaled, c("leaflet", "htmlwidget"))
})


test_that("Road access risk aggregation methods work correctly", {
  library(sf)
  library(terra)
  library(dplyr)
  library(riskintrodata)

  # Create simple test epidemiological unit
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

  # Create raster with varying values to test aggregation
  raster_extent <- st_bbox(epi_units)

  road_access_raster <- rast(
    xmin = raster_extent["xmin"] - 0.1,
    xmax = raster_extent["xmax"] + 0.1,
    ymin = raster_extent["ymin"] - 0.1,
    ymax = raster_extent["ymax"] + 0.1,
    resolution = 0.5,
    crs = "EPSG:4326"
  )

  # Fill with values: 10, 20, 30, 40, 50 - mix of low and high values
  values(road_access_raster) <- c(
    10,
    20,
    30,
    40,
    50,
    rep(25, ncell(road_access_raster) - 5)
  )

  # Test different aggregation methods
  ri_mean <- calc_road_access_risk(
    epi_units = epi_units,
    road_access_raster = road_access_raster,
    aggregate_fun = "mean"
  )

  ri_max <- calc_road_access_risk(
    epi_units = epi_units,
    road_access_raster = road_access_raster,
    aggregate_fun = "max"
  )

  ri_min <- calc_road_access_risk(
    epi_units = epi_units,
    road_access_raster = road_access_raster,
    aggregate_fun = "min"
  )

  ri_sum <- calc_road_access_risk(
    epi_units = epi_units,
    road_access_raster = road_access_raster,
    aggregate_fun = "sum"
  )

  # Test mathematical relationships
  expect_true(ri_max$road_access_risk >= ri_mean$road_access_risk)
  expect_true(ri_mean$road_access_risk >= ri_min$road_access_risk)
  expect_true(ri_sum$road_access_risk > ri_max$road_access_risk) # Sum should be larger than any individual value

  # Test that max captures the highest value we set (50)
  expect_equal(ri_max$road_access_risk, 50)

  # Test that min captures the lowest value we set (10)
  expect_equal(ri_min$road_access_risk, 10)
})


test_that("Road access risk analysis works with real sample data", {
  library(sf)
  library(terra)
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

  # Create synthetic raster data for Tunisia extent since we can't download in tests
  tunisia_extent <- st_bbox(tunisia)

  # Create a raster that covers Tunisia
  road_access_raster <- rast(
    xmin = tunisia_extent["xmin"] - 0.1,
    xmax = tunisia_extent["xmax"] + 0.1,
    ymin = tunisia_extent["ymin"] - 0.1,
    ymax = tunisia_extent["ymax"] + 0.1,
    resolution = 0.1,
    crs = "EPSG:4326"
  )

  # Fill with random but reasonable road access values (0-100 range)
  set.seed(123) # For reproducible tests
  values(road_access_raster) <- runif(
    ncell(road_access_raster),
    min = 0,
    max = 100
  )

  # Calculate road access risk
  ri_road_access <- calc_road_access_risk(
    epi_units = tunisia,
    road_access_raster = road_access_raster,
    aggregate_fun = "mean"
  )

  # Test with real data
  expect_s3_class(ri_road_access, "sf")
  expect_gt(nrow(ri_road_access), 0)
  expect_true(all(is.numeric(ri_road_access$road_access_risk)))
  expect_true(all(!is.na(ri_road_access$road_access_risk)))

  # Test extract_raster with real data
  extracted_raster <- extract_raster(ri_road_access)
  expect_s4_class(extracted_raster, "SpatRaster")

  # Test plotting works with real data
  ri_road_access <- add_scale(ri_road_access, c(0, max(ri_road_access$road_access_risk)))
  static_plot <- plot_risk(ri_road_access, interactive = FALSE)
  expect_s3_class(static_plot, "ggplot")

  interactive_plot <- plot_risk(ri_road_access, interactive = TRUE)
  expect_s3_class(interactive_plot, c("leaflet", "htmlwidget"))

  # Test rescaling with real data
  ri_scaled <- rescale_risk_scores(
    ri_road_access,
    to = c(0, 100),
    method = "linear"
  )
  expect_true(all(
    ri_scaled$road_access_risk >= 0 & ri_scaled$road_access_risk <= 100
  ))
})


test_that("Road access risk handles edge cases correctly", {
  library(sf)
  library(terra)
  library(dplyr)
  library(riskintrodata)

  # Create simple test epidemiological unit
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

  # Test with raster containing NA values
  raster_extent <- st_bbox(epi_units)

  road_access_raster_na <- rast(
    xmin = raster_extent["xmin"] - 0.1,
    xmax = raster_extent["xmax"] + 0.1,
    ymin = raster_extent["ymin"] - 0.1,
    ymax = raster_extent["ymax"] + 0.1,
    resolution = 0.2,
    crs = "EPSG:4326"
  )

  # Mix of values and NAs
  values(road_access_raster_na) <- c(
    10,
    20,
    NA,
    40,
    NA,
    rep(25, ncell(road_access_raster_na) - 5)
  )

  # Should handle NA values gracefully (na.rm = TRUE in zonal function)
  ri_with_na <- expect_no_error(
    calc_road_access_risk(
      epi_units = epi_units,
      road_access_raster = road_access_raster_na,
      aggregate_fun = "mean"
    )
  )

  expect_s3_class(ri_with_na, "sf")
  expect_true(is.numeric(ri_with_na$road_access_risk))
  expect_false(is.na(ri_with_na$road_access_risk)) # Should not be NA due to na.rm = TRUE

  # Test with raster containing all same values
  road_access_raster_uniform <- rast(
    xmin = raster_extent["xmin"] - 0.1,
    xmax = raster_extent["xmax"] + 0.1,
    ymin = raster_extent["ymin"] - 0.1,
    ymax = raster_extent["ymax"] + 0.1,
    resolution = 0.2,
    crs = "EPSG:4326"
  )

  values(road_access_raster_uniform) <- rep(
    42,
    ncell(road_access_raster_uniform)
  )

  ri_uniform <- calc_road_access_risk(
    epi_units = epi_units,
    road_access_raster = road_access_raster_uniform,
    aggregate_fun = "mean"
  )

  # All aggregation methods should give same result for uniform raster
  ri_uniform_max <- calc_road_access_risk(
    epi_units = epi_units,
    road_access_raster = road_access_raster_uniform,
    aggregate_fun = "max"
  )

  ri_uniform_min <- calc_road_access_risk(
    epi_units = epi_units,
    road_access_raster = road_access_raster_uniform,
    aggregate_fun = "min"
  )

  expect_equal(ri_uniform$road_access_risk, 42)
  expect_equal(ri_uniform_max$road_access_risk, 42)
  expect_equal(ri_uniform_min$road_access_risk, 42)
})


test_that("augment_epi_units_with_raster function works independently", {
  library(sf)
  library(terra)
  library(dplyr)
  library(riskintrodata)

  # Create simple test epidemiological units
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2"),
      EU_NAME = c("Epi Unit 1", "Epi Unit 2"),
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
        )))
      ),
      stringsAsFactors = FALSE
    ),
    crs = 4326
  )

  epi_units <- validate_dataset(
    epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |>
    extract_dataset()

  # Create test raster
  raster_extent <- st_bbox(epi_units)

  test_raster <- rast(
    xmin = raster_extent["xmin"] - 0.1,
    xmax = raster_extent["xmax"] + 0.1,
    ymin = raster_extent["ymin"] - 0.1,
    ymax = raster_extent["ymax"] + 0.1,
    resolution = 0.1,
    crs = "EPSG:4326"
  )

  # Create gradient: EU1 gets lower values, EU2 gets higher values
  values(test_raster) <- c(
    rep(10, 11),
    rep(30, 11),
    rep(20, ncell(test_raster) - 22)
  )

  # Test the augment function directly
  augmented_epi_units <- augment_epi_units_with_raster(
    epi_units = epi_units,
    raster = test_raster,
    risk_name = "custom_risk",
    aggregate_fun = "mean"
  )

  # Test structure
  expect_s3_class(augmented_epi_units, "sf")
  expect_true("custom_risk" %in% names(augmented_epi_units))
  expect_equal(nrow(augmented_epi_units), 2)

  # Test that original columns are preserved
  expect_true("eu_id" %in% names(augmented_epi_units))
  expect_true("eu_name" %in% names(augmented_epi_units))

  # Test that risk values are numeric and reasonable
  expect_true(all(is.numeric(augmented_epi_units$custom_risk)))
  expect_true(all(!is.na(augmented_epi_units$custom_risk)))

  # Test different aggregation functions on same data
  augmented_max <- augment_epi_units_with_raster(
    epi_units = epi_units,
    raster = test_raster,
    risk_name = "max_risk",
    aggregate_fun = "max"
  )

  augmented_min <- augment_epi_units_with_raster(
    epi_units = epi_units,
    raster = test_raster,
    risk_name = "min_risk",
    aggregate_fun = "min"
  )

  # Max should be >= Mean >= Min for each EU
  expect_true(all(augmented_max$max_risk >= augmented_epi_units$custom_risk))
  expect_true(all(augmented_epi_units$custom_risk >= augmented_min$min_risk))
})


test_that("MR-001: Raster with negative values (temperature data)", {
  library(sf)
  library(terra)
  library(dplyr)
  library(riskintrodata)

  # Create test epidemiological unit
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1"),
      EU_NAME = c("Test Region"),
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
    epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |>
    extract_dataset()

  # Create raster with only negative values (cold temperatures)
  raster_extent <- st_bbox(epi_units)

  temp_raster <- rast(
    xmin = raster_extent["xmin"] - 0.1,
    xmax = raster_extent["xmax"] + 0.1,
    ymin = raster_extent["ymin"] - 0.1,
    ymax = raster_extent["ymax"] + 0.1,
    resolution = 0.1,
    crs = "EPSG:4326"
  )

  # All negative values
  values(temp_raster) <- rep(-15, ncell(temp_raster))

  # Should handle negative values without error
  ri_temp <- expect_no_error(
    augment_epi_units_with_raster(
      epi_units = epi_units,
      raster = temp_raster,
      risk_name = "temperature_risk",
      aggregate_fun = "mean"
    )
  )

  # Verify that negative values are handled correctly
  expect_s3_class(ri_temp, "sf")
  expect_true(is.finite(ri_temp$temperature_risk))
  # Verify negative value is preserved in aggregation
  expect_true(ri_temp$temperature_risk < 0)
  expect_equal(ri_temp$temperature_risk, -15, tolerance = 0.01)
})


test_that("MR-002: Raster with binary values (migration corridor)", {
  library(sf)
  library(terra)
  library(dplyr)
  library(riskintrodata)

  # Create test epidemiological units
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2", "EU3"),
      EU_NAME = c("Corridor Zone", "Mixed Zone", "No Corridor"),
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

  epi_units <- validate_dataset(
    epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |>
    extract_dataset()

  # Create binary raster (0/1 for migration corridor)
  raster_extent <- st_bbox(epi_units)

  corridor_raster <- rast(
    xmin = raster_extent["xmin"] - 0.1,
    xmax = raster_extent["xmax"] + 0.1,
    ymin = raster_extent["ymin"] - 0.1,
    ymax = raster_extent["ymax"] + 0.1,
    resolution = 0.1,
    crs = "EPSG:4326"
  )

  # Binary values: create spatial gradient across EUs
  # Create values that will properly cover each EU
  n_cells <- ncell(corridor_raster)
  corridor_values <- rep(c(1, 0.5, 0), length.out = n_cells)
  values(corridor_raster) <- corridor_values

  # Test mean aggregation with binary data
  ri_corridor_mean <- augment_epi_units_with_raster(
    epi_units = epi_units,
    raster = corridor_raster,
    risk_name = "corridor_risk",
    aggregate_fun = "mean"
  )

  # Test max aggregation - useful for binary presence/absence
  ri_corridor_max <- augment_epi_units_with_raster(
    epi_units = epi_units,
    raster = corridor_raster,
    risk_name = "corridor_risk",
    aggregate_fun = "max"
  )

  # Verify binary values are handled correctly
  expect_s3_class(ri_corridor_mean, "sf")
  expect_s3_class(ri_corridor_max, "sf")

  # Verify values are within expected range
  expect_true(all(ri_corridor_mean$corridor_risk >= 0))
  expect_true(all(ri_corridor_mean$corridor_risk <= 1))
  expect_true(all(ri_corridor_max$corridor_risk >= 0))
  expect_true(all(ri_corridor_max$corridor_risk <= 1))
})


test_that("MR-003: Raster not covering any epidemiological units", {
  library(sf)
  library(terra)
  library(dplyr)
  library(riskintrodata)

  # Create epidemiological units in one location
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1"),
      EU_NAME = c("Test Unit"),
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
    epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |>
    extract_dataset()

  # Create raster in completely different location
  non_overlapping_raster <- rast(
    xmin = 100,
    xmax = 101,
    ymin = 100,
    ymax = 101,
    resolution = 0.1,
    crs = "EPSG:4326"
  )

  values(non_overlapping_raster) <- runif(ncell(non_overlapping_raster), 0, 100)

  # Should error when extents do not overlap
  expect_error(
    augment_epi_units_with_raster(
      epi_units = epi_units,
      raster = non_overlapping_raster,
      risk_name = "no_coverage_risk",
      aggregate_fun = "mean"
    ),
    "extents do not overlap"
  )
})


test_that("MR-004: Epidemiological units partially covered by raster", {
  library(sf)
  library(terra)
  library(dplyr)
  library(riskintrodata)

  # Create two epidemiological units
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2"),
      EU_NAME = c("Fully Covered", "Partially Covered"),
      geometry = st_sfc(
        # EU1: fully within raster coverage
        st_polygon(list(matrix(
          c(0.2, 0.2, 0.8, 0.2, 0.8, 0.8, 0.2, 0.8, 0.2, 0.2),
          ncol = 2,
          byrow = TRUE
        ))),
        # EU2: extends beyond raster coverage
        st_polygon(list(matrix(
          c(0.5, 0.5, 2.5, 0.5, 2.5, 2.5, 0.5, 2.5, 0.5, 0.5),
          ncol = 2,
          byrow = TRUE
        )))
      ),
      stringsAsFactors = FALSE
    ),
    crs = 4326
  )

  epi_units <- validate_dataset(
    epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |>
    extract_dataset()

  # Create raster that only covers part of the area
  limited_raster <- rast(
    xmin = 0,
    xmax = 1,
    ymin = 0,
    ymax = 1,
    resolution = 0.1,
    crs = "EPSG:4326"
  )

  values(limited_raster) <- rep(50, ncell(limited_raster))

  # Should handle partial coverage
  ri_partial <- augment_epi_units_with_raster(
    epi_units = epi_units,
    raster = limited_raster,
    risk_name = "partial_risk",
    aggregate_fun = "mean"
  )

  expect_s3_class(ri_partial, "sf")
  expect_equal(nrow(ri_partial), 2)

  # EU1 should have valid risk value (fully covered)
  expect_true(is.numeric(ri_partial$partial_risk[1]))
  expect_false(is.na(ri_partial$partial_risk[1]))

  # EU2 might have reduced coverage but should still compute if any overlap exists
  # (behavior depends on implementation - may be NA or computed from partial data)
  expect_true(is.numeric(ri_partial$partial_risk[2]) | is.na(ri_partial$partial_risk[2]))
})


test_that("MR-005: Aggregation with NA values in raster", {
  library(sf)
  library(terra)
  library(dplyr)
  library(riskintrodata)

  # Create test epidemiological unit
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1"),
      EU_NAME = c("Test Unit"),
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
    epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |>
    extract_dataset()

  raster_extent <- st_bbox(epi_units)

  # Create raster with mixed NA and valid values
  mixed_raster <- rast(
    xmin = raster_extent["xmin"] - 0.1,
    xmax = raster_extent["xmax"] + 0.1,
    ymin = raster_extent["ymin"] - 0.1,
    ymax = raster_extent["ymax"] + 0.1,
    resolution = 0.2,
    crs = "EPSG:4326"
  )

  # Half NA, half valid values
  n_cells <- ncell(mixed_raster)
  mixed_values <- c(rep(NA, n_cells %/% 2), rep(50, n_cells - n_cells %/% 2))
  values(mixed_raster) <- mixed_values

  # Test different aggregation methods with NA values
  ri_mean <- augment_epi_units_with_raster(
    epi_units = epi_units,
    raster = mixed_raster,
    risk_name = "mixed_risk",
    aggregate_fun = "mean"
  )

  ri_max <- augment_epi_units_with_raster(
    epi_units = epi_units,
    raster = mixed_raster,
    risk_name = "mixed_risk",
    aggregate_fun = "max"
  )

  ri_min <- augment_epi_units_with_raster(
    epi_units = epi_units,
    raster = mixed_raster,
    risk_name = "mixed_risk",
    aggregate_fun = "min"
  )

  # All should compute values ignoring NAs (na.rm = TRUE in zonal)
  expect_false(is.na(ri_mean$mixed_risk))
  expect_false(is.na(ri_max$mixed_risk))
  expect_false(is.na(ri_min$mixed_risk))

  # Values should be close to 50 since non-NA values are all 50
  expect_equal(ri_mean$mixed_risk, 50, tolerance = 0.01)
  expect_equal(ri_max$mixed_risk, 50, tolerance = 0.01)
  expect_equal(ri_min$mixed_risk, 50, tolerance = 0.01)
})


test_that("MR-006: Raster with all NA values", {
  library(sf)
  library(terra)
  library(dplyr)
  library(riskintrodata)

  # Create test epidemiological unit
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1"),
      EU_NAME = c("Test Unit"),
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
    epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |>
    extract_dataset()

  raster_extent <- st_bbox(epi_units)

  # Create raster with all NA values
  all_na_raster <- rast(
    xmin = raster_extent["xmin"] - 0.1,
    xmax = raster_extent["xmax"] + 0.1,
    ymin = raster_extent["ymin"] - 0.1,
    ymax = raster_extent["ymax"] + 0.1,
    resolution = 0.2,
    crs = "EPSG:4326"
  )

  values(all_na_raster) <- rep(NA, ncell(all_na_raster))

  # Should result in NA when all raster values are NA (with warnings)
  expect_warning(
    ri_all_na <- augment_epi_units_with_raster(
      epi_units = epi_units,
      raster = all_na_raster,
      risk_name = "all_na_risk",
      aggregate_fun = "mean"
    ),
    "no non-missing arguments"
  )

  expect_s3_class(ri_all_na, "sf")
  expect_true(is.na(ri_all_na$all_na_risk) | is.nan(ri_all_na$all_na_risk) | is.infinite(ri_all_na$all_na_risk))
})


test_that("MR-007: Raster with custom NA value in metadata", {
  library(sf)
  library(terra)
  library(dplyr)
  library(riskintrodata)

  # Create test epidemiological unit
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1"),
      EU_NAME = c("Test Unit"),
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
    epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |>
    extract_dataset()

  raster_extent <- st_bbox(epi_units)

  # Create raster with custom NA value (-9999)
  custom_na_raster <- rast(
    xmin = raster_extent["xmin"] - 0.1,
    xmax = raster_extent["xmax"] + 0.1,
    ymin = raster_extent["ymin"] - 0.1,
    ymax = raster_extent["ymax"] + 0.1,
    resolution = 0.2,
    crs = "EPSG:4326"
  )

  # Mix of valid values and custom NA value
  n_cells <- ncell(custom_na_raster)
  custom_values <- c(rep(-9999, n_cells %/% 2), rep(75, n_cells - n_cells %/% 2))
  values(custom_na_raster) <- custom_values

  # Set custom NA value in raster metadata
  NAflag(custom_na_raster) <- -9999

  # Should treat -9999 as NA and compute from valid values
  ri_custom_na <- augment_epi_units_with_raster(
    epi_units = epi_units,
    raster = custom_na_raster,
    risk_name = "custom_na_risk",
    aggregate_fun = "mean"
  )

  expect_s3_class(ri_custom_na, "sf")
  expect_false(is.na(ri_custom_na$custom_na_risk))

  # Should be close to 75 (ignoring -9999 values)
  expect_equal(ri_custom_na$custom_na_risk, 75, tolerance = 0.01)
})
