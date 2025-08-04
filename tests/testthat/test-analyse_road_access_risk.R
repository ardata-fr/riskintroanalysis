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
  epi_units <- validate_dataset_content(
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
  expect_null(attr(ri_road_access, "scale")) # Road access doesn't have predefined scale
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
  static_plot <- plot_risk(ri_road_access, interactive = FALSE)
  static_plot
  expect_s3_class(static_plot, "ggplot")

  # Test interactive plotting
  interactive_plot <- plot_risk(ri_road_access, interactive = TRUE)
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

  epi_units <- validate_dataset_content(
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

  tunisia <- validate_dataset_content(
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
  static_plot <- plot_risk(ri_road_access, interactive = FALSE)
  expect_s3_class(static_plot, "ggplot")

  interactive_plot <- plot_risk(ri_road_access, interactive = TRUE)
  expect_s3_class(interactive_plot, c("leaflet", "htmlwidget"))

  # Test rescaling with real data
  ri_scaled <- rescale_risk_scores(
    ri_road_access,
    from = c(
      min(ri_road_access$road_access_risk),
      max(ri_road_access$road_access_risk)
    ),
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

  epi_units <- validate_dataset_content(
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

  epi_units <- validate_dataset_content(
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
