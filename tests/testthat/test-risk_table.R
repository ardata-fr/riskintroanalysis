test_that("risk_table creates basic risk table correctly", {
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
          ncol = 2, byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
          ncol = 2, byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0),
          ncol = 2, byrow = TRUE
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
  ) |> extract_dataset()

  # Create risk table with default scale
  rt_default <- risk_table(epi_units)

  # Test basic structure
  expect_s3_class(rt_default, "sf")
  expect_true("eu_id" %in% colnames(rt_default))
  expect_true("eu_name" %in% colnames(rt_default))
  expect_true("geometry" %in% colnames(rt_default))
  expect_equal(nrow(rt_default), 3)

  # Test default attributes
  expect_equal(attr(rt_default, "scale"), c(0, 100))
  expect_equal(attr(rt_default, "table_name"), "risk_table")
  expect_equal(attr(rt_default, "ri_dataset"), TRUE)
  expect_equal(attr(rt_default, "risk_cols"), character(0L))

  # Test custom scale
  rt_custom <- risk_table(epi_units, scale = c(0, 12))
  expect_equal(attr(rt_custom, "scale"), c(0, 12))

  # Test that data is preserved correctly
  expect_equal(rt_default$eu_id, epi_units$eu_id)
  expect_equal(rt_default$eu_name, epi_units$eu_name)
})

test_that("risk_table validates epi_units input correctly", {
  library(sf)

  # Test with missing table_name attribute
  invalid_epi_units <- st_as_sf(
    data.frame(
      eu_id = c("EU1", "EU2"),
      eu_name = c("Unit 1", "Unit 2"),
      geometry = st_sfc(
        st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2, byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
          ncol = 2, byrow = TRUE
        )))
      )
    ),
    crs = 4326
  )

  expect_error(
    risk_table(invalid_epi_units),
    "table_name.*is NULL"
  )

  # Test with wrong table_name attribute
  attr(invalid_epi_units, "table_name") <- "wrong_name"
  expect_error(
    risk_table(invalid_epi_units),
    "table_name.*epi_units"
  )
})

test_that("add_risk adds risk columns correctly", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Create test epi_units
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2", "EU3"),
      EU_NAME = c("Unit 1", "Unit 2", "Unit 3"),
      geometry = st_sfc(
        st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2, byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
          ncol = 2, byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0),
          ncol = 2, byrow = TRUE
        )))
      )
    ),
    crs = 4326
  )

  epi_units <- validate_dataset(
    x = epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |> extract_dataset()

  # Create risk table
  rt <- risk_table(epi_units, scale = c(0, 100))

  # Create test risk data
  risk_data <- st_as_sf(
    data.frame(
      eu_id = c("EU1", "EU2", "EU3"),
      test_risk = c(25, 50, 75),
      geometry = st_sfc(
        st_point(c(0, 0)),
        st_point(c(1, 1)),
        st_point(c(2, 2))
      )
    ),
    crs = 4326
  )

  # Add required attributes to risk_data
  attr(risk_data, "risk_col") <- "test_risk"
  attr(risk_data, "scale") <- c(0, 100)
  attr(risk_data, "table_name") <- "test_risk"

  # Add risk to risk table
  rt_with_risk <- add_risk(rt, risk_data)

  # Test that risk column was added
  expect_true("test_risk" %in% colnames(rt_with_risk))
  expect_equal(rt_with_risk$test_risk, c(25, 50, 75))

  # Test that attributes are updated
  expect_equal(attr(rt_with_risk, "risk_cols"), "test_risk")
  expect_equal(attr(rt_with_risk, "scale"), c(0, 100))

  # Test that original columns are preserved
  expect_equal(rt_with_risk$eu_id, rt$eu_id)
  expect_equal(rt_with_risk$eu_name, rt$eu_name)
})

test_that("add_risk validates inputs correctly", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Create valid epi_units and risk_table
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2"),
      EU_NAME = c("Unit 1", "Unit 2"),
      geometry = st_sfc(
        st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2, byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
          ncol = 2, byrow = TRUE
        )))
      )
    ),
    crs = 4326
  )

  epi_units <- validate_dataset(
    x = epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |> extract_dataset()

  rt <- risk_table(epi_units, scale = c(0, 100))

  # Create valid risk data
  risk_data <- data.frame(
    eu_id = c("EU1", "EU2"),
    test_risk = c(30, 70)
  )
  attr(risk_data, "risk_col") <- "test_risk"
  attr(risk_data, "scale") <- c(0, 100)

  # Test with invalid risk_table
  invalid_rt <- epi_units
  attr(invalid_rt, "table_name") <- "not_risk_table"

  expect_error(
    add_risk(invalid_rt, risk_data),
    "should be the output of.*risk_table"
  )

  # Test with NULL cols when risk_data has no risk_col attribute
  risk_data_no_attr <- risk_data
  attr(risk_data_no_attr, "risk_col") <- NULL

  expect_error(
    add_risk(rt, risk_data_no_attr, cols = NULL),
    "cols.*is NULL"
  )

  # Test with scale exceeding risk_table max
  risk_data_exceeds_max <- risk_data
  attr(risk_data_exceeds_max, "scale") <- c(0, 101)

  expect_error(
    add_risk(rt, risk_data_exceeds_max),
    "Risk scale must be within the risk_table scale range"
  )

  # Test with scale below risk_table min
  risk_data_below_min <- risk_data
  attr(risk_data_below_min, "scale") <- c(-5, 50)

  expect_error(
    add_risk(rt, risk_data_below_min),
    "Risk scale must be within the risk_table scale range"
  )

  # Test with missing join column
  risk_data_no_join <- data.frame(
    wrong_id = c("EU1", "EU2"),
    test_risk = c(30, 70)
  )
  attr(risk_data_no_join, "risk_col") <- "test_risk"
  attr(risk_data_no_join, "scale") <- c(0, 100)

  expect_error(
    add_risk(rt, risk_data_no_join, join_by = "eu_id"),
    "join_by.*must be in.*risk_data"
  )
})

test_that("add_risk handles existing columns correctly", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Create test setup
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2"),
      EU_NAME = c("Unit 1", "Unit 2"),
      geometry = st_sfc(
        st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2, byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
          ncol = 2, byrow = TRUE
        )))
      )
    ),
    crs = 4326
  )

  epi_units <- validate_dataset(
    x = epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |> extract_dataset()

  rt <- risk_table(epi_units, scale = c(0, 100))

  # Add first risk
  risk_data1 <- data.frame(
    eu_id = c("EU1", "EU2"),
    border_risk = c(20, 40)
  )
  attr(risk_data1, "risk_col") <- "border_risk"
  attr(risk_data1, "scale") <- c(0, 100)

  rt <- add_risk(rt, risk_data1)
  expect_true("border_risk" %in% colnames(rt))
  expect_equal(rt$border_risk, c(20, 40))

  # Add second risk with same name (should replace)
  risk_data2 <- data.frame(
    eu_id = c("EU1", "EU2"),
    border_risk = c(60, 80)
  )
  attr(risk_data2, "risk_col") <- "border_risk"
  attr(risk_data2, "scale") <- c(0, 100)

  rt_updated <- add_risk(rt, risk_data2)
  expect_equal(rt_updated$border_risk, c(60, 80))
  expect_equal(length(attr(rt_updated, "risk_cols")), 1)
})

test_that("add_risk accepts risks with scales within risk_table range", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Create test setup
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2", "EU3"),
      EU_NAME = c("Unit 1", "Unit 2", "Unit 3"),
      geometry = st_sfc(
        st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2, byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
          ncol = 2, byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0),
          ncol = 2, byrow = TRUE
        )))
      )
    ),
    crs = 4326
  )

  epi_units <- validate_dataset(
    x = epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |> extract_dataset()

  rt <- risk_table(epi_units, scale = c(0, 100))

  # Test 1: Risk with scale (0, 50) should be accepted
  risk_data_50 <- data.frame(
    eu_id = c("EU1", "EU2", "EU3"),
    risk_50 = c(10, 25, 40)
  )
  attr(risk_data_50, "risk_col") <- "risk_50"
  attr(risk_data_50, "scale") <- c(0, 50)

  rt <- add_risk(rt, risk_data_50)
  expect_true("risk_50" %in% colnames(rt))
  expect_equal(rt$risk_50, c(10, 25, 40))

  # Test 2: Risk with scale (20, 80) should be accepted
  risk_data_subset <- data.frame(
    eu_id = c("EU1", "EU2", "EU3"),
    risk_subset = c(30, 50, 70)
  )
  attr(risk_data_subset, "risk_col") <- "risk_subset"
  attr(risk_data_subset, "scale") <- c(20, 80)

  rt <- add_risk(rt, risk_data_subset)
  expect_true("risk_subset" %in% colnames(rt))
  expect_equal(rt$risk_subset, c(30, 50, 70))

  # Test 3: Risk with scale (0, 12) should be accepted
  risk_data_12 <- data.frame(
    eu_id = c("EU1", "EU2", "EU3"),
    risk_12 = c(4, 8, 12)
  )
  attr(risk_data_12, "risk_col") <- "risk_12"
  attr(risk_data_12, "scale") <- c(0, 12)

  rt <- add_risk(rt, risk_data_12)
  expect_true("risk_12" %in% colnames(rt))
  expect_equal(rt$risk_12, c(4, 8, 12))

  # Test 4: All risks should inherit risk_table's scale attribute
  expect_equal(attr(rt, "scale"), c(0, 100))
  expect_equal(attr(rt, "risk_cols"), c("risk_50", "risk_subset", "risk_12"))
})

test_that("remove_risk removes columns correctly", {
  library(sf)
  library(dplyr)
  library(riskintrodata)

  # Create test setup with multiple risks
  epi_units_raw <- st_as_sf(
    data.frame(
      EU_ID = c("EU1", "EU2"),
      EU_NAME = c("Unit 1", "Unit 2"),
      geometry = st_sfc(
        st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2, byrow = TRUE
        ))),
        st_polygon(list(matrix(
          c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
          ncol = 2, byrow = TRUE
        )))
      )
    ),
    crs = 4326
  )

  epi_units <- validate_dataset(
    x = epi_units_raw,
    table_name = "epi_units",
    eu_id = "EU_ID",
    eu_name = "EU_NAME",
    geometry = "geometry"
  ) |> extract_dataset()

  rt <- risk_table(epi_units, scale = c(0, 100))

  # Add multiple risks
  risk_data1 <- data.frame(
    eu_id = c("EU1", "EU2"),
    border_risk = c(20, 40),
    entry_risk = c(30, 50)
  )
  attr(risk_data1, "risk_col") <- c("border_risk", "entry_risk")
  attr(risk_data1, "scale") <- c(0, 100)

  rt <- add_risk(rt, risk_data1, cols = c("border_risk", "entry_risk"))

  # Remove one risk
  rt_removed <- remove_risk(rt, "border_risk")

  expect_false("border_risk" %in% colnames(rt_removed))
  expect_true("entry_risk" %in% colnames(rt_removed))
  expect_equal(attr(rt_removed, "risk_cols"), "entry_risk")
})
