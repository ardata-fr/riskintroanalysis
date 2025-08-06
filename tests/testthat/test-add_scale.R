test_that("add_scale adds scale attribute to dataset", {
  # Create simple test data
  test_data <- data.frame(
    region = c("A", "B", "C"),
    risk_score = c(2.5, 7.8, 4.1)
  )

  # Test adding scale attribute
  result <- add_scale(test_data, c(0, 12))

  # Check that scale attribute was added correctly
  expect_equal(attr(result, "scale"), c(0, 12))

  # Check that original data is unchanged
  expect_equal(result$region, test_data$region)
  expect_equal(result$risk_score, test_data$risk_score)

  # Test with different scale values
  result2 <- add_scale(test_data, c(0, 100))
  expect_equal(attr(result2, "scale"), c(0, 100))
})

test_that("add_scale validates inputs correctly", {
  test_data <- data.frame(x = 1:3, y = 4:6)

  # Should work with valid inputs
  expect_no_error(add_scale(test_data, c(0, 12)))

  # Should error with non-data.frame input
  expect_error(
    add_scale("not a dataframe", c(0, 12)),
    "dataset.*must be.*data.frame"
  )

  # Should error with non-numeric scale
  expect_error(
    add_scale(test_data, c("0", "12")),
    "scale.*must be numeric"
  )

  # Should error with wrong length scale
  expect_error(
    add_scale(test_data, c(0)),
    "scale.*must be length 2"
  )

  expect_error(
    add_scale(test_data, c(0, 5, 10)),
    "scale.*must be length 2"
  )
})

test_that("add_scale works with tibbles and sf objects", {
  library(dplyr)

  # Test with tibble
  test_tibble <- tibble(
    id = 1:3,
    value = c(1.1, 2.2, 3.3)
  )

  result_tibble <- add_scale(test_tibble, c(0, 10))
  expect_equal(attr(result_tibble, "scale"), c(0, 10))
  expect_s3_class(result_tibble, "tbl_df")

  # Test with sf object if sf is available
  if (requireNamespace("sf", quietly = TRUE)) {
    library(sf)

    test_sf <- st_as_sf(
      data.frame(
        id = 1:2,
        risk = c(3.5, 8.1),
        geometry = st_sfc(
          st_point(c(0, 0)),
          st_point(c(1, 1))
        )
      ),
      crs = 4326
    )

    result_sf <- add_scale(test_sf, c(0, 15))
    expect_equal(attr(result_sf, "scale"), c(0, 15))
    expect_s3_class(result_sf, "sf")
  }
})

test_that("add_scale preserves existing attributes", {
  test_data <- data.frame(
    region = c("A", "B"),
    score = c(5, 10)
  )

  # Add some existing attributes
  attr(test_data, "existing_attr") <- "test_value"
  attr(test_data, "another_attr") <- 42

  # Add scale
  result <- add_scale(test_data, c(0, 20))

  # Check that scale was added
  expect_equal(attr(result, "scale"), c(0, 20))

  # Check that existing attributes are preserved
  expect_equal(attr(result, "existing_attr"), "test_value")
  expect_equal(attr(result, "another_attr"), 42)
})
