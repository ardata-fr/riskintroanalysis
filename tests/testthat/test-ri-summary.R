test_that("summarise_scores() works with basic functionality", {
  # Create test data
  risk_table <- data.frame(
    eu_id = c("EU1", "EU2", "EU3"),
    ri_border = c(10, 20, 30),
    ri_entry = c(15, 25, 35),
    ri_mobility = c(12, 22, 32)
  )
  attr(risk_table, "risk_cols") <- c("ri_border", "ri_entry", "ri_mobility")
  attr(risk_table, "scale") <- c(0, 100)

  # Test mean method
  result_mean <- summarise_scores(risk_table, method = "mean")
  expect_equal(result_mean$overall_risk, c(12.33, 22.33, 32.33), tolerance = 0.01)
  expect_equal(attr(result_mean, "risk_col"), "overall_risk")
  expect_equal(attr(result_mean, "scale"), c(0, 100))
  expect_false(attr(result_mean, "has_overwrite"))

  # Test max method
  result_max <- summarise_scores(risk_table, method = "max")
  expect_equal(result_max$overall_risk, c(15, 25, 35))

  # Test min method
  result_min <- summarise_scores(risk_table, method = "min")
  expect_equal(result_min$overall_risk, c(10, 20, 30))

  # Test median method
  result_median <- summarise_scores(risk_table, method = "median")
  expect_equal(result_median$overall_risk, c(12, 22, 32))
})

test_that("summarise_scores() handles custom column selection", {
  risk_table <- data.frame(
    eu_id = c("EU1", "EU2"),
    ri_border = c(10, 20),
    ri_entry = c(15, 25),
    ri_mobility = c(12, 22)
  )

  # Test with specific columns
  result <- summarise_scores(
    risk_table,
    cols = c("ri_border", "ri_entry"),
    method = "mean"
  )
  expect_equal(result$overall_risk, c(12.5, 22.5))

  # Test with custom name_to
  result_custom <- summarise_scores(
    risk_table,
    cols = c("ri_border", "ri_entry"),
    method = "max",
    name_to = "custom_risk"
  )
  expect_equal(result_custom$custom_risk, c(15, 25))
  expect_equal(attr(result_custom, "risk_col"), "custom_risk")
})

test_that("summarise_scores() handles keep_cols parameter", {
  risk_table <- data.frame(
    eu_id = c("EU1", "EU2"),
    ri_border = c(10, 20),
    ri_entry = c(15, 25)
  )
  attr(risk_table, "risk_cols") <- c("ri_border", "ri_entry")

  # Test keep_cols = FALSE (default)
  result_drop <- summarise_scores(risk_table, method = "mean")
  expect_false("ri_border" %in% names(result_drop))
  expect_false("ri_entry" %in% names(result_drop))
  expect_null(attr(result_drop, "risk_cols"))

  # Test keep_cols = TRUE
  result_keep <- summarise_scores(risk_table, method = "mean", keep_cols = TRUE)
  expect_true("ri_border" %in% names(result_keep))
  expect_true("ri_entry" %in% names(result_keep))
  expect_equal(attr(result_keep, "risk_cols"), c("ri_border", "ri_entry"))
})

test_that("summarise_scores() handles NA values correctly", {
  risk_table <- data.frame(
    eu_id = c("EU1", "EU2", "EU3"),
    ri_border = c(10, NA, 30),
    ri_entry = c(15, 25, NA),
    ri_mobility = c(NA, 22, 32)
  )
  attr(risk_table, "risk_cols") <- c("ri_border", "ri_entry", "ri_mobility")

  result <- summarise_scores(risk_table, method = "mean")

  # EU1: mean of 10, 15 = 12.5
  # EU2: mean of 25, 22 = 23.5
  # EU3: mean of 30, 32 = 31
  expect_equal(result$overall_risk, c(12.5, 23.5, 31))
})

test_that("summarise_scores() handles all NA values", {
  risk_table <- data.frame(
    eu_id = c("EU1", "EU2"),
    ri_border = c(NA, 10),
    ri_entry = c(NA, 20)
  )
  attr(risk_table, "risk_cols") <- c("ri_border", "ri_entry")

  result <- summarise_scores(risk_table, method = "mean")

  # EU1: all NA values should result in NA
  # EU2: mean of 10, 20 = 15
  expect_true(is.na(result$overall_risk[1]))
  expect_equal(result$overall_risk[2], 15)
})

test_that("summarise_scores() works with overwrite_table", {
  risk_table <- data.frame(
    eu_id = c("EU1", "EU2", "EU3"),
    ri_border = c(10, 20, 30),
    ri_entry = c(15, 25, 35)
  )
  attr(risk_table, "risk_cols") <- c("ri_border", "ri_entry")

  overwrite_table <- data.frame(
    eu_id = c("EU1", "EU3"),
    overwrite_risk = c(99, 88),
    overwrite_risk_comm = c("Expert override", "Field assessment")
  )

  result <- summarise_scores(
    risk_table,
    method = "mean",
    overwrite_table = overwrite_table
  )

  # EU1: overwritten to 99
  # EU2: calculated mean (20+25)/2 = 22.5
  # EU3: overwritten to 88
  expect_equal(result$overall_risk, c(99, 22.5, 88))
  expect_true(attr(result, "has_overwrite"))
  expect_true("overwrite_risk" %in% names(result))
  expect_true("overwrite_risk_comm" %in% names(result))
})

test_that("summarise_scores() handles partial overwrite_table", {
  risk_table <- data.frame(
    eu_id = c("EU1", "EU2", "EU3"),
    ri_border = c(10, 20, 30),
    ri_entry = c(15, 25, 35)
  )
  attr(risk_table, "risk_cols") <- c("ri_border", "ri_entry")

  # Overwrite table with NA values (should not override)
  overwrite_table <- data.frame(
    eu_id = c("EU1", "EU2", "EU3"),
    overwrite_risk = c(99, NA, 88),
    overwrite_risk_comm = c("Override 1", "", "Override 3")
  )

  result <- summarise_scores(
    risk_table,
    method = "mean",
    overwrite_table = overwrite_table
  )

  # EU1: overwritten to 99
  # EU2: calculated mean (20+25)/2 = 22.5 (NA override ignored)
  # EU3: overwritten to 88
  expect_equal(result$overall_risk, c(99, 22.5, 88))
})

test_that("summarise_scores() validates overwrite_table structure", {
  risk_table <- data.frame(
    eu_id = c("EU1", "EU2"),
    ri_border = c(10, 20),
    ri_entry = c(15, 25)
  )

  # Missing eu_id column
  bad_overwrite1 <- data.frame(
    bad_id = c("EU1"),
    overwrite_risk = c(99),
    overwrite_risk_comm = c("Comment")
  )
  expect_error(
    summarise_scores(risk_table, method = "mean", overwrite_table = bad_overwrite1),
    "overwrite_table.*should have.*eu_id.*column"
  )

  # Missing overwrite_risk column
  bad_overwrite2 <- data.frame(
    eu_id = c("EU1"),
    bad_risk = c(99),
    overwrite_risk_comm = c("Comment")
  )
  expect_error(
    summarise_scores(risk_table, method = "mean", overwrite_table = bad_overwrite2),
    "overwrite_table.*should have.*overwrite_risk.*column"
  )

  # Missing overwrite_risk_comm column
  bad_overwrite3 <- data.frame(
    eu_id = c("EU1"),
    overwrite_risk = c(99),
    bad_comm = c("Comment")
  )
  expect_error(
    summarise_scores(risk_table, method = "mean", overwrite_table = bad_overwrite3),
    "overwrite_table.*should have.*overwrite_risk_comm.*column"
  )

  # Wrong data types
  bad_overwrite4 <- data.frame(
    eu_id = c(1), # Should be character
    overwrite_risk = c(99),
    overwrite_risk_comm = c("Comment")
  )
  expect_error(
    summarise_scores(risk_table, method = "mean", overwrite_table = bad_overwrite4),
    "eu_id.*column.*should be character"
  )

  bad_overwrite5 <- data.frame(
    eu_id = c("EU1"),
    overwrite_risk = c("99"), # Should be numeric
    overwrite_risk_comm = c("Comment")
  )
  expect_error(
    summarise_scores(risk_table, method = "mean", overwrite_table = bad_overwrite5),
    "overwrite_risk.*should be numeric"
  )

  bad_overwrite6 <- data.frame(
    eu_id = c("EU1"),
    overwrite_risk = c(99),
    overwrite_risk_comm = c(123) # Should be character
  )
  expect_error(
    summarise_scores(risk_table, method = "mean", overwrite_table = bad_overwrite6),
    "overwrite_risk_comm.*should be character"
  )
})

test_that("summarise_scores() preserves attributes correctly", {
  risk_table <- data.frame(
    eu_id = c("EU1", "EU2"),
    ri_border = c(10, 20),
    ri_entry = c(15, 25)
  )
  attr(risk_table, "risk_cols") <- c("ri_border", "ri_entry")
  attr(risk_table, "scale") <- c(0, 50)
  attr(risk_table, "table_name") <- "test_table"

  result <- summarise_scores(risk_table, method = "mean", name_to = "final_risk")

  expect_equal(attr(result, "scale"), c(0, 50))
  expect_equal(attr(result, "risk_col"), "final_risk")
  expect_equal(attr(result, "table_name"), "risk_table")
  expect_false(attr(result, "has_overwrite"))

  # Test with overwrite
  overwrite_table <- data.frame(
    eu_id = c("EU1"),
    overwrite_risk = c(99),
    overwrite_risk_comm = c("Override")
  )

  result_with_override <- summarise_scores(
    risk_table,
    method = "mean",
    overwrite_table = overwrite_table
  )

  expect_true(attr(result_with_override, "has_overwrite"))
})

test_that("summarise_scores() uses automatic risk column detection", {
  risk_table <- data.frame(
    eu_id = c("EU1", "EU2"),
    ri_border = c(10, 20),
    ri_entry = c(15, 25),
    other_col = c(100, 200)
  )
  attr(risk_table, "risk_cols") <- c("ri_border", "ri_entry")

  # Should automatically use risk_cols attribute
  result <- summarise_scores(risk_table, method = "mean")

  # Should only use ri_border and ri_entry, not other_col
  expect_equal(result$overall_risk, c(12.5, 22.5))
})

test_that("summarise_scores() method argument validation", {
  risk_table <- data.frame(
    eu_id = c("EU1"),
    ri_border = c(10),
    ri_entry = c(15)
  )

  expect_error(
    summarise_scores(risk_table, method = "invalid"),
    "should be one of"
  )
})