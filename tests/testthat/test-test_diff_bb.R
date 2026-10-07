# Setup dummy data for testing
dummy_data <- data.frame(
  arm = rep(c("Control", "Vaccine"), each = 5),
  total_cells = rep(1000, 10),
  target_cells = c(rep(10, 5), rep(20, 5))
)

test_that("test_diff_bb returns properly structured outputs", {
  res <- test_diff_bb(dummy_data, "target_cells", "total_cells", "arm")

  expect_s3_class(res, "bb_test_result")
  expect_true(is.numeric(res$p_value))
  expect_true(is.numeric(res$chisq_stat))
})

test_that("test_diff_bb catches dataframe and column errors", {
  # Not a dataframe
  expect_error(test_diff_bb(list(a = 1), "target", "total", "arm"), "must be a data frame")

  # Missing columns
  expect_error(test_diff_bb(dummy_data, "bad_col", "total_cells", "arm"), "not found")
})

test_that("test_diff_bb catches biological impossibilities", {
  bad_data <- dummy_data
  bad_data$target_cells[1] <- 5000 # 5000 successes out of 1000 trials

  expect_error(test_diff_bb(bad_data, "target_cells", "total_cells", "arm"), "cannot exceed total counts")
})

test_that("test_diff_bb catches invalid group structures", {
  bad_groups <- dummy_data
  bad_groups$arm <- c(rep("A", 3), rep("B", 3), rep("C", 4)) # 3 levels

  expect_error(test_diff_bb(bad_groups, "target_cells", "total_cells", "arm"), "exactly two distinct levels")
})
