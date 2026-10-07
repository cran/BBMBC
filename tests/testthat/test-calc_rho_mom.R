test_that("calc_rho_mom calculates correctly for valid inputs", {
  props <- c(0.1, 0.2, 0.3)
  rho <- calc_rho_mom(props)
  expect_type(rho, "double")
  expect_true(!is.na(rho))
})

test_that("calc_rho_mom handles NAs correctly based on na.rm", {
  props_with_na <- c(0.1, 0.2, 0.3, NA)
  expect_type(calc_rho_mom(props_with_na, na.rm = TRUE), "double")
  expect_true(is.na(calc_rho_mom(props_with_na, na.rm = FALSE)))
})

test_that("calc_rho_mom throws expected errors for bad inputs", {
  expect_error(calc_rho_mom(0.5), "At least two observations")
  expect_error(calc_rho_mom(c(-0.1, 0.5)), "between 0 and 1")
  expect_error(calc_rho_mom(c(0.5, 1.2)), "between 0 and 1")
})

test_that("calc_rho_mom warns and returns NA for extreme means", {
  expect_warning(res_zero <- calc_rho_mom(c(0, 0, 0)), "exactly 0 or 1")
  expect_true(is.na(res_zero))

  expect_warning(res_one <- calc_rho_mom(c(1, 1, 1)), "exactly 0 or 1")
  expect_true(is.na(res_one))
})
