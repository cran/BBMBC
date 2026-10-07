test_that("calc_t1_bb executes and returns expected structure", {
  # Use n_sims = 2 and n_cores = 1 so CRAN checks run fast and don't spawn excess threads
  res <- calc_t1_bb(n_0 = 5, n_1 = 5, n_sims = 2, n_cores = 1)

  expect_s3_class(res, "t1_error_result")
  expect_true("type_1_error_rate" %in% names(res))
  expect_true("parameters" %in% names(res))

  # Check that exactly 2 simulations were attempted
  expect_equal(res$convergences + res$failed_fits, 2, ignore_attr = TRUE)
})

test_that("calc_t1_bb handles unequal dispersion specifications natively", {
  res_unequal <- calc_t1_bb(rho_0 = 0.005, rho_1 = 0.010, n_sims = 2, n_cores = 1)
  expect_equal(res_unequal$parameters$model_dispformula, "~ group")
})
