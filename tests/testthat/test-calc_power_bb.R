test_that("calc_power_bb executes and returns expected structure", {
  res <- calc_power_bb(n_0 = 5, n_1 = 5, n_sims = 2, n_cores = 1)

  expect_s3_class(res, "power_result")
  expect_true("power" %in% names(res))
  expect_true(res$power >= 0 && res$power <= 1)
})

test_that("calc_power_bb catches invalid fold changes that exceed probability bounds", {
  # If p_base = 0.6 and fold_change = 2, p_treat = 1.2 (impossible)
  expect_error(
    calc_power_bb(p_base = 0.6, fold_change = 2.0, n_sims = 2, n_cores = 1),
    "must be less than 1"
  )
})
