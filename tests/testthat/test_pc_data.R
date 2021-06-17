# Testing the precompuate dataset

test_that("Testing pc_data", {
  expect_equal(names(pc_data), c("logZ", "logLambda"))
  expect_equal(dim(pc_data$logZ), c(200, 501))
  expect_equal(dim(pc_data$logLambda), c(200, 501))
  expect_equal(round(pc_data$logZ[1, 1], 6), 0.693147)
  expect_equal(round(pc_data$logLambda[1, 1], 6), -0.693147)
})
