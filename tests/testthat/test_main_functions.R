context("Testing the main functions of compak")

library(compak)
data(days)
h.KL <- compak_KLbandwidth(days)
h.KL2 <- compak_KLbandwidth(days, interval = c(0.5, 0.6))
h.CV <- compak_CVbandwidth(days)
range.days <- 20:40
f.cmp_CV <- compak_evalpmf(days, 20:40, nu = 1/h.CV, workers = 1)
f.cmp_KL <- compak_evalpmf(days, 20:40, nu = 1/h.KL, workers = 1)
fit.compak_CV <- compak_fitpmf(days, range.days, bandwidth = "CV")
fit.compak_KL <- compak_fitpmf(days, range.days, bandwidth = "KL")

test_that("Testing CV bandwidth selection", {
  expect_equal(round(h.CV,6), 0.025064)
})

test_that("Testing KL bandwidth selection", {
  expect_equal(round(h.KL,6), 0.742109)
  expect_equal(round(h.KL2, 6), 0.599927)
})

test_that("Testing manually fitting cmp pmf under KL bandwidth", {
  expect_equal(length(f.cmp_KL), length(range.days))
  expect_equal(f.cmp_KL, compak_evalpmf(days, 20:40, h = h.KL))
  expect_equal(round(f.cmp_KL[1], 6), 0.018822)
  expect_equal(sum(f.cmp_KL >0 & f.cmp_KL <1), length(range.days))
  expect_equal(round(sum(f.cmp_KL), 6), 0.946863)
})

test_that("Testing manually fitting cmp pmf under CV bandwidth", {
  expect_equal(length(f.cmp_CV), length(range.days))
  expect_equal(round(f.cmp_CV[5], 6), 0.024935)
  expect_equal(sum(f.cmp_CV >0 & f.cmp_CV <1), length(range.days))
  expect_equal(round(sum(f.cmp_CV), 6), 1)
})

test_that("Testing the wrapper compak_fitpmf function", {
  expect_equal(class(fit.compak_CV), "compak")
  expect_equal(fit.compak_CV$h, h.CV)
  expect_equal(fit.compak_KL$h, h.KL)
  expect_equal(1/fit.compak_KL$h, fit.compak_KL$nu)
  expect_equal(fit.compak_CV$f.cmp, f.cmp_CV)
  expect_equal(fit.compak_KL$f.cmp, f.cmp_KL)
  expect_equal(fit.compak_CV$x, range.days)
})
