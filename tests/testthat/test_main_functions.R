data(days)
range.days <- 20:40
h.KL <- compak_KLbandwidth(days)
h.KL2 <- compak_KLbandwidth(days, interval = c(0.5, 0.6))
h.CV <- compak_CVbandwidth(days)
f.cmp_CV <- compak_evalpmf(
  a.sample = days,
  x = range.days,
  nu = 1 / h.CV,
  workers = 1
)
f.cmp_CV_parallel <- compak_evalpmf(
  a.sample = days,
  x = range.days,
  nu = 1 / h.CV,
  workers = 2
)
f.cmp_KL <- compak_evalpmf(
  a.sample = days,
  x = range.days,
  nu = 1 / h.KL,
  workers = 1
)
f.cmp_KL_parallel <- compak_evalpmf(
  a.sample = days,
  x = range.days,
  nu = 1 / h.KL,
  workers = 2
)
fit.compak_CV <- compak_fitpmf(days, range.days, bandwidth_optim = "CV")
fit.compak_KL <- compak_fitpmf(days, range.days, bandwidth_optim = "KL")

test_that("Testing CV bandwidth selection", {
  expect_equal(round(h.CV, 6), 0.025064)
})

test_that("Testing KL bandwidth selection", {
  expect_equal(round(h.KL, 6), 0.742109)
  expect_equal(round(h.KL2, 6), 0.599927)
})

test_that("Testing manually fitting cmp pmf under KL bandwidth", {
  expect_equal(length(f.cmp_KL$f.cmp), length(range.days))
  expect_equal(f.cmp_KL$f.cmp, compak_evalpmf(days, range.days, h = h.KL)$f.cmp)
  expect_equal(round(f.cmp_KL$f.cmp[1], 6), 0.018822)
  expect_equal(sum(f.cmp_KL$f.cmp > 0 & f.cmp_KL$f.cmp < 1), length(range.days))
  expect_equal(round(sum(f.cmp_KL$f.cmp), 6), 0.946863)
})

test_that("Testing manually fitting cmp pmf under CV bandwidth", {
  expect_equal(length(f.cmp_CV$f.cmp), length(range.days))
  expect_equal(round(f.cmp_CV$f.cmp[5], 6), 0.024935)
  expect_equal(sum(f.cmp_CV$f.cmp > 0 & f.cmp_CV$f.cmp < 1), length(range.days))
  expect_equal(round(sum(f.cmp_CV$f.cmp), 6), 1)
})

test_that("Testing the wrapper compak_fitpmf function", {
  expect_equal(class(fit.compak_CV), "compak")
  expect_equal(fit.compak_CV$h, h.CV)
  expect_equal(fit.compak_KL$h, h.KL)
  expect_equal(1 / fit.compak_KL$h, fit.compak_KL$nu)
  expect_equal(fit.compak_CV$f.cmp, f.cmp_CV$f.cmp)
  expect_equal(fit.compak_KL$f.cmp, f.cmp_KL$f.cmp)
  expect_equal(fit.compak_CV$x, range.days)
  expect_true(is.list(fit.compak_CV$kernel.est))
  expect_equal(length(fit.compak_CV$kernel.est[[1]]), length(range.days))
  expect_equal(length(fit.compak_CV$kernel.est), length(unique(days)))
})

test_that("Testing the parallel computing setting", {
  expect_equal(f.cmp_CV, f.cmp_CV_parallel)
  expect_equal(f.cmp_KL, f.cmp_KL_parallel)
})


test_that("Testing various error/stop/warning", {
  expect_error(compak_CVbandwidth(c(days, NA)))
  expect_warning(compak_CVbandwidth(days, workers = 0.5))
  expect_error(compak_evalpmf(c(days, NA), h = 1))
  expect_warning(compak_evalpmf(days, h = 1, workers = 0.5))
  expect_error(compak_evalpmf(days))
  expect_error(compak_evalpmf(days, h = 1, nu = 2))
  expect_error(compak_fitpmf(days, h = 1, nu =2))
  expect_error(compak_fitpmf(c(days, NA)))
  expect_warning(compak_fitpmf(days, workers = 0.5))
  expect_error(compak_fitpmf(days,
                             bandwidth_optim = "testing"))
  expect_error(compak_KLbandwidth(c(days, NA)))
})

test_that("Testing the print function", {
  expect_snapshot(print.compak(fit.compak_KL))
})

