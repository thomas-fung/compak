context("Testing the two graphic functions")

library(compak)
data(days)
fit <- compak_fitpmf(days, bandwidth = "KL")

test_that("Testing pc_data", {
  disp_hist_base <- function() plot.compak(fit)
  vdiffr::expect_doppelganger("disp-histogram-base", disp_hist_base)

  disp_hist_ggplot <- gg_plot(fit)
  vdiffr::expect_doppelganger("ggplot2 histogram", disp_hist_ggplot)
})

