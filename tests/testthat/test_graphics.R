# Testing the two graphic functions

library(ggplot2)
data(days)
fit <- compak_fitpmf(days, bandwidth = "KL")

test_that("Testing pc_data", {
  disp_hist_base <- function() plot.compak(fit)
  vdiffr::expect_doppelganger("disp-histogram-base", disp_hist_base)

  disp_hist_ggplot <- autoplot.compak(fit) +
    xlim(min(days) - 2, max(days) + 2)
  vdiffr::expect_doppelganger("ggplot2-histogram", disp_hist_ggplot)
})
