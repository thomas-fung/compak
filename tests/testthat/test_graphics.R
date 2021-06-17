# Testing the two graphic functions

data(days)
fit <- compak_fitpmf(days, bandwidth = "KL")
expect_doppelganger <- function(title, fig,
                                writer = vdiffr::write_svg,
                                cran = FALSE)
{
  fig_name <- vdiffr:::str_standardise(title)
  testcase <- vdiffr:::make_testcase_file(fig_name)
  writer(fig, testcase, title)
  file <- paste0(fig_name, ".svg")
  withCallingHandlers(testthat::expect_snapshot_file(testcase,
                                                     name = file, cran = cran), expectation_failure = function(cnd) {
                                                       if (is_snapshot_stale(title, testcase)) {
                                                         testthat::skip(paste_line("SVG snapshot generated under a different vdiffr version.",
                                                                                   i = "Please update your snapshots."))
                                                       }
                                                       if (!is.null(snapshotter <- get_snapshotter())) {
                                                         path_old <- vdiffr:::snapshot_path(snapshotter, file)
                                                         path_new <- vdiffr:::snapshot_path(snapshotter, paste0(fig_name,
                                                                                                       ".new.svg"))
                                                         if (all(file.exists(path_old, path_new))) {
                                                           vdiffr:::push_log(fig_name, path_old, path_new)
                                                         }
                                                       }
                                                     })
}


test_that("Testing pc_data", {
  disp_hist_base <- function() plot.compak(fit)
  expect_doppelganger("disp-histogram-base", disp_hist_base)

  disp_hist_ggplot <- autoplot.compak(fit) +
    xlim(min(days) - 2, max(days) + 2)
  expect_doppelganger("ggplot2-histogram", disp_hist_ggplot)
})
