#' Plot for an \code{compak} object in ggplot
#'
#' This function provides a histogram corresponding to the observations and then overlaid with the
#' pmf of a compak smoother using ggplot.
#' @param x an object class 'compak' object, obtain from a call to \code{compak_fitpmf()}.
#' @param ... other argument passed to or from other methods (currently unused).
#'
#' @return An ggplot object of the plot.
#' @export
#' @import ggplot2
#' @examples
#' data(days)
#' fit <- compak_fitpmf(days, bandwidth = "CV")
#' autoplot(fit)
autoplot.compak <- function(x, ...) {
  p <- ggplot(data.frame(z = x$data)) +
    geom_bar(aes(x = z, y = ..count.. / sum(..count..))) +
    xlim(min(x$data - 2), max(x$data + 2)) +
    geom_pointrange(aes(
      x = x,
      y = y,
      ymin = 0,
      ymax = y,
      colour = "compak fit"
    ),
    colour = "red",
    data = data.frame(x = x$x, y = x$f.cmp)
    ) +
    labs(
      title = "Compak smoother fit", x = "Data",
      subtitle = paste0("bandwidth = ", round(x$h, 4)),
      y = "Probability"
    )
  return(p)
}

#' Plot for an \code{compak} object
#'
#' This function provides a histogram corresponding to the observations and then overlaid with the
#' pmf of a compak smoother.
#' @param x an object class 'compak' object, obtain from a call to \code{compak_fitpmf()}.
#' @param ... other argument passed to or from other methods (currently unused).
#'
#' @return A histogram with the data and the \code{compak} smoother fit.
#' @export
#' @import graphics
#' @examples
#' data(days)
#' fit <- compak_fitpmf(days, bandwidth = "CV")
#' plot(fit)
plot.compak <- function(x, ...) {
  range <- min(x$data - 2):max(x$data + 2)
  hist(x$data,
    breaks = range - 0.5, col = "grey", freq = F,
    main = paste0("Compak smoother fit, bandwidth = ", round(x$h, 4)),
    xlab = "Data"
  )
  points(x$x, x$f.cmp, col = "red", pch = 16, cex = 1)
  lines(x$x, x$f.cmp, col = "red", lwd = 3, type = "h")
}
