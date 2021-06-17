#' Plot for an \code{compak} object in ggplot
#'
#' This function provides a histogram corresponding to the observations and then overlaid with the
#' pmf of a compak smoother using ggplot or graphics.
#' @param x an object class 'compak' object, obtain from a call to \code{compak_fitpmf()}.
#' @param ... other argument passed to or from other methods (currently unused).
#'
#' @return An ggplot object for autoplot.
#' @import ggplot2
#' @importFrom rlang .data
#' @import graphics
#' @examples
#' data(days)
#' fit <- compak_fitpmf(days, bandwidth = "CV")
#' plot(fit) # or autoplot(fit)
#' @name compak_graphics
NULL

#' @rdname compak_graphics
#' @export
autoplot.compak <- function(x, ...) {
  dat <- data.frame(table(x$data)/sum(table(x$data)))
  dat$Var1 <- as.numeric(levels(dat$Var1))
  p <- ggplot(dat) +
    geom_col(aes(x = .data$Var1, y = .data$Freq)) +
    geom_pointrange(aes(
      x = .data$x,
      y = .data$y,
      ymin = 0,
      ymax = .data$y,
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


#' @rdname compak_graphics
#' @export
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
