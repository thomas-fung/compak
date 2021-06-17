#' Print Values of COM-Poisson Model
#'
#' \code{print} method for class \code{compak}.
#'
#' @param x an object class 'compak', obtained from a call to \code{compak_fitpmf}.
#' @param digits numeric; minimum number of significant digits to be used for most numbers.
#' @export
#'
#' @details
#' \code{print.compak} can be used to print a short summary of object class 'compak'.
#'
#' @examples
#' ## For examples see example(compak_fitpmf)
#'
print.compak <- function(x,
                         digits =
                           max(3, getOption("digits") - 3)){
  cat("\nCall: ",
      paste(deparse(x$call),
            sep = "\n",
            collapse = "\n"), "\n", sep = "")
  cat("\nData: ", paste(x$data_name), sep = "")
  cat("\nBandwidth: ",
      paste(deparse(round(x$h, digits))), sep = "")
  cat("\nBandwidth selection: ",
      paste(ifelse(x$bandwidth_optim == "CV", "cross-validation", "Kullback-Leibler divergence")), sep = "")
  cat("\nKernel: Conway-Maxwell-Poisson", sep = "")
  cat("\nRange: [",
      paste(min(x$x), ", ", max(x$x), "]", sep = "",
            collapse = "\n"), "\n", sep = "")
  cat("\nParameter estimates", "\n",
      paste(c("nu: ", round(x$nu, digits)),
            sep = ""), "\n", sep = "")
}


