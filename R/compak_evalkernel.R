#' Calculates the kernel at a key point
#'
#' This function evaluates the kernel, which is centred at \code{key}, over a grid of points using the counts provided in a list.
#'
#' @param key character: where the kernel should be centred.
#' @param counts list of distinct count values; names are the observed values; elements are the corresponding frequencies
#' @param x numeric vector :the points of the grid at which the density is to be estimated.
#' @param nu numeric: CMP_mu kernel has dispersion \code{nu}
#'
#' @return The kernel values.
#' @keywords internal
compak_evalkernel <- function(key, counts, x, nu) {
  obvs <- counts[[key]]
  X_i <- strtoi(key)
  probs <- obvs * compak_dcomp(x = x, mu = X_i, nu = nu)
  return(probs)
}
