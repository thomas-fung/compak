#' Retrieve logZ and logLambda values from the precomputed data frame
#'
#' @description \code{logZ} and \code{logLambda} values are precomputed and stored in the \code{pc_data} data frame. This function would retrieve it based on the \code{mu} and \code{nu}.
#'
#' The values are computed based on integer-value of \code{mu}>=1 and at a 0.1 grid of \code{nu}.
#'
#' @param mu numeric: a vector of integers that are greater than 1, representing the means
#' @param nu numeric: a vector of positive numbers, representing the dispersion
#'
#' @return A list containing the following components:
#'
#' \item{logLambda}{a numeric vector of log-lambda values based on mu and nu.}
#' \item{logZ}{a numeric vector of log-Z values based on mu and nu.}
#'
#' @keywords internal
compak_lambdaZ <- function(mu, nu) {
  mu.index <- floor(mu)
  nu.index <- nu * 10 + 1
  nu.index.lower <- floor(nu.index)
  nu.index.upper <- ceiling(nu.index)
  weight <- nu.index %% 1
  logLambda.lower <- pc_data$logLambda[mu.index, nu.index.lower]
  logLambda.upper <- pc_data$logLambda[mu.index, nu.index.upper]
  logLambda.out <- (1 - weight) * logLambda.lower + weight * logLambda.upper
  logZ.lower <- pc_data$logZ[mu.index, nu.index.lower]
  logZ.upper <- pc_data$logZ[mu.index, nu.index.upper]
  logZ.out <- (1 - weight) * logZ.lower + weight * logZ.upper
  return(list("logLambda" = logLambda.out, "logZ" = logZ.out))
}
