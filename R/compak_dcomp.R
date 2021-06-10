#' Density of Conway-Maxwell-Poisson distribution with parameter \code{mu} and \code{nu} with a
#' \code{compak} setting

#' @param x vector of quantiles
#' @param mu,nu numeric scalars: mean and dispersion parameters.
#'
#' @return The density of the cmp distribution.
#'
#' @keywords internal
compak_dcomp <- function(x, mu, nu) {
  if (mu == 0) {
    return(x == 0) * 1
  }
  if (mu > 200 | nu > 40) {
    return(mpcmp::dcomp(x, mu, nu))
  }
  find_lambda_Z <- compak_lambdaZ(mu, nu)
  logLambda <- find_lambda_Z$logLambda
  logZ <- find_lambda_Z$logZ
  log.dcomp <- x * logLambda - logZ - nu * lgamma(x + 1)
  return(exp(log.dcomp))
}
