#' Retrieve logZ and logLambda values from the precomputed data frame
#'
#' @param mu numeric: a vector of integer.
#' @param nu numeric: a vector of integer.
#' @param logZ numeric: a matrix of precomputed values for logZ.
#' @param logLambda numeric: a matrix of precomputed values for logLambda.
#'
#' @return A list containing the following components:
#'
#' \item{logLambda}{a numeric vector of log-lambda values based on mu and nu.}
#' \item{logZ}{a numeric vector of log-Z values based on mu and nu.}
#'
#' @keywords internal
compak_lambdaZ <- function(mu, nu, logZ, logLambda){
  # mu must be  here, but
  # mu can be a vector of means (observations)
  # with common nu (although also works with )
  mu.index <- mu
  nu.index <- nu*10 + 1;
  nu.index.lower <- floor(nu.index)
  nu.index.upper <- ceiling(nu.index)
  weight <- nu.index%%1
  logLambda.lower <- logLambda[mu.index, nu.index.lower]
  logLambda.upper <-logLambda[mu.index, nu.index.upper]
  logLambda.out <- (1-weight)*logLambda.lower + weight*logLambda.upper
  logZ.lower <- logZ[mu.index, nu.index.lower]
  logZ.upper <- logZ[mu.index, nu.index.upper]
  logZ.out <- (1-weight)*logZ.lower + weight*logZ.upper
  return(list("logLambda"=logLambda.out, "logZ"=logZ.out))
}
