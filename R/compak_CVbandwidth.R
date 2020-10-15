#' Calculates the optimal bandwidth via cross-validation
#'
#' @param a.sample numeric vector; the data sample from which the estimate is to be computed.
#' @param interval  numeric vector; the end-points of the interval to be searched for the best bandwidth.
#' @param parallel logical: if \code{FALSE} (default), parallel process will not be used to evaluate the compak smoother.
#'
#' @return The optimal bandwidth parameter within the \code{interval}.
#' @export
#'
#' @examples
#' data(days)
#' h.CV <- compak_CVbandwidth(days)
compak_CVbandwidth <- function(a.sample, interval = c(0.025, 1), parallel = F){
  # performs leave-one-out cross-validation for bandwidth selection
  # inputs a.sample of data
  # interval specifies range of bandwidth to optimize over

  likelihood.cv <- function(h, a.sample){
    n <- length(a.sample)
    f.cv <- matrix(0, nrow = n, ncol = 1)
    nu <- 1/h
    for(i in 1:n){
      f.cv[i] <- compak_evalpmf(a.sample[-i], x = a.sample[i], nu = nu, parallel = parallel)
    }
    return(-sum(log(f.cv)))
  }

  h_cv <- stats::optimize(f=likelihood.cv, interval = interval, a.sample=a.sample)$minimum
  return(h_cv)
}
