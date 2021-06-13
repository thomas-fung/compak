#' Calculates the optimal bandwidth via cross-validation
#'
#' @param a.sample numeric vector; the data sample from which the estimate is to be computed.
#' @param interval  numeric vector; the end-points of the interval to be searched for the best bandwidth.
#' @param workers numeric; a positive integer to represent the number of cores used for parallel processing to evaluate the kde
#'
#' @return The optimal bandwidth parameter within the \code{interval}.
#' @export
#'
#' @examples
#' data(days)
#' h.CV <- compak_CVbandwidth(days)
compak_CVbandwidth <- function(a.sample, interval = c(0.025, 1), workers = 1L) {
  # performs leave-one-out cross-validation for bandwidth selection
  # inputs a.sample of data
  # interval specifies range of bandwidth to optimize over

  if (!is.numeric(workers) || floor(workers) != workers || workers < 0) {
    warning("workers must be a positive integer. Resetting it to default = 1.")
    workers <- 1
  } else {
    if (workers > parallelly::availableCores()){
      warning("The number of requested workers is greater than what's available on your system. Reset workers to what's available.")
      workers <- parallelly::availableCores()
    }
  }

  likelihood.cv <- function(h, a.sample) {
    n <- length(a.sample)
    f.cv <- matrix(0, nrow = n, ncol = 1)
    nu <- 1 / h
    for (i in 1:n) {
      f.cv[i] <- compak_evalpmf(a.sample[-i],
        x = a.sample[i], nu = nu,
        workers = 1
      )$f.cmp
    }
    return(-sum(log(f.cv)))
  }
  if (workers > 1) {
    future::plan(future::multisession, workers = workers)
  }
  h_cv <- stats::optimize(f = likelihood.cv, interval = interval, a.sample = a.sample)$minimum
  if (workers > 1) {
    future::plan(future::sequential)
  }

  return(h_cv)
}
