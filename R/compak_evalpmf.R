#' Fits CMP_mu discrete kernel smoother for a sample of counts
#'
#' This function computes the density using the CMP_mu discrete kernel smooth over a grid of points using the given bandwidth \code{h} or dispersion parameter \code{nu}.
#' @param a.sample numeric vector: the data sample from which estimate is to be computed.
#' @param x Either \code{NULL} or a numeric vector: the points of the grid at which the density is to be estimated. If none is provided, the range of \code{a.sample} will be used.
#' @param h,nu numeric: the bandwidth or smoothing parameter. Only one is needed and they are related by nu = 1/h.
#' @param workers numeric; a positive integer to represent the number of cores used for parallel processing to evaluate the kde
#'
#' @return Return the estimated p.m.f. values.
#'
#' @export
#'
#' @examples
#' data(days)
#' # The bandwidth h can be the one obtained by cross validation.
#' (h.CV <- compak_CVbandwidth(days))
#' compak_evalpmf(days, 20:40, h = h.CV, workers = 1)
compak_evalpmf <- function(a.sample, x = NULL, h = NULL, nu = NULL, workers = 1L) {
  # fits CMP_mu discrete kernel pmf P(X = x) smoother for a.sample of counts
  # evaluated at a (vector of) point(s) x
  if (is.null(h) && is.null(nu)) {
    stop('argument "h" (bandwidth) and "nu" are both missing, with no default. Please specify one of them.')
  } else if (!is.null(h) && !is.null(nu) && h != nu) {
    stop("specify 'h' or 'nu' but not both.")
  } else if (!is.null(nu)) {
    h <- 1 / nu
  } else if (!is.null(h)) {
    nu <- 1 / h
  }
  if (!is.numeric(workers) || floor(workers) != workers || workers < 0) {
    warning("workers must be a positive integer. Resetting it to default = 1.")
    workers <- 1
  }

  if (is.null(x)) {
    x <- min(a.sample):max(a.sample)
  }
  npoints <- length(x)
  fhat <- vector(length = npoints)
  samp.points <- length(a.sample)
  # first get number of observations for each observed count
  counts <- list()
  for (i in a.sample) {
    if (!is.null(counts[[toString(i)]])) {
      counts[[toString(i)]] <- counts[[toString(i)]] + 1
    }
    else {
      counts[[toString(i)]] <- 1
    }
  }

  # evaluate kernels
  keys <- rlist::list.names(counts) # this is the list of distinct count values observed as strings
  # for parallel evaluating the kernels (requires future & furrr library)
  if (workers > 1) {
    future::plan(future::multisession, workers = workers)
    results <- furrr::future_map(keys, ~ compak_evalkernel(.x,
      counts = counts,
      x = x, nu = nu
    ))
    future::plan(future::sequential)
  } else {
    results <- purrr::map(keys, ~ compak_evalkernel(.x,
      counts = counts,
      x = x, nu = nu
    ))
  }

  # sum kernels together
  for (ker in results) {
    fhat <- fhat + ker
  }

  # return the normalised kde at the desired x values
  return(fhat / samp.points)
}
