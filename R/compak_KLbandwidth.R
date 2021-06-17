#' Calculates the optimal bandwidth via minimising Kullback-Leibler divergence
#'
#' @param a.sample numeric vector; the data sample from which the estimate is to be computed.
#' @param x a numeric vector: the points of the grid at which the density is to be estimated. This vector should be considerable longer than the range of the a.sample. If NULL, the function will populate the vector for you using a Chebyshev's Inequality type argument.
#' @param interval  numeric vector; the end-points of the interval to be searched for the best bandwidth.
#' @param workers numeric; a positive integer to represent the number of cores used for parallel processing to evaluate the compak smoother.
#'
#' @return The optimal bandwidth parameter within the \code{interval}
#'
#' @export
#'
#' @examples
#' data(days)
#' h.KL <- compak_KLbandwidth(days)
compak_KLbandwidth <- function(a.sample, x = NULL, interval = c(0.025, 1), workers = 1L) {
  if (sum(is.na(a.sample))!=0){
    stop('a.sample contains missing values')
  }
  # fit the Poisson pmf
  mu <- mean(a.sample)
  sigma <- stats::sd(a.sample)
  if (is.null(x)) {
    x <- seq(
      from = max(0, floor(mu - 20 * sigma)),
      to = ceiling(mu + 20 * sigma)
    )
  }
  f.pois <- stats::dpois(x = x, lambda = mu)
  # fit the Neg-Bin pmf via Method of Moments if counts are "overdispersed"
  size <- mu^2 / (stats::var(a.sample) - mu)
  if (size > 0) f.nb <- stats::dnbinom(x, mu = mu, size = size)


  # We optimize over this function.
  MKL <- function(h, a.sample) {
    # fit the CMP_mu kde with dispersion 1/h
    fhat <- compak_evalpmf(
      a.sample = a.sample, x = x,
      nu = 1 / h,
      workers = 1
    )$f.cmp

    # KL1 = KLD(fhat, f.pois)$sum.KLD.px.py
    KL1 <- compak_KL(fhat, f.pois)

    # fit the Neg-Bin pmf via Method of Moments
    if (size <= 0) {
      KL2 <- KL1
    } # if sample is "underdispersed" use Poisson
    else {
      KL2 <- compak_KL(fhat, f.nb)
    }

    # return the Kullback-Leiber divergence between the kde and the Poisson pmf
    return(max(KL1, KL2))
  }

  if (workers > 1) {
    future::plan(future::multisession, workers = workers)
  }
  h_kl <- stats::optimize(f = MKL, interval = interval, a.sample = a.sample)$minimum
  if (workers > 1) {
    future::plan(future::sequential)
  }
  return(h_kl)
}
