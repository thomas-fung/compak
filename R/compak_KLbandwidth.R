#' Calculates the optimal bandwidth via minimising Kullback-Leibler divergence
#'
#' @param a.sample numeric vector; the data sample from which the estimate is to be computed.
#' @param x a numeric vector: the points of the grid at which the density is to be estimated.
#' @param interval  numeric vector; the end-points of the interval to be searched for the best bandwidth.
#' @param parallel logical: if \code{FALSE} (default), parallel process will not be used to evaluate the compak smoother.
#'
#' @return The optimal bandwidth parameter within the \code{interval}
#' @export
#'
#' @examples
#' data(days)
#' h.KL <- compak_KLbandwidth(days)
#'
compak_KLbandwidth <- function(a.sample, x = 0:200, interval = c(0.025, 1), parallel = FALSE){
  # fit the Poisson pmf
  mu <- mean(a.sample)
  f.pois <- stats::dpois(x= x, lambda=mu)

  # fit the Neg-Bin pmf via Method of Moments if counts are "overdispersed"
  size <- mu^2/(stats::var(a.sample)-mu)
  if(size > 0)  f.nb <- stats::dnbinom(x, mu=mu, size=size)

  # We optimize over this function.
  MKL <- function(h, a.sample){
    # fit the CMP_mu kde with dispersion 1/h
    if (parallel) {
      fhat <- compak_evalpmf(a.sample, x, nu = 1/h, parallel = TRUE)
    } else {
      fhat <- compak_evalpmf(a.sample, x, nu = 1/h, parallel= FALSE)
    }

    #KL1 = KLD(fhat, f.pois)$sum.KLD.px.py
    KL1 <- compak_KL(fhat, f.pois)

    # fit the Neg-Bin pmf via Method of Moments
    if(size <= 0) KL2 <- KL1 #if sample is "underdispersed" use Poisson
    else {
      KL2 <- compak_KL(fhat, f.nb)
    }

    # return the Kullback-Leiber divergence between the kde and the Poisson pmf
    return(max(KL1, KL2))
  }

  h_kl <- stats::optimize(f=MKL, interval= interval, a.sample=a.sample)$minimum

  return(h_kl)
}
