#' Calculates the optimal bandwidth via minimising Kullback-Leibler divergence
#'
#' @param a.sample numeric vector; the data sample from which the estimate is to be computed.
#' @param from,to  numeric; the range over which to optimize the bandwidth.
#' @param parallel logical: if \code{FALSE} (defualt), parallel process will not be used to evaluate the compak smoother.
#'
#' @return The optimal bandwidth parameter within the range.
#' @export
#'
#' @examples
#' days <- c(rep(25,5), rep(26,5),rep(27,7),rep(28,8),
#' rep(29,11),rep(30,2),rep(31,1),rep(32,4),
#' rep(33,4),rep(34,2),rep(35,2))
#' h.KL <- compak_KLbandwidth(days, from = 0.025, parallel=F)
#'
compak_KLbandwidth <- function(a.sample, from = 0, to = 1, parallel = FALSE){

  # range of x values to compute KL divergence on
  range.x <- 0:200

  # fit the Poisson pmf
  mu <- mean(a.sample)
  f.pois <- dpois(x=range.x, lambda=mu)

  # fit the Neg-Bin pmf via Method of Moments if counts are "overdispersed"
  size <- mu^2/(var(a.sample)-mu)
  if(size > 0)  f.nb <- dnbinom(range.x, mu=mu, size=size)


  # We optimize over this function. Requires Laplaces Demon library
  MKL <- function(h, a.sample){
    # fit the CMP_mu kde with dispersion 1/h
    if (parallel) {
      fhat <- compak_evalpmf(range.x, a.sample, 1/h)
    } else {
      fhat <- compak_evalpmf(range.x, a.sample, 1/h, parallel=FALSE)
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

  h_kl <- optimize(f=MKL, interval=c(from, to), a.sample=a.sample)$minimum

  return(h_kl)
}
