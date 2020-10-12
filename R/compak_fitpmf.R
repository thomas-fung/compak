#' Fit compak smoother at values x using a sample of data
#'
#' The function estimates the p.m.f. using the Conway-Maxwell-Poisson associated kernels (compak) smoother. Bandwidth can be specified as \code{h} or one can be provided by using cross-validation or minimising Kullback-Leibler divergence.

#' @param x numeric vector: the points of the grid at which the density is to be estimated.
#' @param a.sample numeric vector: the data sample from which estimate is to be computed.
#' @param h numeric: the bandwidth or smoothing parameter.
#' @param bandwidth
#' @param ... other arguments passed to bandwidth selection, such as \code{from} and \code{to}.
#'
#' @return
#' @export
#'
#' @examples
#'
compak_fitpmf = function(x, a.sample, h = NULL,
                         bandwidth = c("KL", "CV"), ...){
  if (is.null(x)) {
    x <- min(a.sample):max(a.sample)
  }
  if (is.null(h)){
    if(bandwidth == "KL"){
      h <- compak_KLbandwidth(a.sample, parallel = F, ...)
    } else if (bandwidth == "CV"){
      h <- compak_CVbandwidth(a.sample, ...)
    } else {
      stop('"bandwidth" can only be "KL" or "CV"')
    }
  } else h <- bandwidth

  f.cmp <- compak_evalpmf(x, a.sample, 1/h, parallel = F)
  return(list("f.cmp" = f.cmp, "h" = h))
}
