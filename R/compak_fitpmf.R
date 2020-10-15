#' Fit compak smoother at values x using a sample of data
#'
#' The function estimates the p.m.f. using the Conway-Maxwell-Poisson associated kernels (compak) smoother. Bandwidth can be specified as \code{h} or \code{nu} where \code{nu}= 1/\code{h}. If none is specified, one can be provided by using cross-validation or minimising Kullback-Leibler divergence.
#'
#' @param a.sample numeric vector: the data sample from which estimate is to be computed.
#' @param x Either \code{NULL} or a numeric vector: the points of the grid at which the density is to be estimated. If none is provided, the range of \code{a.sample} will be used.
#' @param h,nu numeric: the bandwidth or smoothing parameter. Only one is needed and they are related by \code{nu} = 1/\code{h}. If neither \code{h} nor \code{nu} is provided, a bandwidth selection will be carried out.
#' @param bandwidth_optim character; the type of bandwidth selection to be used. Possible values are "KL" (Kullback-Leibler divergence) and "CV" (cross-validation).
#' @param ... other arguments passed to bandwidth selection, such as \code{interval}.
#'
#' @return
#' An object class 'compak' is a list containing the following components:
#' \item{f.cmp}{the estimated p.m.f. values}
#' \item{data}{The data - same as input \code{a.sample}}
#' \item{h}{The bandwidth used to compute the density estimate}
#' \item{nu}{The dispersion used to compute the density estimate}
#' \item{x}{The coordinates of the points where the density is estimated}
#' @export
#'
#' @examples
#' ### Huang et, al (2020) Page 10
#' data(days)
#' fit.compak2 <- compak_fitpmf(days, 10:40, bandwidth_optim = "CV")
#'
#' ### Huang et, al (2020) Page 9
#' \donttest{
#' data(somites)
#' fit.compak <- compak_fitpmf(somites, 60:180, bandwidth_optim = "CV")
#' }

compak_fitpmf <- function(a.sample, x = NULL, h = NULL, nu = NULL,
                         bandwidth_optim = "KL", ...){

  if (!is.null(h) && !is.null(nu) && h != nu) {
    stop("specify 'h' or 'nu' but not both.")
  } else if (!is.null(nu)){
    h <- 1/nu
  }
  if (is.null(x)) {
    x <- min(a.sample):max(a.sample)
  }

  if (is.null(h)){
    if(bandwidth_optim == "KL"){
      h <- compak_KLbandwidth(a.sample, parallel = F)
    } else if (bandwidth_optim == "CV"){
      h <- compak_CVbandwidth(a.sample, ...)
    } else {
      stop('"bandwidth_optim" can only be "KL" or "CV"')
    }
  }

  f.cmp <- compak_evalpmf(a.sample = a.sample, x = x, nu = 1/h, parallel = F)
  out <- list("f.cmp" = f.cmp, "x" = x, "h" = h, "nu" = 1/h, data = a.sample)
  class(out) <- "compak"
  return(out)
}
