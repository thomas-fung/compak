#' Fit compak smoother at values x using a sample of data
#'
#' The function estimates the p.m.f. using the Conway-Maxwell-Poisson associated kernels (compak) smoother. Bandwidth can be specified as \code{h} or \code{nu} where \code{nu}= 1/\code{h}. If none is specified, one can be provided by using cross-validation or minimising Kullback-Leibler divergence.
#'
#' @param a.sample numeric vector: the data sample from which estimate is to be computed.
#' @param x Either \code{NULL} or a numeric vector: the points of the grid at which the density is to be estimated. If none is provided, the range of \code{a.sample} will be used.
#' @param h,nu numeric: the bandwidth or smoothing parameter. Only one is needed and they are related by \code{nu} = 1/\code{h}. If neither \code{h} nor \code{nu} is provided, a bandwidth selection will be carried out.
#' @param workers numeric; a positive integer to represent the number of cores used for parallel processing to evaluate the kde
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
#' \item{bandwidth_optim}{the type of bandwidth selection used - same as input}
#' \item{kernel.est}{a list that contains the estimated kernel at each grid point}
#' @export
#'
#' @examples
#' ### Huang et, al (2020) Page 10
#' data(days)
#' fit.compak2 <- compak_fitpmf(days, 10:40, bandwidth_optim = "CV")
#' fit.compak2
#'
#' ### Huang et, al (2020) Page 9
#' \donttest{
#' data(somites)
#' fit.compak <- compak_fitpmf(somites, 60:180, bandwidth_optim = "CV")
#' }
#'
compak_fitpmf <- function(a.sample, x = NULL, h = NULL, nu = NULL,
                          workers = 1L,
                          bandwidth_optim = "KL", ...) {
  call <- match.call()
  a.sample_name <- call$a.sample
  if (sum(is.na(a.sample))!=0){
    stop('a.sample contains missing values')
  }
  if (!is.null(h) && !is.null(nu) && h != 1 / nu) {
    stop("provide at most one of 'h' or 'nu' but not both. If both are empty, the bandwidth will be selected for you.")
  } else if (!is.null(nu)) {
    h <- 1 / nu
  }
  if (is.null(x)) {
    x <- min(a.sample):max(a.sample)
  }

  if (!is.numeric(workers) || floor(workers) != workers || workers < 0) {
    warning("workers must be a positive integer. Resetting it to default = 1.")
    workers <- 1
  } else {
    if (workers > parallelly::availableCores()) {
      warning("The number of requested workers is greater than what's available on your system. Reset workers to what's available.")
      workers <- parallelly::availableCores()
    }
  }

  if (workers > 1) {
    future::plan(future::multisession, workers = workers)
  }

  if (is.null(h)) {
    if (bandwidth_optim == "KL") {
      h <- compak_KLbandwidth(a.sample, workers = 1, ...)
    } else if (bandwidth_optim == "CV") {
      h <- compak_CVbandwidth(a.sample, workers = 1, ...)
    } else {
      stop('"bandwidth_optim" can only be "KL" or "CV"')
    }
  } else {
    warning("The bandwidth_optim option is ignored as user provided a value for h or nu.")
    bandwidth_optim <- "user_specified"
  }

  f.cmp <- compak_evalpmf(a.sample = a.sample, x = x, nu = 1 / h, workers = 1)
  out <- list(
    call = call,
    data_name = a.sample_name,
    "f.cmp" = f.cmp$f.cmp, "x" = x, "h" = h,
    "nu" = 1 / h,
    data = a.sample,
    bandwidth_optim = bandwidth_optim,
    kernel.est = f.cmp$kernel.est
  )
  if (workers > 1) {
    future::plan(future::sequential)
  }
  class(out) <- "compak"
  return(out)
}
