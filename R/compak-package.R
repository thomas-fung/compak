#' Conway-Maxwell-Poisson Associated Kernel
#'
#' @name compak-package
#' @aliases compak
#' @docType package
#' @title Conway-Maxwell-Poisson Associated Kernel
#' @keywords package
#' @references
#' Huang, A., Sippel, L. and Fung, T. (2020). \code{compak}:
#' Conway-Maxwell-Poisson Associated Kernel. R package version 0.1
#'
#' Huang, A., Sippel, L. and Fung, T. (2020). A consistent second-order discrete kernel smoother.
#' Under revision.
NULL

#' Days data set
#'
#' This data set gives the count of days required for the insect pest (spirally whitefly, Aleurodicus dispersus Russel) to develop from egg to adult stages while being hosted on the Hura fruit tree plant (Hura crepitans).The \code{days} data has 51 observations.
#'
#' @name days
#' @format A vector of 51 observations.
#'
#' @docType data
#' @keywords datasets
#' @usage
#' data(days)
#' @references
#' Kiessé, T.S. (2017). On finite sample properties of nonparametric discrete asymmetric kernel estimators. \emph{Statistics}, 51, 1046--1060.
#' @examples
#' ## For examples see example(compak_fitpmf)
NULL

#' somites data set
#'
#' This data set gives the number of body segments, known as somites, in common garden earthworms.
#' The \code{somites} data has 487 observations.
#'
#' @name somites
#' @format A vector of 487 observations.
#'
#' @docType data
#' @keywords datasets
#' @usage
#' data(somites)
#' @references
#' Owen, A.B. (2001). \emph{Empirical likelihood, Monographs on Statistics and Applied Probability (Series)}. Chapman & Hall, NY.
#'
#' Pearl, R., & Fuller, W.N. (1905). Variation and correlation in the earthworm. \emph{Biometrika}, 4, 213–229.
#'
#' @examples
#' ## For examples see example(compak_fitpmf)
NULL


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
