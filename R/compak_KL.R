#' Calculate the Kullback-Leibler divergence between two density functions
#'
#' @param f1,f2 numeric vectors: two density functions
#'
#' @return The Kullback-Leibler distance between the two density functions.
#' @keywords internal
#'
compak_KL = function(f1, f2){
  # remove zeros from the KL sum
  non.zero = which(f1>0)
  KL = sum(f1[non.zero]*(log(f1[non.zero])-log(f2[non.zero])))
  return(KL)
}
