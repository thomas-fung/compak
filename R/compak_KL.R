compak_KL = function(f1, f2){
  # Calculate the Kullback-Leibler divergence from f2 to f1.
  # remove zeros from the KL sum
  non.zero = which(f1>0)
  KL = sum(f1[non.zero]*(log(f1[non.zero])-log(f2[non.zero])))
  return(KL)
}