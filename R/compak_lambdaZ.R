compak_lambdaZ = function(mu, nu){
  # mu must be integer here, but
  # mu can be a vector of means (observations)
  # with common nu (although also works with vectorized nu)
  mu.index = mu ;
  nu.index = nu*10 + 1;
  nu.index.lower = floor(nu.index)
  nu.index.upper = ceiling(nu.index)
  weight = nu.index%%1
  logLambda.lower = logLambda[mu.index, nu.index.lower]
  logLambda.upper = logLambda[mu.index, nu.index.upper]
  logLambda.out = (1-weight)*logLambda.lower + weight*logLambda.upper
  
  logZ.lower = logZ[mu.index, nu.index.lower]
  logZ.upper = logZ[mu.index, nu.index.upper]
  logZ.out = (1-weight)*logZ.lower + weight*logZ.upper
  logZ.out
  
  return(list("logLambda"=logLambda.out, "logZ"=logZ.out))
}