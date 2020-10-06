compak_evalpmf = function(x, a.sample, nu, parallel = FALSE){
  # fits CMP_mu discrete kernel pmf P(X = x) smoother for a.sample of counts
  # evaluated at a (vector of) point(s) x
  # dispersion (bandwidth) parameter nu  which is the reciprocal of the bandwidth (1/h)
  # parallel determines whether to use parallel process to evaluate the kde
  
  # for parallel evaluating the kernels (requires parallel library)
  numCores = detectCores()
  
  npoints = length(x)
  fhat = vector(length = npoints)
  samp.points = length(a.sample)
  
  #first get number of observations for each observed count
  counts = list()
  for(i in a.sample){
    if(!is.null(counts[[toString(i)]])){
      counts[[toString(i)]] = counts[[toString(i)]] + 1
    }
    else{
      counts[[toString(i)]] = 1
    }
  }
  
  # evaluate kernels
  keys = list.names(counts) # this is the list of distinct count values observed as strings.  Requires rlist library
  if (parallel){
    results = mclapply(X=keys, FUN=compak_evalkernel, counts = counts, x=x, nu=nu, mc.cores=numCores)
  } else {
    results = lapply(X=keys, FUN=compak_evalkernel, counts = counts, x=x, nu=nu)
  }
  
  # sum kernels together
  for(ker in results){
    fhat = fhat + ker
  }
  
  # return the normalised kde at the desired x values
  return(fhat / samp.points)
}