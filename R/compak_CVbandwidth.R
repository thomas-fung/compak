compak_CVbandwidth = function(a.sample, interval=c(0.025,1)){
  # performs leave-one-out cross-validation for bandwidth selection
  # inputs a.sample of data
  # interval specifies range of bandwidth to optimize over
  
  likelihood.cv = function(h, a.sample){
    n = length(a.sample)
    f.cv = matrix(, nrow = n, ncol = 1)
    nu = 1/h
    for(i in 1:n){
      f.cv[i] = compak_evalpmf(a.sample[i], a.sample[-i], nu, parallel =F)
    }
    return(-sum(log(f.cv)))
  }
  
  h_cv = optimize(f=likelihood.cv, interval=interval, a.sample=a.sample)$minimum
  return(h_cv)
}