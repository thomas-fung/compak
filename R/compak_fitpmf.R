compak_fitpmf = function(x, a.sample, bandwidth = "KL"){
  # Fits compak smoother at values x using a.sample of data
  # Default bandwidth choice is minimal KL
  # But can choose cross-validation visa bandwidth = "CV"
  # or user-specified bandwidth choice (everything else)
  
  if(bandwidth=="KL"){
    h = compak_KLbandwidth(a.sample, interval = c(0.025,1), parallel=F)
  }
  
  else if(bandwidth=="CV"){
    h = compak_CVbandwidth(a.sample, interval = c(0.025,1))
  }
  
  else h = bandwidth
   
  f.cmp = compak_evalpmf(x, a.sample, 1/h, parallel = F)
  return(list("f.cmp"=f.cmp, "h"=h))
}