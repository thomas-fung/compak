compak_KLbandwidth = function(a.sample, interval=c(0,1), parallel = FALSE){
  # Calculates the optimal bandwidth via minimisking KL divergence
  # inputs a.sample of count data
  # interval is the interval over which to optimize
  # parallel determines whether to use parallel process to evaluate the kde
  
  # range of x values to compute KL divergence on
  range.x = 0:200
  
  # fit the Poisson pmf
  mu = mean(a.sample)
  f.pois = dpois(x=range.x, lambda=mu)
  
  # fit the Neg-Bin pmf via Method of Moments if counts are "overdispersed"
  size = mu^2/(var(a.sample)-mu)
  if(size > 0)  f.nb = dnbinom(range.x, mu=mu, size=size)

  
  # We optimize over this function. Requires LaplacesDemon library
  MKL = function(h, a.sample){
    # fit the CMP_mu kde with dispersion 1/h
    if (parallel) {
      fhat = compak_evalpmf(range.x, a.sample, 1/h)
    } else {
      fhat = compak_evalpmf(range.x, a.sample, 1/h, parallel=FALSE)
    }

    #KL1 = KLD(fhat, f.pois)$sum.KLD.px.py
    KL1 = compak_KL(fhat, f.pois)

    # fit the Neg-Bin pmf via Method of Moments
    if(size <=0) KL2 = KL1 #if sample is "underdispersed" use Poisson
    else{
      KL2 = compak_KL(fhat, f.nb)
    }
    
    # return the Kullback-Leiber divergence between the kde and the Poisson pmf
    return(max(KL1, KL2))
  }
  
  h_kl = optimize(f=MKL, interval=interval, a.sample=a.sample)$minimum
  
  return(h_kl)
}