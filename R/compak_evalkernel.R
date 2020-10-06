compak_evalkernel = function(key, counts, x, nu){
  # Calculates the kernel at X_i = key
  # takes a named list of counts where the names are the observation values and the elements are the number of that count observed
  # evaluates the kernel multiplied by the number of counts at points in vector x
  # CMP_mu kernel has dispersion nu
  obvs = counts[[key]]
  X_i = strtoi(key)
  probs = obvs*compak_dcomp(x=x, mu=X_i, nu=nu)
  return(probs)
}