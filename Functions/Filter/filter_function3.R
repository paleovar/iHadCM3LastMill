#' Filter_Function

filter_function3 <- function(d18O,h, tau, dt){
  h <- c(rep.int(0, as.integer(tau/dt)), h)
  d18O_filtered = numeric(length(d18O)+length(h))
  for (ii in 1:length(d18O)){
    for (jj in 1:length(h)){
      d18O_filtered[ii+jj-1] = d18O_filtered[ii+jj-1] + d18O[ii]*h[jj]
    } 
  }
  d18O_filtered = DescTools::AUC(seq(0:(length(d18O)-1)),d18O)/DescTools::AUC(seq(0:(length(d18O_filtered)-1)),d18O_filtered)*d18O_filtered
  
  
  return (d18O_filtered)
}

