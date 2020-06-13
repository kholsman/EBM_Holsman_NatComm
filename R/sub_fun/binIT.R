#' make bins
binIT  <-  function(x,bins=seq(0,1000,10)){unlist(lapply(x,function(x) bins[rev(which(x>=bins))[1]] ))}
