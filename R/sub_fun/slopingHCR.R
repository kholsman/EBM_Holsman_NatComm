slopingHCR <- function(
  Ftarget  =  tail(Fished$B)[1]/tail(noF$B)[1],
  Bratio   =  .2,
  alpha    =  .05,
  Cbeta    =  .2){
  
  maxFabc  <-  0*Bratio  # pre-allocate:
  
  if(any(Bratio<1)){ 
    
    if(any(Bratio>=alpha)) 
      maxFabc[Bratio>=alpha] <- Ftarget*((Bratio[Bratio>=alpha]-alpha)/(1-alpha))
    
    if(any(Bratio<alpha))
      maxFabc[Bratio<alpha]  <- 0
    
    if(any(Bratio<=Cbeta))
      maxFabc[Bratio<=Cbeta] <- 0
    
  }
  
  if(any(Bratio>=1))
    maxFabc[Bratio>1]        <- Ftarget
  
  return(maxFabc)
}
