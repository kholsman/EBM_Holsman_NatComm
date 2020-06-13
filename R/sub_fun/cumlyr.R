#' get target biomass from ctl files:
cumlyr  <-  function(x,sumyr=3){
  
  x   <-  as.numeric(x)
  x2  <-  x*0
  for(i in sumyr:length(x)){
    x2[i]  <-  ifelse(sum(x[i-(1:sumyr)+1])==sumyr,1,0)
  } 
  return(x2==1)
  
}