

get_targetB <- function(fldIN){
  
  target_B        <-  rbind(
    getBtarget(fldrIN=fldIN[1],nm="B0_set"),
    getBtarget(fldrIN=fldIN[2],nm="B0_set"))
  rownames(target_B)  <-  names(txt)
  return(target_B)
}