getBtarget<-function(fldrIN,nm="B0_set"){
  
  tt<-scan(file=fldrIN,what=character(),sep="\n")
  tmp<-tt[grep(nm,tt)+1]
  tmp<-as.numeric(strsplit(tmp,split=" ")[[1]])
  return(tmp)
}
