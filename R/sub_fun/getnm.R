# getnm.R
# function to get name

getnm<-function(nm=mclist0[1]){
  nmi<-strsplit(nm,split=paste0("Summary_proj_",fldr))[[1]][2]
  nmi<-strsplit(nmi,split="_mc")[[1]][1]
  return(paste0("dat",nmi,"_mc"))
  
}