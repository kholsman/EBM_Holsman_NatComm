calcRisk<-function(delta=grabDat(datIn=dat_019_CENaivecf_0_5_12,
                                 valIn="Catch_total_biom")[[sp]],limm=lim[l],
                   Yrbin=c(2017,2025,2050,2075,2100),esmlist=list(rcp45_n,rcp85NoBio_n),mn=FALSE){
  #delta<-tmpC<-C_2_5_12[[sp]]
  nbin<-length(Yrbin) 
  nlist<-length(esmlist)
  risk<-data.frame(matrix(NA,nlist,nbin-1))
  colnames(risk)<-paste(paste0("(",Yrbin[-nbin]),paste0(Yrbin[-1],"]"),sep=",")
  rownames(risk)<-1:nlist
  rownames(risk)<-c("rcp45","rcp85")
  tmpC<-delta
  for(esm in 1:nlist){
    cc<-esmlist[[esm]]
    for(y in 1:(nbin-1)){
      rr<-which(tmpC$Year>Yrbin[y]&tmpC$Year<=Yrbin[y+1] )
      delta[,-1]<- round((tmpC[,-1]- tmpC$persistence)/ tmpC$persistence,5)*100
      risk[esm,y]<-100*round(length(which(unlist(delta[rr,cc])<=(limm)))/length(unlist(delta[rr,cc])),4)
      if(mn) risk[esm,y]<-round(mean(unlist(delta[rr,cc])),2)
      #risk[esm,y]<-100*round(length(which(unlist(delta[rr,cc])<=(limm)))/length(unlist(delta[rr,cc])),4)
    }
  }
  return(risk)
}