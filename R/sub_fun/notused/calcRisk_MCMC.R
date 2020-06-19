calcRisk_MCMC<-function(delta=dat_019_CENaivecf_0_5_12_mc[sp,,,],limm=-10,logit=F,
                        Yrbin=c(2017,2025,2050,2075,2100),esmlist=list(rcp45_n,rcp85NoBio_n)){
  #delta<-tmpC<-C_2_5_12[[sp]]
  nbin<-length(Yrbin) 
  nitr<-dim(delta)[1]
  Years<-as.numeric(rownames(delta[1,,]))
  nlist<-length(esmlist)
  mnrisk<-matrix(NA,nlist,nbin-1)
  
  risk<-array(NA,c(nlist,nitr,nbin-1))
  dimnames(risk)<-list(c("rcp45","rcp85"),paste("itr",1:nitr),
                       paste(paste0("(",Yrbin[-nbin]),paste0(Yrbin[-1],"]"),sep=","))
  
  colnames(mnrisk)<-paste(paste0("(",Yrbin[-nbin]),paste0(Yrbin[-1],"]"),sep=",")
  rownames(mnrisk)<-1:nlist
  rownames(mnrisk)<-c("rcp45","rcp85")
  sdrisk<-mnrisk
  
  for(esm in 1:nlist){
    cc<-esmlist[[esm]]
    for(ii in 1:nitr){
      tmpC<-delta[ii,,]
      for(y in 1:(nbin-1)){
        rr<-which(Years>Yrbin[y]&Years<=Yrbin[y+1] )
        tmpp<- round((tmpC[rr,cc]- tmpC[rr,1])/ tmpC[rr,1],5)*100
        risk[esm,ii,y]<-100*round(length(which(unlist(tmpp)<=(limm)))/length(unlist(tmpp)),4)
      }  #risk[esm,y]<-100*round(length(which(unlist(delta[rr,cc])<=(limm)))/length(unlist(delta[rr,cc])),4)
    }
    mnrisk[esm,]<-(apply((risk[esm,,]),2,mean,na.rm=T))
    sdrisk[esm,]<-(apply((risk[esm,,]),2,sd,na.rm=T))
    if(logit) mnrisk[esm,]<-logit(apply(inv.logit(risk[esm,,]),2,mean,na.rm=T))
    if(logit) sdrisk[esm,]<-logit(apply(inv.logit(risk[esm,,]),2,sd,na.rm=T))
  }
  return(list(mcmcRisk=risk,mnRisk=mnrisk,sdRisk=sdrisk))
}
