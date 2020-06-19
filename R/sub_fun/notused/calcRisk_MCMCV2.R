#calcRisk_MCMCV2.R

calcRisk_MCMC<-function(
                   datIN       = datuseMCMC_B,
                   limm        = -10,
                   YrbinIN     = Yrbin,
                   esmlistIN   = esmlist,
                   delta_var   = "SSB_total_biom",
                   mn=FALSE){
  
  nbin           <- length(YrbinIN) 
  itr            <- sort(unique(datIN$MC_n))
  nitr           <- length(itr)
  nlist          <- length(esmlistIN)
  yrbin_nm       <- paste(paste0("(",Yrbin[-nbin]),paste0(Yrbin[-1],"]"),sep=",")

  risk           <- array(NA,c(nlist,nitr,nbin-1))
  dimnames(risk) <- list(c("rcp45","rcp85"),paste("itr",1:nitr),yrbin_nm)
  
  if(!any(names(datIN)==delta_var))  
    stop("delta_var does not match any column names of datIN")
  names(datIN)[names(datIN)==delta_var] <- "var"
  
  for(esm in 1:nlist){
    for(ii in 1:nitr){
      for(y in 1:(nbin-1)){
        for(v in 1:esm){
        dd_ref          <-  datIN%>%filter(Year>YrbinIN[y]&Year<=YrbinIN[y+1],Scenario==1,MC_n==itr[ii] )
        dd              <-  datIN%>%filter(Year>YrbinIN[y]&Year<=YrbinIN[y+1],Scenario==esmlistIN[[esm]][v],MC_n==itr[ii] )
        dd_delta        <-  merge(x=data.frame(dd_ref),y=data.frame(dd),by=c("MC_n","Year"))
        dd_delta$YrBin  <-  
        dd_delta$delta  <-  round((dd_delta$var.x - dd_delta$var.y)/dd_delta$var.x,5)*100
        risk[esm,ii,y]  <-  100* round ( length(which(dd_delta$delta<=limm)) /  length(dd_delta$delta),4)
        if(mn) 
          risk[esm,y]   <-  round(mean(dd_delta$delta),2)
        rm(list=c("dd_delta","dd","dd_ref"))
      }
    }
  }
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