#calcDelta.R

calcDelta<-function(datIN       = datuse_B,
                   limm        = -10,
                   YrbinIN     = Yrbin,
                   ref_scen    = 1,
                   esmlistIN   = list(rcp45=grep("rcp45",Scenarios),rcp85=grep("rcp85",Scenarios)),
                   delta_var_nm = "SSB_total_biom",
                   mn=FALSE){
  
  nbin           <- length(YrbinIN) 
  itr            <- sort(unique(datIN$MC_n))
  scen           <- sort(unique(datIN$Scenario))
  yrbin_nm       <- paste(paste0("(",Yrbin[-nbin]),paste0(Yrbin[-1],"]"),sep=",")
  
  nlist          <- length(esmlistIN)
  nscen          <- length(scen)
  nitr           <- length(itr)
  nlist          <- length(esmlistIN)
  
  if(nitr==0){
    nitr     <- 1
    itr      <- -999
    datIN$MC_n <- -999
  }
  
  if(!any(names(datIN)==delta_var_nm))  
    stop("delta_var_nm does not match any column names of datIN")
  names(datIN)[names(datIN)==delta_var_nm] <- "var"

  esm_df <- data.frame(scen =unlist(esmlistIN),rcp = rep.int(names(esmlistIN),times =lengths(esmlistIN)))
  esm_df$rcp[match(c(5,8),esm_df$scen)]
  
      for(y in 1:(nbin-1)){
       
        dd_ref             <-  datIN%>%filter(Year>YrbinIN[y]&Year<=YrbinIN[y+1],Scenario==ref_scen)%>%
          select("sp","MC_n",Scenario_ref=Scenario,"Year",TempC_ref=bottomT_C,var_ref=var)
        dd                 <-  datIN%>%filter(Year>YrbinIN[y]&Year<=YrbinIN[y+1],Scenario!=ref_scen)%>%select("sp","MC_n",Scenario,"Year",TempC=bottomT_C,var)
        dd_delta           <-  merge(x=data.frame(dd),y=data.frame(dd_ref),by=c("sp","MC_n","Year"),all.x=T)
        dd_delta$YrBin        <-  yrbin_nm[y]
        dd_delta$delta_var_nm <-  delta_var_nm
        dd_delta$rcp          <-  esm_df$rcp[match(dd_delta$Scenario,esm_df$scen)]
        dd_delta$delta_var         <-  dd_delta$var - dd_delta$var_ref
        dd_delta$delta_TempC       <- dd_delta$TempC - dd_delta$TempC_ref
        dd_delta$delta_var_prcnt   <-  round((dd_delta$var - dd_delta$var_ref)/dd_delta$var_ref,5)*100
        dd_delta$delta_TempC_prcnt <- round((dd_delta$TempC - dd_delta$TempC_ref)/dd_delta$TempC_ref,5)*100
        dd_delta$belowLim          <- 0
        dd_delta$belowLim[dd_delta$delta_var_prcnt<=limm] <- 1
        
        if(y==1){
          deltaOUT      <-  dd_delta
        }else{
          deltaOUT      <-  rbind(deltaOUT,dd_delta)
        }  
        rm(list=c("dd_delta","dd","dd_ref"))
      }
  return(deltaOUT)
}