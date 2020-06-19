#' get_romsnpz()
#' produce derived products for EBM paper from raw simulation output from CEATTLE
#' @param retroFLIN hindcast/retro data file
#' @param futFLIN   projection data file
#' @param covn Covariate number, e.g. 1 = Bottom Temperature
#' @param replace_nm covariate type value of 1 adds "# " to the covarnames
#' @param scenIN future scenario to combine with retro data for the file BT_dat
#' @param nm_retro hindcast/retro data file
#' @param nm_fut   replacement value to use if covn
#' 
#' 
get_romsnpz <- function(outfile      = "data/raw/covariates.Rdata",
                        retroFLIN    = retroFL,
                        futFLIN      = futFL,
                        covn         =  1,
                        replace_nm   =  TRUE,
                        scenIN       =  9,
                        nm_retro     = c("BT_retro","SST_retro"),
                        nm_fut       = c("BT_future","SST_future")){
 
  
  # Load ROMSNPZ data
  #_______________________________________
  
  # retro. data:
  #------------------------------------
  if(status) cat("reading retro. data")  
  tt        <-  scan(file=retroFLIN,what=character(),sep="#",skip=1, nlines=1)
  covars    <-  unlist(strsplit(tt,split=" ")) 
  covars    <-  covars[covars!=""]
  
  covariates    <-  data.frame(Year =-999,covar ="BottomTemp",Value = -999, Scenario="persistence")
  if(replace_nm){
    covars_retro   <-  c(nm_retro,covars)
    covars_fut     <-  c(nm_fut ,covars)
  }
  ncovars   <-  length(covars_retro)
  
  findTXT <- function(x, searchList){
    tmp <- searchList[which(unlist(lapply(x, function(x) grepl(x,searchList,fixed=T))))+1]
    as.numeric(strsplit(tmp,split=" ")[[1]])
  }
  findTXT <- function(x, searchList){
    which(unlist(lapply(x, function(x) grepl(x,searchList,fixed=T))))
  }
  
  for(covn in 1:ncovars){
   
    # retro   <-  paste0("#  ", covars_retro[covn])
    # fut     <-  paste0("# " , covars_fut[covn])
    retro     <- covars_retro[covn]
    fut       <- covars_fut[covn]
    if(status) cat(paste("reading: ", retro))
    tt        <-  scan(file=retroFLIN,what=character(),sep="\n",skip=2)
    nn        <-  findTXT(list(retro),searchList=tt)
    tmp       <-  as.numeric(strsplit(tt[nn+1],split=" ")[[1]])
    ny        <-  as.numeric(tt[grep("#nyrs",tt)+1])
    yrs       <-  tt[grep("#Retro_years",tt)+1]
    yrs       <-  as.numeric(strsplit(yrs,split=" ")[[1]])
    tmp_hind  <-  tmp
    
    # future data:
    #------------------------------------
    #if(status) cat("reading future data")  
    tt                 <-  scan(file=futFLIN,what=character(),sep="\n",skip=2)
    nn                 <-  findTXT(list(fut),searchList=tt)
    nyrs_fut           <-  as.numeric(tt[grep("#nyrs_fut",tt)+1])
    n_fut_itr          <-  as.numeric(tt[grep("#n_fut_itr",tt)+1])
    yrs_fut            <-  tt[grep("#fut_years",tt)+1];    
    yrs_fut            <-  as.numeric(strsplit(yrs_fut,split=" ")[[1]])
    tmp                <-  tt[grep(fut,tt)[1]+1:n_fut_itr]
    tmp_fut            <-  matrix(unlist( lapply(tmp,function(x) as.numeric(strsplit(x,split=" ")[[1]]))),n_fut_itr,nyrs_fut,byrow=T)
    colnames(tmp_fut)  <-  yrs_fut
    
    Years         <-  c(yrs,yrs_fut)
    nYrsTot       <-  length(Years)
    
    BT_dat    <-  data.frame(t=c(yrs,yrs_fut),temp=c(tmp_hind,tmp_fut[scenIN,]))
    BT_dat$t  <-  strptime(paste(BT_dat$t,"01-01",sep="-"),format="%Y-%m-%d")
    
    # make full matrix of data for bottom Temperature
    simnames  <-  tt[grep("mn_Hind",tt)+(1:n_fut_itr)-1]
    simnames  <-  unlist(strsplit(simnames,"#  | "))
    simnames  <-  simnames[-which(simnames%in%c("","|"))]
    simnames[simnames=="mn_Hind"]  <-  "persistence"
    
    allDat  <-  data.frame(matrix(NA,dim(BT_dat)[1],1+n_fut_itr))
    colnames(allDat)  <-  c("t",simnames)
    i  <-  1
    allDat[,1:2]  <-  cbind(c(yrs,yrs_fut),c(tmp_hind,tmp_fut[i,]))
    for(i in 2:n_fut_itr)
      allDat[,i+1]  <-  c(tmp_hind,tmp_fut[i,])
    rownames(allDat)<-allDat$t
    
    out <- reshape2::melt(allDat, id.vars=c("t"),variable.name="Scenario")
    out$Var <- retro
    if(covn==1){
      covariates  <- out
    }else{
      covariates  <- rbind(covariates,out)
    }
   rm(out)
   rm(allDat)
  }
 
  covariates$note <- "unitless; Z-score scaled covar"
  covariates[covariates$Var%in%c(nm_retro,nm_fut),]$note <- "deg C"

  save(covariates,file=outfile)
  
}
