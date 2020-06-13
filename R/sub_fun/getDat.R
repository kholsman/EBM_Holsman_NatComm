#'-------------------------------------
#'  getDat
#'-------------------------------------
#' This function extracts any value from the dat_2_5_x files
getDat<-function(dat,scn=1,sp=1,age=1,val="B"){
  subDat<-dat[dat$Scenario==scn&dat$age==age&dat$species==sp,]
  xx<-subDat[,"future_year"]
  yy<-subDat[,val]
  return(yy)
}