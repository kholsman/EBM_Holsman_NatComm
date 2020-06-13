#'-------------------------------------
#'  grabDat
#'-------------------------------------
#' This function extracts any value from the dat_2_5_x files 
grabDat<-function(datIn=dat_2_5_3,valIn="B",age=1){
  outtmp<-list()
  for(spIn in 1:3){
    tmp<-data.frame(matrix(0,nYrsTot,length(simnames)+1))
    colnames(tmp)<-c("Year",simnames)
    tmp[,1]<-Years
    for(ss in 1:length(simnames))
      tmp[,ss+1]<-getDat(dat=datIn,scn=ss,val=valIn,sp=spIn,age=age)
    outtmp[[spIn]]<-tmp
  }
  return(outtmp)
}