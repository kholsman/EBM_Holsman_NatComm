

getMCMC<-function(datin=dat_2_5_13_mc,
                  nspp=3,
                  nitr=20,
                  agein=1,
                  valin='SSB_total_biom'){
  
  datin%>%filter(age==agein)%>%
  eval(parse(text=paste0("tmp<-grabDat(datIn=",datin,1,",valIn=valin,age=agein)")))
  nrc             <-dim(tmp[[1]][,-1])
  tmpmc           <-array(NA,c(nspp,nitr,nrc[1],nrc[2]))
  s               <-1
  dimnames(tmpmc) <-list(paste0("sp",1:nspp),
                         paste0("itr",1:nitr),
                         tmp[[s]][,1],
                         colnames(tmp[[s]][,-1]))
  rm(tmp)
  for(itr in 1:nitr) {
    eval(parse(text=paste0("tmp<-grabDat(datIn=",datin,itr,",valIn=valin)")))
    for(s in 1:3){
      tmpmc[s,itr,,]<-as.matrix(tmp[[s]][,-1])
    }
  }
  return(tmpmc)
}