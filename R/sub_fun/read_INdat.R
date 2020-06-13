# Read in datat
# 
read_INdat<-function(dat_path=dat_path,fl="",SaveIt=TRUE,coluse=c("orange",col1(6)),ltyy1=1,alpha1=50,plot.path=plot_file){
  dat<-read.csv(file.path(dat_path,"results/Future_report.rep"),sep=" ")
  snames<-c("Pollock","P. cod","arrowtooth")
  
  tmp<-dat
  scenarios<-unique(tmp$fut_simulation)
  # print(scenarios)
  ltyyuse<-rep(ltyy1,max(scenarios))
  # dat<-tmp
  tmpDat<-list()
  tmpDat$Alldat<-dat
  Fc<-(which(names(dat)=="F_rate")+2):(which(names(dat)=="objective_fun")-1)
  Bc<-(which(names(dat)=="SSB.SSB0")+2):(grep("catch.biomass",names(dat))-1)
  Rc<-(grep("recruits.numbers",names(dat))+2):dim(dat)[2]
  Cc<-(grep("catch.biomass",names(dat))+2):(grep("recruits.numbers",names(dat))-1)
  lab<-dat[,which(names(dat)=="SSB.SSB0")]
  fr<-tmpDat$fr<-which(lab=="SSB")
  fr0<-tmpDat$fr0<-which(lab=="SSB0")
  tmpDat$dat0<-dat0<-dat[fr0,]
  tmpDat$dat<-dat<-dat[fr,]
  tmpDat$rec<-dat[,Rc]
  tmpDat$Frate<-dat[,Fc]
  tmpDat$SSB<-dat[,Bc]
  tmpDat$SSB0<-dat0[,Bc]
  tmpDat$catch<-dat[,Cc]
  ltyy<-ltyyuse
  tmpDat$species<-dat0$species
  tmpDat$sim<-dat0$fut_simulation
  tmpDat$itr<-dim(dat0[dat0$fut_simulation==scenarios[1]&dat0$species==1,Rc])[1]
  tmpDat$year<-as.numeric(substr(names(dat[,Rc]),2,5))
  return(tmpDat)
}
