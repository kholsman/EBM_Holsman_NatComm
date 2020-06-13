#'________________________________________________
#'Comparaitve timeseries
#'________________________________________________
#'
modelTS<-function(
  sppINFOIN  = sppINFO,
  plotvals   = list(set1=c("SSB0_total_biom","SSB_total_biom"),
                    set2 =c("Catch_total_biom")),
  plotlist   = c("logCobs","logCmax","logRFR","logmnPreyED","logR", "G_hat" ),
  ageIN      = c(1),
  datlist    = list(cap=dat_2MT_219_CENaivecf1_2_5_13_mc,noCap=dat_219_CENaivecf_2_5_12_mc),
  sppIN      = c(1,2,3),
  startYR    = 1979,
  ScnIN      = 1:12,
  alpha1     =.5,
  alpha2     =.5,
  W          = 8,
  H          = 8,
  collin     = night,
  lgnpos     = c(.95,1.12),
  shading    = T,
  fn         = "_ts_bioINDX_bin.pdf",
  ylimmIN    = c(-2,2),
  altTheme   = T,
  trndfill   = night(10)[6],
  trndln     = "white",
  dataIN     = dat_2_5_3){
  s  <-  1; mn     <-  apply(dat_2MT_219_CENaivecf1_2_5_13_mc[s,,,],2:3,mean)
  s  <-  1; sd     <-  apply(dat_2MT_219_CENaivecf1_2_5_13_mc[s,,,],2:3,sd)
  s  <-  1; mn12   <-  apply(dat_219_CENaivecf_2_5_12_mc[s,,,],2:3,mean)
  s  <-  1; sd12   <-  apply(dat_219_CENaivecf_2_5_12_mc[s,,,],2:3,sd)
  
  sim  <-  4;plot(mn12[,sim],type="l",lwd=2,col="red")
  lines(mn12[,sim]-sd12[,sim],col="red")
  lines(mn12[,sim]+sd12[,sim],col="red")
  lines(mn[,sim],type="l",lwd=2)
  lines(mn[,sim]-sd[,sim])
  lines(mn[,sim]+sd[,sim])
  s  <-  1; mn      <-  apply(C_2MT_219_CENaivecf1_2_5_13_mc[s,,,],2:3,mean)
  s  <-  1; sd      <-  apply(C_2MT_219_CENaivecf1_2_5_13_mc[s,,,],2:3,sd)
  s  <-  1; mn12    <-  apply(C_219_CENaivecf_2_5_12_mc[s,,,],2:3,mean)
  s  <-  1; sd12    <-  apply(C_219_CENaivecf_2_5_12_mc[s,,,],2:3,sd)
  s  <-  1; mn3     <-  apply(C_219_CENaivecf_2_5_3_mc[s,,,],2:3,mean)
  s  <-  1; sd3     <-  apply(C_219_CENaivecf_2_5_3_mc[s,,,],2:3,sd)
  
  sim  <-  4;plot(mn12[,sim],type="l",lwd=2,col="red")
  
  lines(mn12[,1],type="l",lwd=2,col="red",lty=2)
  lines(mn12[,sim]-sd12[,sim],col="red")
  lines(mn12[,sim]+sd12[,sim],col="red")
  lines(mn[,sim],type="l",lwd=2)
  lines(mn[,1],type="l",lwd=2,lty=2)
  lines(mn[,sim]-sd[,sim])
  lines(mn[,sim]+sd[,sim])
  
  lines(mn3[,1],type="l",lwd=2,lty=2,col="blue")
  lines(mn3[,sim],type="l",lwd=2,lty=1,col="blue")
  lines(mn[,sim]-sd[,sim])
  lines(mn[,sim]+sd[,sim])
  
  dataIN$Year<-dataIN$future_year+startYR-1
  data<-dataIN[dataIN$age%in%ageIN&dataIN$species%in%sppIN&dataIN$Scenario%in%ScnIN,]
  spnm<-rep("",length(sppIN))
  for(s in 1:length(sppIN))
    spnm[s]<-sppINFOIN[[s]]$guildIN
  
  currentYR<-date();currentYR<-substr(currentYR,-3+nchar(currentYR),nchar(currentYR))
  
  # prepare a data frame for plotting
  summary(Indometh)
  
  wide <- reshape(Indometh, v.names = "conc", idvar = "Subject",
                  timevar = "time", direction = "wide")
  wide
  
  
  reshape(data, direction = "long")
  cnum<-match(c("Year","Scenario","species","age",unlist(plotvals)),colnames(data))
  data_base<-data[,cnum]
  cnum2<-which(colnames(data_base)%in%unlist(plotvals))
  dataLong<-reshape(data_base, direction = "long", idvar = "ID",varying = list(cnum2),v.names="val",timevar = "type")
  dataLong$plotby<-as.factor(paste0(dataLong$species,"_",dataLong$type))
  nn<-12
  
  
  p<-ggplot(dataLong, aes(x = Year, y = val,colour=Scenario)) +
    geom_line(aes(x = Year, y = val,color = Scenario),alpha = alpha1) +
    #geom_area(aes(x = Year, y = val,color = Scenario, fill = Scenario), alpha = alpha1, position = position_dodge(0.8)) +
    facet_wrap(~ plotby,nrow=length(unlist(plotvals)),ncol=length(sppIN)) +
    scale_color_manual(values = collin(nn)) +
    scale_fill_manual(values = collin(nn)) +
    
    geom_smooth(alpha=alpha2,se=T, method = "loess",fill=trndfill,color=trndln )  +
    
    theme_light()+
    labs(x=NULL, y=NULL,
         title=paste(currentYR,spnm),
         subtitle="Timeseries of diet-based bioenergetics indices for juvenile and adult fish based on stomach \nsamples collected during the annual AFSC bottom trawl survey.",
         caption=paste0("Credit: Kirstin Holsman ",currentYR,"\nData source: www.afsc.noaa.gov/REFM/REEM/data")) +
    theme(plot.subtitle=element_text(margin=margin(b=15))) +
    theme(legend.title=element_text(face="bold")) +
    theme(legend.position=lgnpos) +
    theme(plot.margin=margin(20,20,20,20)) 
  if(ylimmIN[1]!=FALSE){
    p<- p + xlim(ylimmIN[1],ylimmIN[2])
    p<- p + ylim(ylimmIN[1],ylimmIN[2]) 
  }
  if(altTheme) p<- p+ theme_kir_an(sub_title_size=12,
                                   sub_title_just="l",
                                   axis_title_just = "cm") 
  p
  
  print(paste0("plotting (3)",spnm))
  ggsave(filename=file.path(indxpath,paste0(spnm,fn)),device="png",dpi=500,width=W,height=H)
}  