###########################################################
## plots for EBM paper
## Kirstin Holsman 
## Sep 2018
###########################################################

# 1. Set up
# 2. load data

    rm(list=ls())
    graphics.off()
    

   #____________________
   # For tomorrow :
   # calculate delta C and delta B from persistence for each MCMC
   # Rerun MCMC now that it is fixed
   # maybe compare RCP 2.6 and 4.5 and 8.5? from ESRL portal 
  #-------------------------------------
  #   1. SET THINGS UP
  #-------------------------------------  
    library(RmarineHeatWaves); library(plyr); library(dplyr); library(ggplot2)
    library(svMisc)  #install.packages("svMisc")
    library( quantmod)
    #main  <-  "/Users/kholsman/GitHub/CEATTLE/docs/EBM_paper"
    main  <-  path.expand("~/GitHub/CEATTLE/docs_notShared/EBM_paper")
    main  <-  path.expand("~/Documents/D_AFSC_Files/Manuscripts/00_EBM_ACLIM_CEATTLE/EBM_paper")
    setwd(main)
   

    update.figs     <-  FALSE
    fldr            <-  "aclim_00_JunV2_2019"  #"aclim_00_Feb_2019"
    UpdateMCMC      <-  1      # update MCMC?
    readdat         <-  TRUE  # read in new data (0 read, 1 = load saved data) updated June 1 2019
    status          <-  TRUE   # print updates
    dpiIN           <-  150
    
    
    getnm<-function(nm=mclist0[1]){
      nmi<-strsplit(nm,split=paste0("Summary_proj_",fldr))[[1]][2]
      nmi<-strsplit(nmi,split="_mc")[[1]][1]
      return(paste0("dat",nmi,"_mc"))

    }

    
  #-------------------------------------
  #   2. LOAD DATA
  #-------------------------------------      
  # B0_219_CENaivecf_2_5_12_mc
    source(file = "FUN_GG_EBM_paper.R")
    source("FUN_EBM_paper.R")
    if(readdat==FALSE){
        load("EBM_ceattlenew.Rdata")    
    }else{
        source("SUB_EBM_paper.R")
        save.image(file = "EBM_ceattlenew.Rdata")
    } 
    

    riskTypes       <-  c("10% decline","50% decline","80% decline")
    RISK            <-  list("no cap" = risk12,"2 MT cap" = risk13)
    timeF           <-  levels(risk12$timeframe)


    flList          <-  dir("data/runs",paste0(fldr,"_0"))
    txt             <-  c("no cap"="_219_CENaivecf_2_5_12","2MT cap"="_2MT_219_CENaivecf1_2_5_13")
    fldin           <-  (paste0("data/runs/",fldr,"_2/projections/",fldr,txt,"/",fldr,txt,".ctl")) # KEY RUN
    target_B_2      <-  rbind(
                          getBtarget(fldrIN=fldin[1],nm="B0_set"),
                          getBtarget(fldrIN=fldin[2],nm="B0_set"))
    rownames(target_B_2)  <-  names(txt)
    txt              <-  c("no cap"="_019_CENaivecf_0_5_12","2MT cap"="_2MT_019_CENaivecf1_0_5_13")
    fldin            <-  (paste0("data/runs/",fldr,"_0/projections/",fldr,txt,"/",fldr,txt,".ctl")) # KEY RUN
    target_B_0       <-  rbind(
                          getBtarget(fldrIN=fldin[1],nm="B0_set"),
                          getBtarget(fldrIN=fldin[2],nm="B0_set"))
    rownames(target_B_0)<-names(txt)


# set the color scheme
    coll_use         <-  c(colors()[320],col2(6)[c(2,3,4)],col3(6)[c(3,4,6)])
    
# set up some plotting labels
    A1B_n_sim           <-  grep("A1B",simnames)
    bio_n_sim           <-  grep("bio",simnames)
    rcp45_n_sim         <-  grep("rcp45",simnames)
    rcp85_n_sim         <-  grep("rcp85",simnames)
    rcp85NoBio_n_sim    <-  setdiff(rcp85_n_sim,bio_n_sim)
    meanhistT           <-  mean(TempC_219_CENaivecf_2_5_12_mc[s,,,1][1,][hind_yrs])
      
    # get target biomass from ctl files:
    cumlyr  <-  function(x,sumyr=3){
      
      x<-as.numeric(x)
      x2<-x*0
      for(i in sumyr:length(x)){
        x2[i]<-ifelse(sum(x[i-(1:sumyr)+1])==sumyr,1,0)
      }
      return(x2==1)

    }

# plot cumulative years above threshold:
  head(TempC_019_CENaivecf_0_5_3_mc[1,1,,])
  above         <-  TempC_019_CENaivecf_0_5_3_mc[1,1,,]>2.1
  tt1           <-  apply(above,2,cumlyr,sumyr=5)
  tt            <-  apply(tt1,2,cumsum)/length(above[,1])
  yrs2          <-  as.numeric(rownames(above))
  findthrsh     <-  function(x,thrsh=.25,yrsIN=yrs2){
    yrsIN[as.numeric(x)>thrsh][1]
  }

  yrt           <-  apply(tt,2,findthrsh)
  plot(yrs2,tt)
  yrt[rcp85NoBio_n-1]
  yrt[rcp45_n-1]
  nscen         <-  length(c(rcp45_n,rcp85NoBio_n))
  plot(yrs2,tt[,2],ylim=c(0,nscen*1.2),xlim=c(1965,2100),type="l",col=NA,axes=F,ylab="",xlab="")
  axis(1)
  firstY<-rep(2017,nscen);names(firstY)<-colnames(tt1)[c(rcp45_n,rcp85NoBio_n)-1]
  for(i in 1:nscen){
    cc              <-  (c(rcp45_n,rcp85NoBio_n)-1)[i]
    ll              <-  NA*tt1[,cc]
    ll[tt1[,cc]]    <-  i
    suby            <-  yrs2[tt1[,cc]]
    if(any(suby>2017))
      firstY[i]    <-  suby[suby>2017][1]
    points(yrs2,ll,pch=16,cex=1.5,col=makeTransparent(dawn(13)[i],alpha=250))
  }

  text(rep(1980,nscen),1:nscen,colnames(tt1)[c(rcp45_n,rcp85NoBio_n)-1],cex=.8,col=dawn(13)[1:nscen])
  mean(firstY[1:3],na.rm=T)
  mean(firstY[4:6],na.rm=T)


#-------------------------------------
# Final figures:
#-------------------------------------
#fig 2: temperature
   graphics.off()
      GGplot_aclimTS(dat=allDat,h=2*1.3,w=4.75*1.3,
        ylabb=expression(paste("Bottom temperature",'('^{o},"C)")),
        ltyy=c("solid",rep("solid",6)),
        subtitle_face="plain",
        plotSet=list(c(1,rcp45_n),c(1,rcp85NoBio_n)),
        coll=coll_use,tline=2,talpha=.5,
        xlabb="",lgnpos= "right",plot_marginIN=c(-10,-1,-10,1))

   if(update.figs) ggsave(file=paste0("Figures/Fig2.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)  

#fig 3: delta B
    # 50% quantiles
  graphics.off()
      GGplot_aclimCEATTLE_delta(h=4.75*1.3,w=4.75*1.3,
        nmLIST  = list("SSB0"="dat_219_CENaivecf_2_5_12","SSB0"="dat_219_CENaivecf_2_5_12"),
        datLIST = list(dat1=B0_219_CENaivecf_2_5_12_mc,dat2=B0_219_CENaivecf_2_5_12_mc),
        valLIST = list(valIn1="SSB0_total_biom", valIn2="SSB0_total_biom"),
        prob= c(.01,.50,.9),plot_marginIN=c(-15,5,-10,5),alpha=c(10,5),lgnpos= "bottom",coll=coll_use)

  if(update.figs) ggsave(file=paste0("Figures/Fig3.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

#fig 4: delta C
    # 50% quantiles
 

  graphics.off()
      GGplot_aclimCEATTLE_delta(deltaIN=TRUE,h=4.75*1.3,w=4.75*1.3,ydiv=1,
        ylimm_up=c(100,100,100),ylimm_dwn=c(-200,-200,-200),
        plot_marginIN=c(-15,5,-10,5),alpha=c(0,0),lwdd=c(.7,.3),plotpersist = FALSE,
        coll = coll_use,
        ylabb   = expression(paste(Delta," Catch (%)")),
        xlimmIN = c(2010,2100),
        nmLIST  = list("2 MT cap"="dat_2MT_219_CENaivecf1_2_5_13","no cap"="dat_219_CENaivecf_2_5_12"),
        valLIST = list(valIn1="Catch_total_biom", valIn2="Catch_total_biom"),
        datLIST = list(dat1=B_219_CENaivecf_2_5_12_mc,dat2=B_219_CENaivecf_2_5_12_mc),
        lgnpos= "bottom",scalesIN="fixed")


  if(update.figs) ggsave(file=paste0("Figures/Fig4.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

  #fig 5: risk
  graphics.off()
      GGplot_aclimCEATTLE_risk(h=2*1.3,w=4*1.3,coll= c(col2(6)),sp=1,colvar="type",rowvar="sp",alpha=c(.4,1),
        plot_marginIN=c(-15,0,-10,5),mode="MSM",lwdd=c(.7,.4,.4),rcpIN=c("RCP 8.5"="rcp85"),
        nrowlg  = c(1,1),pchh=c(16,15),
        lgnpos= "bottom",RISKTYPES = riskTypes[c(1,2,3)],ltyy=c("solid","solid","solid"))

        grid.force()
        # change shape of arrows
        grid.gedit("segments", gp=gpar(linejoin ='mitre'))
        # change the shape in legend also
        grid.gedit("layout", gp=gpar(linejoin ='mitre'))

  if(update.figs) ggsave(file=paste0("Figures/Fig5.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)


  #fig 6: Threshold

dev.new(height=4.75*1.3,width=4.5*1.3)
PLOT_THRESHOLD2(
  multIN=10,
  firstdiff=T,
  ntemps=3,
  ylimmIN =c(-1,1.5),
  xlimmIN =c(1,7),
  trndln  = "white",
  trndln2 = Ornjazz[3],
  tipping = Ornjazz[5],
  sizeIN=c(0.1,.3,.75,2))

 if(update.figs) ggsave(file=paste0("Figures/Fig6.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

# Fig S1: HCR


  graphics.off()
  GG_HCRplot(h=3.5,w=8,futScen="GFDL_rcp45",fontSize=3,yfont=c(2070,2073))


 if(update.figs) ggsave(file=paste0("Figures/FigS1.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

# Fig S2: SSB with and without cap


      GGplot_aclimCEATTLE_delta(deltaIN=F,h=4.75*1.3,w=4.75*1.3,ydiv=1e6,
        plot_marginIN=c(-15,5,-10,5),alpha=c(0,0),lwdd=c(.7,.3),
        ylabb   = "Spawning biomass (million tons)",
        nmLIST  = list("2 MT cap"="dat_2MT_219_CENaivecf1_2_5_13","no cap"="dat_219_CENaivecf_2_5_12"),
        valLIST = list(valIn1="SSB_total_biom", valIn2="SSB_total_biom"),
        datLIST = list(dat1=B_219_CENaivecf_2_5_12_mc,dat2=B_219_CENaivecf_2_5_12_mc),
        lgnpos= "bottom",scalesIN="free_y")
  if(update.figs) ggsave(file=paste0("Figures/FigS2.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

# Fig S3: effective F


  graphics.off()
  dev.new(height=3.5,weight=5)
  plot_Feffective()

 if(update.figs) ggsave(file=paste0("Figures/FigS3.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)



# Fig S4: risk plot
 
    graphics.off()
      GGplot_aclimCEATTLE_risk(h=4.75*1.3,w=3.2*1.3,coll= c(col2(6)),colvar="type",rowvar="sp",alpha=c(.4,1),
        plot_marginIN=c(-15,5,-10,5),mode="MSM",lwdd=c(.7,.4,.4),rcpIN=c("RCP 8.5"="rcp85"),pchh=c(16,15),
        lgnpos= "bottom",RISKTYPES = riskTypes[c(1,3)],ltyy=c("solid","solid"))

        grid.force()
        # change shape of arrows
        grid.gedit("segments", gp=gpar(linejoin ='mitre'))
        # change the shape in legend also
        grid.gedit("layout", gp=gpar(linejoin ='mitre'))

if(update.figs) ggsave(file=paste0("Figures/FigS4.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)


# Fig S5: threshold 1
graphics.off()
dev.new(height=3*1.3,width=4.75*1.3)
#dev.new(height=3*1.3,width=4.75*1.3)
PLOT_THRESHOLD(
  dataIN_1=tmpall12_1,
  dataIN_2=tmpall12_2,
  dataIN_3=tmpall12_3,
  firstdiff=F,
  ntemps=3,
  multIN=10,
  #ylimmIN =c(-1.5,1.5),
  #xlimmIN =c(.5,7),
  ylimmIN =c(-1,1.5),
  xlimmIN =c(1,7),
  trndln  = "white",
  trndln2 = Ornjazz[3],
  tipping = Ornjazz[5],
  sizeIN=c(0.1,.3,.75,2))


 if(update.figs) ggsave(file=paste0("Figures/FigS5.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

# Fig S6: hindcast years








  #-------------------------------------
  #   Figure 1: PLOT TEMP
  #-------------------------------------  
 
      graphics.off()
      GGplot_aclimTS(dat=allDat,h=2*1.3,w=4.75*1.3,
        ltyy=c("solid",rep("solid",6)),
        plotSet=list(c(1,rcp45_n),c(1,rcp85NoBio_n)),
        coll=coll_use,tline=2,talpha=.5,
        xlabb="",lgnpos= "right",plot_marginIN=c(-10,-1,-10,1))
    
    graphics.off()
    
    GGplot_aclimTSv2(
      DAT =ROMSNPZdat,
      hindtype =".raw",
      refyr=c(2006:2016),
      threshold =mean(tmpall13_1$thrsh_x,tmpall13_2$thrsh_x,tmpall13_3$thrsh_x),
      futtype =".raw", #c("raw","bc","rc","bcs")
      biascorrect=TRUE,
      persistence = TRUE,
      threshold_range=c(
        min(tmpall13_1$thrsh_x,tmpall13_2$thrsh_x,tmpall13_3$thrsh_x),
        max(tmpall13_1$thrsh_x,tmpall13_2$thrsh_x,tmpall13_3$thrsh_x)),
      #threshold_range=c(min(all13),max(all13)),
      talpha2 =.6,
       threshcol="gray",
      hindset = "aclim_hindcast",
      plotSetIN = list("RCP 4.5" = c("GFDL_rcp45","MIROC_rcp45","CESM_rcp45"),"RCP 8.5" = c("GFDL_rcp85","MIROC_rcp85","CESM_rcp85")),
        h=2*1.3,w=4.75*1.3,,nrow=1,
        ltyy=c("solid",rep("solid",6)),
        coll=coll_use,tline=0,talpha=.7,
        xlabb="",lgnpos= "right",plot_marginIN=c(-10,-1,-10,1))


      fn<-"BT"
  if(update.figs) ggsave(file=paste0("Figures/Fig01_BTts.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)


    graphics.off()
      GGplot_aclimTSv2(
      DAT =ROMSNPZdat,
      hindtype =".raw",
      refyr=c(2006:2016),threshold =mean(tmpall13_1$thrsh_x,tmpall13_2$thrsh_x,tmpall13_3$thrsh_x),
      futtype =".raw", #c("raw","bc","rc","bcs")
      biascorrect=TRUE,
      persistence = TRUE,
       threshcol="gray",
      hindset = "aclim_hindcast",
      plotSetIN = list("RCP 4.5" = c("GFDL_rcp45","MIROC_rcp45","CESM_rcp45"),"RCP 8.5" = c("GFDL_rcp85","MIROC_rcp85","CESM_rcp85")),
        h=4*1.3,w=3.75*1.3,nrow=2,
        ltyy=c("solid",rep("solid",6)),
        coll=coll_use,tline=3,talpha=.5,
        xlabb="",lgnpos= "right",plot_marginIN=c(-10,-1,-10,1))

      fn<-"BT"
  if(update.figs) ggsave(file=paste0("Figures/Fig01v2_",fn,"ts.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)


  GGplot_aclimTSv2(
    DAT =ROMSNPZdat,
    hindtype =".raw",
    futtype =".raw", #c("raw","bc","rc","bcs")
    biascorrect=FALSE,
    threshold =mean(tmpall13_1$thrsh_x,tmpall13_2$thrsh_x,tmpall13_3$thrsh_x),
    persistence = TRUE,
    threshcol="gray",
    hindset = "aclim_hindcast",
    plotSetIN = list("RCP 4.5" = c("GFDL_rcp45","MIROC_rcp45","CESM_rcp45"),"RCP 8.5" = c("GFDL_rcp85","MIROC_rcp85","CESM_rcp85")),
    h=4*1.3,w=3.75*1.3,nrow=2,
    ltyy=c("solid",rep("solid",6)),
    coll=coll_use,tline=3,talpha=.5,
    xlabb="",lgnpos= "right",plot_marginIN=c(-10,-1,-10,1))

      fn<-"BT"
  if(update.figs) ggsave(file=paste0("Figures/Fig01v2RAW_",fn,"ts.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)




# update this with covars from all covars - add window for estimation 

#Rcp X covariates

#covlist= c("BottomTemp","ColdPool","FallZavg","SpringZavg")
# [1] "t"              "persistence"    "MIROC_A1B"      "ECHOG_A1B"     
# [5] "CCCMA_A1B"      "GFDL_rcp45"     "GFDL_rcp85"     "GFDL_rcp85_bio"
# [9] "MIROC_rcp45"    "MIROC_rcp85"    "CESM_rcp45"     "CESM_rcp85"    
# [13] "CESM_rcp85_bio"

  #-------------------------------------
  #   Figure 2: PLOT B0
  #-------------------------------------  
  

 
  
  # 50% quantiles
  graphics.off()
      GGplot_aclimCEATTLE_delta(h=4.75*1.3,w=4.75*1.3,
        nmLIST  = list("SSB0"="dat_219_CENaivecf_2_5_12","SSB0"="dat_219_CENaivecf_2_5_12"),
        datLIST = list(dat1=B0_219_CENaivecf_2_5_12_mc,dat2=B0_219_CENaivecf_2_5_12_mc),
        valLIST = list(valIn1="SSB0_total_biom", valIn2="SSB0_total_biom"),
        prob= c(.05,.50,.95),plot_marginIN=c(-15,5,-10,5),alpha=c(10,5),lgnpos= "bottom",coll=coll_use)

  if(update.figs) ggsave(file=paste0("Figures/Fig02_B0_ts_2575qnt.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

  
  # 50% quantiles
  graphics.off()
      GGplot_aclimCEATTLE_delta(h=4.75*1.3,w=4.75*1.3, alpha=c(0,0),
        nmLIST  = list("2 MT cap"="dat_2MT_219_CENaivecf1_2_5_13","no cap"="dat_219_CENaivecf_2_5_12"),
        datLIST = list(dat1=C_219_CENaivecf_2_5_12_mc,dat2=C_219_CENaivecf_2_5_12_mc),
        valLIST = list(valIn1="Catch_total_biom", valIn2="Catch_total_biom"),
        prob= c(.05,.50,.95),plot_marginIN=c(-15,5,-10,5),lgnpos= "bottom",coll=coll_use)

  if(update.figs) ggsave(file=paste0("Figures/Fig02_C_ts.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)
  

  graphics.off()
      GGplot_aclimCEATTLE_delta(h=4.75*1.3,w=4.75*1.3,
        nmLIST  = list("SSB0"="dat_219_CENaivecf_2_5_12","SSB0"="dat_219_CENaivecf_2_5_12"),
        datLIST = list(dat1=B0_219_CENaivecf_2_5_12_mc,dat2=B0_219_CENaivecf_2_5_12_mc),
        valLIST = list(valIn1="SSB0_total_biom", valIn2="SSB0_total_biom"),
        prob= c(.05,.50,.95),plot_marginIN=c(-15,5,-10,5),alpha=c(0,0),lgnpos= "bottom",coll=coll_use)

  if(update.figs) ggsave(file=paste0("Figures/Fig02_noshade_B0_ts.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)


DAT<-grabDat(datIn=dat_219_CENaivecf_2_5_12,valIn='SSB0_total_biom')
sp<-1


mnset<-list("RCP_45"=c("GFDL_rcp45","MIROC_rcp45","CESM_rcp45"),"RCP_85"=c("GFDL_rcp85","MIROC_rcp85","CESM_rcp85"))
blocks<-c(1970,seq(2000,2100,25))

mnD<-sdD<-seD<-list()
for(s in 1:3){
  tt<-data.frame(
  cbind(year=DAT[[s]][,1],
    100*(DAT[[s]][,-(1)]-DAT[[s]][,2])/DAT[[s]][,2]))

 
  tt$block<-length(blocks)
  for(i in 1:(length(blocks)-1)){
    rr<-which(tt$year>=blocks[i]&tt$year<blocks[i+1])
    tt$block[rr]<-i
  }
  mn_change<-matrix(NA,length(mnset),max(tt$block))
  colnames(mn_change)<-blocks[-1]
  rownames(mn_change)<-names(mnset)
  n_change<-sd_change<-mn_change

  for(i in 1:dim(mn_change)[2]){
    subtt<-as.numeric(unlist(tt[tt$block==i,colnames(tt)%in%mnset[[1]]]))
    mn_change[1,i]<-mean(subtt)
    sd_change[1,i]<-sd(subtt)
    n_change[1,i]<-length(subtt)
    subtt<-as.numeric(unlist(tt[tt$block==i,colnames(tt)%in%mnset[[2]]]))
    mn_change[2,i]<-mean(subtt)
    sd_change[2,i]<-sd(subtt)
    n_change[2,i]<-length(subtt)
  }
  se_change<-sd_change/sqrt(n_change)
  mnD[[s]]<-mn_change
  sdD[[s]]<-sd_change
  seD[[s]]<-se_change
}


# -46.71541
# -69.30791

# -31.95819
# -57.40636

# -7.670692
# -42.338140

  #-------------------------------------
  #   Figure 3: PLOT B40, 12, 13
  #-------------------------------------  
  A1B_n_sim           <-  grep("A1B",simnames)
  bio_n_sim           <-  grep("bio",simnames)
  rcp45_n_sim         <-  grep("rcp45",simnames)
  rcp85_n_sim         <-  grep("rcp85",simnames)
  rcp85NoBio_n_sim    <-  setdiff(rcp85_n_sim,bio_n_sim)
  

  graphics.off()
  GG_HCRplot(h=3.5,w=8,futScen="GFDL_rcp45",fontSize=3,yfont=c(2070,2073))


 if(update.figs) ggsave(file=paste0("Figures/FigS1_HCR.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

  graphics.off()
  dev.new(height=3.5,weight=5)
  plot_Feffective()

 if(update.figs) ggsave(file=paste0("Figures/Feffective.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

  graphics.off()
      GGplot_aclimCEATTLE_delta(deltaIN=F,h=4.75*1.3,w=4.75*1.3,ydiv=1e6,
        plot_marginIN=c(-15,5,-10,5),alpha=c(0,0),lwdd=c(.7,.3),
        ylabb   = "Spawning biomass (million tons)",
        ylimm_up    = c(20,2,1.5),
        ylimm_dwn   = c(0,0,0),
        nmLIST  = list("2 MT cap"="dat_2MT_219_CENaivecf1_2_5_13","no cap"="dat_219_CENaivecf_2_5_12"),
        valLIST = list(valIn1="Catch_total_biom", valIn2="Catch_total_biom"),
        datLIST = list(dat1=B_219_CENaivecf_2_5_12_mc,dat2=B_219_CENaivecf_2_5_12_mc),
        lgnpos= "bottom",scalesIN="free_y")


      GGplot_aclimCEATTLE_delta(deltaIN=F,h=4.75*1.3,w=4.75*1.3,ydiv=1e6,
        plot_marginIN=c(-15,5,-10,5),alpha=c(0,0),lwdd=c(.7,.3),
        ylabb   = "Spawning biomass (million tons)",
        nmLIST  = list("2 MT cap"="dat_2MT_219_CENaivecf1_2_5_13","no cap"="dat_219_CENaivecf_2_5_12"),
        valLIST = list(valIn1="SSB_total_biom", valIn2="SSB_total_biom"),
        datLIST = list(dat1=B_219_CENaivecf_2_5_12_mc,dat2=B_219_CENaivecf_2_5_12_mc),
        lgnpos= "bottom",scalesIN="free_y")
  if(update.figs) ggsave(file=paste0("Figures/Fig02_deltaB0_ts.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)


  
  graphics.off()
      GGplot_aclimCEATTLE_delta(deltaIN=T,h=4.75*1.3,w=4.75*1.3,ydiv=1,
        plot_marginIN=c(-15,5,-10,5),alpha=c(0,0),lwdd=c(.7,.3),
        ylabb   = "Spawning biomass (million tons)",
        nmLIST  = list("2 MT cap"="dat_2MT_219_CENaivecf1_2_5_13","no cap"="dat_219_CENaivecf_2_5_12"),
        valLIST = list(valIn1="SSB_total_biom", valIn2="SSB_total_biom"),
        datLIST = list(dat1=B0_219_CENaivecf_2_5_12_mc,dat2=B0_219_CENaivecf_2_5_12_mc),
        lgnpos= "bottom",scalesIN="free_y")

  #-------------------------------------
  #   Figure 4: PLOT risk
  #-------------------------------------  

    graphics.off()
      GGplot_aclimCEATTLE_risk(h=4.75*1.3,w=3.2*1.3,coll= c(col2(6)),colvar="type",rowvar="sp",alpha=c(.4,1),
        plot_marginIN=c(-15,5,-10,5),mode="MSM",lwdd=c(.7,.4,.4),rcpIN=c("RCP 8.5"="rcp85"),pchh=c(16,15),
        lgnpos= "bottom",RISKTYPES = riskTypes[c(1,2)],ltyy=c("solid","solid"))

        grid.force()
        # change shape of arrows
        grid.gedit("segments", gp=gpar(linejoin ='mitre'))
        # change the shape in legend also
        grid.gedit("layout", gp=gpar(linejoin ='mitre'))

if(update.figs) ggsave(file=paste0("Figures/Fig04_Risk.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

  
graphics.off()
      GGplot_aclimCEATTLE_risk(h=2*1.3,w=4*1.3,coll= c(col2(6)),sp=1,colvar="type",rowvar="sp",alpha=c(.4,1),
        plot_marginIN=c(-15,0,-10,5),mode="MSM",lwdd=c(.7,.4,.4),rcpIN=c("RCP 8.5"="rcp85"),
        nrowlg  = c(1,1),pchh=c(16,15),
        lgnpos= "bottom",RISKTYPES = riskTypes[c(1,2,3)],ltyy=c("solid","solid","solid"))

        grid.force()
        # change shape of arrows
        grid.gedit("segments", gp=gpar(linejoin ='mitre'))
        # change the shape in legend also
        grid.gedit("layout", gp=gpar(linejoin ='mitre'))

if(update.figs) ggsave(file=paste0("Figures/Fig04_2_Risk.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)
  
graphics.off()
      GGplot_aclimCEATTLE_risk(h=3.5*1.3,w=4*1.3,coll= c(col2(6)),sp=1,colvar="type",rowvar="rcp",alpha=c(.4,1),
        plot_marginIN=c(-15,0,-10,5),mode="MSM",lwdd=c(.7,.4,.4),rcpIN=c("RCP 4.5"="rcp45","RCP 8.5"="rcp85"),
        nrowlg  = c(1,1),pchh=c(16,15),
        lgnpos= "bottom",RISKTYPES = riskTypes[c(1,2)],ltyy=c("solid","solid","solid","dashed","dashed","dashed"))

        grid.force()
        # change shape of arrows
        grid.gedit("segments", gp=gpar(linejoin ='mitre'))
        # change the shape in legend also
        grid.gedit("layout", gp=gpar(linejoin ='mitre'))

if(update.figs) ggsave(file=paste0("Figures/Fig04_byRCP_Risk.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

graphics.off()
      GGplot_aclimCEATTLE_risk(h=5*1.3,w=2*1.3,coll= c(col2(6)),sp=1,colvar="sp",rowvar="type",alpha=c(.4,1),
        plot_marginIN=c(-15,0,-10,5),mode="MSM",lwdd=c(.7,.4,.4),rcpIN=c("RCP 8.5"="rcp85"),
        nrowlg  = c(2,1),pchh=c(16,15),
        lgnpos= "bottom",RISKTYPES = riskTypes[c(1,2,3)],ltyy=c("solid","solid","solid"))

        grid.force()
        # change shape of arrows
        grid.gedit("segments", gp=gpar(linejoin ='mitre'))
        # change the shape in legend also
        grid.gedit("layout", gp=gpar(linejoin ='mitre'))

if(update.figs) ggsave(file=paste0("Figures/Fig04_3_Risk.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

graphics.off()
      GGplot_aclimCEATTLE_riskV2(h=2.5*1.3,w=4*1.3,coll= c(col2(6)),sp=1,colvar="type",rowvar="sp",alpha=c(.4,1),
        plot_marginIN=c(-15,0,-10,5),mode="MSM",lwdd=c(.7,.4,.4),rcpIN=c("RCP 8.5"="rcp85"),
        nrowlg  = c(1,1),pchh=c(16,15),
        lgnpos= "bottom",RISKTYPES = riskTypes[c(1,2,3)],ltyy=c("solid","solid","solid")
        )

        grid.force()
        # change shape of arrows
        grid.gedit("segments", gp=gpar(linejoin ='mitre'))
        # change the shape in legend also
        grid.gedit("layout", gp=gpar(linejoin ='mitre'))

if(update.figs) ggsave(file=paste0("Figures/Fig04_3_Risk.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

#Table 1: Risk table:
 
 i<-ii<-0

for(j in c("10% decline","50% decline","80% decline")){
   for(rcpIN in c("rcp45","rcp85")){
    for(s in 1:3){
    for(tf in timeF){
     
        i<-i+1
        tmp1<-as_tibble(RISK[[1]])%>%filter(sp%in%s,timeframe%in%tf,mode%in%"MSM", rcp%in%rcpIN,type%in%j)
        tmp2<-as_tibble(RISK[[2]])%>%filter(sp%in%s,timeframe%in%tf,mode%in%"MSM", rcp%in%rcpIN,type%in%j)


        tmpsd1<-round(tmp1$riskCcv*tmp1$riskC,1)
        tmpsd1[is.na(tmpsd1)]<-0
        tmp1$sdriskC<-tmpsd1

        tmpsd2<-round(tmp2$riskCcv*tmp2$riskC,1)
        tmpsd2[is.na(tmpsd2)]<-0
        tmp2$sdriskC<-tmpsd2


        tmpsd1<-round(tmp1$riskBcv*tmp1$riskB,1)
        tmpsd1[is.na(tmpsd1)]<-0
        tmp1$sdriskB<-tmpsd1

        tmpsd2<-round(tmp2$riskBcv*tmp2$riskB,1)
        tmpsd2[is.na(tmpsd2)]<-0
        tmp2$sdriskB<-tmpsd2
        
        tmpOUTtmp<-data.frame(
          sp     =tmp1$sp,
          type   =tmp1$type,
          rcp    =tmp1$rcp,
          timeF  =tmp1$timeframe,
          sp2    =tmp2$sp,
          type2   =tmp2$type,
          rcp2    =tmp2$rcp,
          timeF2 =tmp2$timeframe,
          noCapRB=tmp1$riskB,
          noCapRBsd=tmp1$sdriskB,
          CapRB  =tmp2$riskB,
          CapRBsd=tmp2$sdriskB,
          deltaRB  =tmp2$riskB-tmp1$riskB,
          
          noCapRC=tmp1$riskC,
          noCapRCsd=tmp1$sdriskC,
          CapRC  =tmp2$riskC,
          CapRCsd=tmp2$sdriskC,
          deltaRC  =tmp2$riskC-tmp1$riskC)
          if(i==1) tmpOUT_tmp<-tmpOUTtmp
          if(i>1) tmpOUT_tmp<-rbind(tmpOUT_tmp,tmpOUTtmp)
        }
      }
    }
    i<-0
    ii<-ii+1
    if(ii==1){
      tmpOUT<-tmpOUT_tmp
    }else{
       tmpOUT<-cbind(tmpOUT, tmpOUT_tmp)
    }
  }
      


write.csv(tmpOUT,file="RiskValue.csv")


  #-------------------------------------
  #   Figure 4: PLOT delta
  #-------------------------------------  
  A1B_n_sim           <-  grep("A1B",simnames)
  bio_n_sim           <-  grep("bio",simnames)
  rcp45_n_sim         <-  grep("rcp45",simnames)
  rcp85_n_sim         <-  grep("rcp85",simnames)
  rcp85NoBio_n_sim    <-  setdiff(rcp85_n_sim,bio_n_sim)

  graphics.off()
      GGplot_aclimCEATTLE_delta(deltaIN=TRUE,h=4.75*1.3,
        w=4.75*1.3,ydiv=1,ylimm_up=c(100,100,100),ylimm_dwn=c(-100,-100,-100),prob= c(.05,.50,.95),
        plot_marginIN=c(-15,5,-10,5),alpha=c(20,20),lgnpos= "bottom",scalesIN="fixed")
  
  if(update.figs) ggsave(file=paste0("Figures/Fig02_deltaB0_ts.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

  graphics.off()
      GGplot_aclimCEATTLE_delta(deltaIN=TRUE,h=4.75*1.3,w=4.75*1.3,ydiv=1,
        ylimm_up=c(100,100,100),ylimm_dwn=c(-200,-200,-200),
        plot_marginIN=c(-15,5,-10,5),alpha=c(0,0),lwdd=c(.7,.3),plotpersist = FALSE,
        coll = coll_use,
        ylabb   = expression(paste(Delta," Catch (%)")),
        xlimmIN = c(2010,2100),
        nmLIST  = list("2 MT cap"="dat_2MT_219_CENaivecf1_2_5_13","no cap"="dat_219_CENaivecf_2_5_12"),
        valLIST = list(valIn1="Catch_total_biom", valIn2="Catch_total_biom"),
        datLIST = list(dat1=B_219_CENaivecf_2_5_12_mc,dat2=B_219_CENaivecf_2_5_12_mc),
        lgnpos= "bottom",scalesIN="fixed")

  graphics.off()
      GGplot_aclimCEATTLE_delta(deltaIN=F,h=4.75*1.3,w=4.75*1.3,ydiv=1,
       ylimm_up    = c(20,2,1.5),ylimm_dwn=c(0,0,0),
       #ylimm_up=c(100,100,100),ylimm_dwn=c(-200,-200,-200),
        plot_marginIN=c(-15,5,-10,5),alpha=c(0,0,0),lwdd=c(.7,.3,.1),plotpersist = FALSE,
        coll = coll_use,
        ltyy    = c("solid","solid","solid"),
        ylabb   = expression(paste(Delta," Catch (%)")),
        xlimmIN = c(2010,2100),
        nmLIST  = list("2 MT cap"="dat_2MT_219_CENaivecf1_2_5_13","no cap"="dat_219_CENaivecf_2_5_12", "no HCR"="dat_2_5_3"),
        valLIST = list(valIn1="Catch_total_biom", valIn2="Catch_total_biom",valIn3="Catch_total_biom"),
        datLIST = list(dat1=B_219_CENaivecf_2_5_12_mc,dat2=B_219_CENaivecf_2_5_12_mc,dat3=B_219_CENaivecf_2_5_12_mc),
        lgnpos= "bottom",scalesIN="free_y")
 if(update.figs) ggsave(file=paste0("Figures/Fig04_deltaC_ts.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)
  


 GGplot_aclimCEATTLE_delta(deltaIN=T,h=4.75*1.3,w=4.75*1.3,ydiv=1,
       ylimm_up    = c(20,2,1.5),ylimm_dwn=c(0,0,0),
       #ylimm_up=c(100,100,100),ylimm_dwn=c(-200,-200,-200),
        plot_marginIN=c(-15,5,-10,5),alpha=c(0,0,0),lwdd=c(.7,.3,.1),plotpersist = FALSE,
        coll = coll_use,
        ltyy    = c("solid","solid","solid"),
        ylabb   = expression(paste(Delta,"(%)")),
        xlimmIN = c(2010,2100),
        nmLIST  = list(
          "M2 "="dat_219_CENaivecf_2_5_12",
          "ration"="dat_219_CENaivecf_2_5_12"),
        valLIST = list(valIn1="M2", valIn2="Ration_g_fish"),
        datLIST = list(dat1=M2_219_CENaivecf_2_5_12_mc,dat2=ration_219_CENaivecf_2_5_12_mc),
        lgnpos= "bottom",scalesIN="fixed")

  
  if(update.figs) ggsave(file=paste0("Figures/Fig07_deltaC_M2_12.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

 GGplot_aclimCEATTLE_delta(deltaIN=T,h=4.75*1.3,w=4.75*1.3,ydiv=1,
       ylimm_up    = c(20,2,1.5),ylimm_dwn=c(0,0,0),
       #ylimm_up=c(100,100,100),ylimm_dwn=c(-200,-200,-200),
        plot_marginIN=c(-15,5,-10,5),alpha=c(0,0,0),lwdd=c(.7,.3,.1),plotpersist = FALSE,
        coll = coll_use,
        ltyy    = c("solid","solid","solid"),
        ylabb   = expression(paste(Delta,"(%)")),
        xlimmIN = c(2010,2100),
        nmLIST  = list(
          "M2 "="dat_2MT_219_CENaivecf1_2_5_13",
          "ration"="dat_2MT_219_CENaivecf1_2_5_13"),
        valLIST = list(valIn1="M2", valIn2="Ration_g_fish"),
        datLIST = list(dat1=M2_2MT_219_CENaivecf1_2_5_13_mc,dat2=ration_2MT_219_CENaivecf1_2_5_13_mc),
        lgnpos= "bottom",scalesIN="fixed")

  
  if(update.figs) ggsave(file=paste0("Figures/Fig07_deltaC_M2_13.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)
  
graphics.off()

dev.new(height=3*1.3,width=4.75*1.3)
PLOT_THRESHOLD(
  dataIN_1=tmpall13_1,
  dataIN_2=tmpall13_2,
  dataIN_3=tmpall13_3,
  firstdiff=F,
  ntemps=3,
  multIN=10,
  ylimmIN =c(-1.5,1.5),
  xlimmIN =c(1,7),
  trndln  = "white",
  trndln2 = Ornjazz[3],
  tipping = Ornjazz[5],
  sizeIN=c(0.1,.3,.75,2))


 if(update.figs) ggsave(file=paste0("Figures/Fig05_2mtcap.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)
all12<-c(tmpall12_1$hat$tmp[tmpall12_1$signif2],
  tmpall12_2$hat$tmp[tmpall12_2$signif2],
    tmpall12_3$hat$tmp[tmpall12_3$signif2])

all13<-c(tmpall13_1$hat$tmp[tmpall13_1$signif2],
  tmpall13_2$hat$tmp[tmpall13_2$signif2],
    tmpall13_3$hat$tmp[tmpall13_3$signif2])
# 1.3-2.9
# 1.4-2.7

dev.new(height=3*1.3,width=4.75*1.3)
#dev.new(height=3*1.3,width=4.75*1.3)
PLOT_THRESHOLD(
  dataIN_1=tmpall12_1,
  dataIN_2=tmpall12_2,
  dataIN_3=tmpall12_3,
  firstdiff=F,
  ntemps=3,
  multIN=10,
  ylimmIN =c(-1.5,1.5),
  xlimmIN =c(.5,7),
  trndln  = "white",
  trndln2 = Ornjazz[3],
  tipping = Ornjazz[5],
  sizeIN=c(0.1,.3,.75,2))


 if(update.figs) ggsave(file=paste0("Figures/Fig05_nocap.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

dev.new(height=4.75*1.3,width=4.75*1.3)
PLOT_THRESHOLD2(
  multIN=10,
  firstdiff=T,
  ntemps=3,
  ylimmIN =c(-1,1.5),
  xlimmIN =c(1,7),
  trndln  = "white",
  trndln2 = Ornjazz[3],
  tipping = Ornjazz[5],
  sizeIN=c(0.1,.3,.75,2))

 if(update.figs) ggsave(file=paste0("Figures/Fig05v2.tiff"), device = "tiff", 
      scale = 1, width = NA, height = NA, units = "in",
      dpi = dpiIN)

PLOT_THRESHOLD(

  dataIN_1=tmpall13_1_20y,
  dataIN_2=tmpall13_2_20y,
  dataIN_3=tmpall13_3_20y,
  firstdiff=F,
  ntemps=3,
    multIN=5,
  ylimmIN =c(-1,1.5),
  xlimmIN =c(1,7),
  trndln  = "white",
  trndln2 = Ornjazz[3],
  tipping = Ornjazz[5],
  sizeIN=c(0.1,.3,.75,2))

dev.new(height=4.75*1.3,width=4.75*1.3)
PLOT_THRESHOLD(
   multIN=5,
  dataIN_1=Btmpall13_1,
  dataIN_2=Btmpall13_2,
  dataIN_3=Btmpall13_3,
  firstdiff=T,
  ntemps=3,
  ylimmIN =c(-1,1.5),
  xlimmIN =c(1,7),
  trndln  = "white",
  trndln2 = Ornjazz[3],
  tipping = Ornjazz[5],
  sizeIN=c(0.1,.3,.75,2))


 tmpall13_1$TempC[which(!is.na(tmpall13_1$sigthresh))]



#-------------------------------------
#   PLOT F and Feffective (Anne's plots)
#-------------------------------------  



#-------------------------------------
#   OLDER PLOTS
#-------------------------------------  

  #-------------------------------------
  #   3. Some plots
  #-------------------------------------    
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
  #-------------------------------------
  #   PLOT TEMP
  #-------------------------------------           
        graphics.off()
        quartz(h=7,w=7)
        par(mar=c(1,1,1,1)) # margins of graph: (bottom,left, top, right)
        par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
        par(oma=c(2,2,1,1))# outer margins of graph: (bottom,left, top, right)
        par(mfrow=c(2,1))
        alphaAll  <-  ceiling(rep(50/(length(probbs)/2),length(probbs)/2))
        LWDD  <-  c(rep(2,10),1,1,2)
        LTYY  <-  c(1,2,ltyall[-2]*0+1)
        plotList  <-  list(c(2,rcp45_n))

      
        plot_aclimTS(dat=allDat,
                     esnmSet=esnm,
                     plotSet=plotList,
                     coll=collIn,
                     esnmCol=c(col2(2)[2],col3(2)[2]),
                     ltyy=LTYY,
                     lwdd=LWDD,
                     w=6,h=4,
                     alpha=alphaAll*0,
                     prob=probbs,
                     add0line=TRUE)
        mtext("a)",font=2,side=3,adj=.98,line=-1.5, outer=FALSE,cex=1.2)
        legend("topleft",c("hindcast",simnames)[c(1,plotList[[1]])],lwd=LWDD[c(1,plotList[[1]])],lty=LTYY[c(1,plotList[[1]])],col=collIn[c(1,plotList[[1]])],box.lty=0)
    
        plotList  <-  list(c(2,rcp85NoBio_n))
        plot_aclimTS(dat=allDat,
                     esnmSet=esnm,
                     plotSet=plotList,
                     coll=collIn,
                     esnmCol=c(col2(2)[2],col3(2)[2]),
                     ltyy=LTYY,
                     lwdd=LWDD,
                     w=6,h=4,
                     alpha=alphaAll*0,
                     prob=probbs,
                     add0line=TRUE)
        mtext("b)",font=2,side=3,adj=.98,line=-1.5, outer=FALSE,cex=1.2)
        legend("topleft",c("hindcast",simnames)[c(1,plotList[[1]])],lwd=LWDD[c(1,plotList[[1]])],lty=LTYY[c(1,plotList[[1]])],col=collIn[c(1,plotList[[1]])],box.lty=0)
        mtext("Bottom Temperature",font=2,side=2,line=.5, outer=TRUE,cex=1.6)
        
        if(update.figs) quartz.save(file="Figures/BottomTemp_horz.jpg",type="jpg",dpi=500)
        
      
  #-------------------------------------
  #   PLOT BO conceptual projection target
  #------------------------------------- 
    
      #compare spp X 2_5_3
      graphics.off()
      quartz(h=4,w=5)
      par(mar=c(1,2,1,1)) # margins of graph: (bottom,left, top, right)
      par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
      par(oma=c(2,2,1,1))# outer margins of graph: (bottom,left, top, right)
      par(mfrow=c(1,1))
      layout.show(1)
      alphaAll  <-  ceiling(rep(50/(length(probbs)/2),length(probbs)/2))
      # plotListALL  <-  list()
      # plotListALL[[1]]  <-  list(c(2,rcp45_n))
      # plotListALL[[2]]  <-  list(c(2,rcp85NoBio_n))
      plotListALL  <-  list()
      plotListALL[[1]]  <-  list(c(2))
      names(plotListALL)  <-  c("RCP45","RCP85")[1]
      sp  <-  1;j  <-  1
          cmult  <-  c(0,0)
          cmult[j]  <-  0
          plot_aclimCEATTLE(
            dat=B0_0_5_3,
            sp=sp,
            esnmSet=esnm,
            plotSet=plotListALL[[j]],
            coll=c("black",colors()[320]),
            esnmCol=c(col2(2)[2],col3(2)[2]),
            ltyy=ltyall*2,
            lwdd=lwdall,
            w=6,h=4,
            alpha=alphaAll*cmult,
            prob=probbs,
            add0line=TRUE,alphaLine=225)
      
          plot_aclimCEATTLE(
            dat=B_0_5_3,
            sp=sp,
            esnmSet=esnm,
            plotSet=plotListALL[[j]],
            coll=rep("black",2),
            esnmCol=c(col2(2)[2],col3(2)[2]),
            ltyy=ltyall,
            lwdd=lwdall*0+rep(2,13),
            w=6,h=4,
            alpha=alphaAll*cmult,
            prob=probbs,
            add0line=TRUE,
            addNew=FALSE)

         abline(h=rev(tail(B0_0_5_3[[sp]]$persistence))[1]*c(.35,.40),lwd=2,lty=c(2,3))
         # abline(h=rev(tail(B0_0_5_13[[sp]]$persistence))[1]*c(.35),lwd=2,lty=c(3))
       
          legend("topright",expression('B'[0],"B"["F"["target"]]),
                 lwd=2,lty=c(2,1),col=c(colors()[320],"black"),box.lty=0)
          text(2095,rev(tail(B0_0_5_3[[sp]]$persistence))[1]*.30,expression("B"["35%"]))
          text(2080,rev(tail(B0_0_5_3[[sp]]$persistence))[1]*.45,expression("B"["40%"]))
      
      if(update.figs) quartz.save(file="Figures/HCR_method.jpg",type="jpg",dpi=500)     
      
  #-------------------------------------
  #   PLOT all BO projections
  #------------------------------------- 
        
  graphics.off()
plot_ts  <-  function(nm1="dat_2MT_019_CENaivecf1_0_5_13",nm2="dat_019_CENaivecf_0_5_12",valIn1="SSB0_total_biom",valIn2="SSB0_total_biom",saveIT=FALSE,plab=paste0(letters[1:6],")"),padj=.01,pline=-2,
                            alphaAll=ceiling(rep(50/(length(probbs)/2),length(probbs)/2)),ydivv=1e6,
                            splgnd=1,lgndLoc="topright",collInn=collIn,ylabb="Spawning Biomass (million tons)",
                            lwddIn=c(rep(2,10),1,1,2),
                            ltyyIn=c(1,2,ltyall[-2]*0+1)){

            eval(parse(text=paste0("dat1  <-  ",nm1)))
            eval(parse(text=paste0("dat2  <-  ",nm2)))
            
            dat1  <-  grabDat(datIn=dat1,valIn=valIn1)
            dat2  <-  grabDat(datIn=dat2,valIn=valIn2)
            
            quartz(h=8,w=7)
            par(mar=c(1,3,3,1)) # margins of graph: (bottom,left, top, right)
            par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
            par(oma=c(2,2,1,1))# outer margins of graph: (bottom,left, top, right)
            par(mfrow=c(2,1))
            layout(cbind(c(1,3,5),c(2,4,6 )))
            layout.show(6)
           
            plotListALL  <-  list()
            plotListALL[[1]]  <-  list(c(2,rcp45_n))
            plotListALL[[2]]  <-  list(c(2,rcp85NoBio_n))
            names(plotListALL)  <-  c("RCP45","RCP85")
            ii  <-  0
            for(sp in 1:3){
              for(j in 1:2){
                ii  <-  ii+1
                cmult  <-  c(0,0)
                cmult[j]  <-  0
                plot_aclimCEATTLE(
                  dat=dat1,
                  sp=sp,
                  ydiv=ydivv,
                  esnmSet=esnm,
                  plotSet=plotListALL[[j]],
                  coll=collInn,
                  esnmCol=c(col2(2)[2],col3(2)[2]),
                  ltyy=ltyyIn,
                  lwdd=lwddIn,
                  w=6,h=4,
                  alpha=alphaAll*cmult,
                  prob=probbs,
                  add0line=TRUE,alphaLine=80)
                mtext(plab[ii],side=3,adj=padj,font=2,line=pline)  
                
                plot_aclimCEATTLE(
                  dat=dat2,
                  sp=sp,
                  esnmSet=esnm,
                  plotSet=plotListALL[[j]],
                  coll=collInn,
                  esnmCol=c(col2(2)[2],col3(2)[2]),
                  ltyy=ltyyIn,
                  lwdd=lwddIn,
                  w=6,h=4,
                  alpha=alphaAll*cmult,
                  prob=probbs,
                  add0line=TRUE,
                  addNew=FALSE)
                if(sp==splgnd){
                  legend(lgndLoc,simnames[c(plotListALL[[j]][[1]])-1],
                         col=collInn[c(plotListALL[[j]][[1]])],
                         lwd=lwddIn[c(plotListALL[[j]][[1]])],
                         lty=ltyyIn[c(plotListALL[[j]][[1]])],box.lty=0)
                }
              }
            }
            mtext(side=3,outer=T,paste0(nm1,"VS",nm2), line=-.5) 
            mtext(outer=T,side=2,ylabb,line=0,font=2)
            
            if(saveIT) quartz.save(file=paste0("Figures/",nm1,"VS",nm2,".jpg"),type="jpg",dpi=500)
}  
          
  plot_ts_mcmc  <-  function(dat1=B0_2MT_219_CENaivecf1_2_5_13_mc,
                    dat2=B_2MT_219_CENaivecf1_2_5_13_mc,
                    plab=paste0(letters[1:6],")"),
                    padj=.01,
                    pline=-2,
                    probIN=c(.25,.50,.75),
                    saveIT=TRUE,
                    alphaAll=ceiling(rep(50/(length(probbs)/2),length(probbs)/2)),
                    ydivv=1e6,
                    splgnd=1,lgndLoc="topright",collInn=collIn,
                    ylabb="Spawning Biomass (million tons)",
                    lwddIn=c(rep(2,10),1,1,2),
                    ltyyIn=c(1,2,ltyall[-2]*0+1)){ 
        
        quartz(h=8,w=7)
        par(mar=c(1,3,3,1)) # margins of graph: (bottom,left, top, right)
        par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
        par(oma=c(2,2,1,1))# outer margins of graph: (bottom,left, top, right)
        par(mfrow=c(2,1))
        layout(cbind(c(1,3,5),c(2,4,6 )))
        layout.show(6)
        
        plotListALL  <-  list()
        plotListALL[[1]]  <-  list(c(2,rcp45_n))
        plotListALL[[2]]  <-  list(c(2,rcp85NoBio_n))
        names(plotListALL)  <-  c("RCP45","RCP85")
        ii  <-  0
        for(sp in 1:3){
          
          for(j in 1:2){
            ii  <-  ii+1
            cmult  <-  c(0,0)
            cmult[j]  <-  0
            cmult  <-  c(1,1)
            plot_aclimCEATTLEMCMC(
              dat=dat1,
              sp=sp,
              ydiv=ydivv,
              esnmSet=esnm,
              plotSet=plotListALL[[j]],
              coll=collInn,
              esnmCol=c(col2(2)[2],col3(2)[2]),
              ltyy=ltyyIn,
              lwdd=lwddIn,
              w=6,h=4,
              alpha=alphaAll*cmult,
              prob=probbs,
              add0line=TRUE,alphaLine=80)
            mtext(plab[ii],side=3,adj=padj,font=2,line=pline)  
            
            plot_aclimCEATTLEMCMC(
              dat=dat2,
              sp=sp,
              esnmSet=esnm,
              plotSet=plotListALL[[j]],
              coll=collInn,
              esnmCol=c(col2(2)[2],col3(2)[2]),
              ltyy=ltyyIn,
              lwdd=lwddIn,
              w=6,h=4,
              alpha=alphaAll*cmult,
              prob=probbs,
              add0line=TRUE,
              addNew=FALSE)
            if(sp==splgnd){
              legend(lgndLoc,simnames[c(plotListALL[[j]][[1]])-1],
                     col=collInn[c(plotListALL[[j]][[1]])],
                     lwd=lwddIn[c(plotListALL[[j]][[1]])],
                     lty=ltyyIn[c(plotListALL[[j]][[1]])],box.lty=0)
            }
          }
        }
        mtext(side=3,paste0(nm1,"VS",nm2), line=-.5, outer=T) 
        mtext(side=2,ylabb,line=0,outer=T,font=2)
       
    if(saveIT) quartz.save(file=paste0("Figures/",nm1,"VS",nm2,".jpg"),type="jpg",dpi=500)
  }  
  
  plot_ts(nm1="dat_2MT_019_CENaivecf1_0_5_13",
          nm2="dat_019_CENaivecf_0_5_12",
          valIn1="SSB0_total_biom",
          valIn2="SSB0_total_biom",
          saveIT=TRUE,
          plab=paste0(letters[1:6],")"))
  
  plot_ts(nm1="dat_2MT_019_CENaivecf1_0_5_13",
          nm2="dat_019_CENaivecf_0_5_12",
          valIn1="SSB0_total_biom",
          valIn2="SSB0_total_biom",
          saveIT=TRUE,
          plab=paste0(letters[1:6],")"))
  
  # plot_ts_mcmc(dat1=B0_2MT_219_CENaivecf1_2_5_13_mc,
  #         dat2=B_2MT_219_CENaivecf1_2_5_13_mc,
  #         saveIT=TRUE,
  #         plab=paste0(letters[1:6],")"))
  
  plot_ts(nm1="dat_2MT_019_CENaivecf1_0_5_13",
          nm2="dat_2MT_019_CENaivecf1_0_5_13",
          valIn1="SSB0_total_biom",
          valIn2="SSB_total_biom",
          saveIT=TRUE,
          plab=paste0(letters[1:6],")"))
  

  # CE reference points preserve populations - but do it through shutting down the fishery:
  plot_ts(nm1="dat_0_5_3",
          nm2="dat_019_CENaivecf_0_5_12",
          valIn1="SSB_total_biom",
          valIn2="SSB_total_biom",
          saveIT=TRUE,
          plab=paste0(letters[1:6],")"))
  plot_ts(nm1="dat_0_5_3",
          nm2="dat_019_CENaivecf_0_5_12",
          valIn1="Catch_total_biom",
          valIn2="Catch_total_biom",
          saveIT=TRUE,
          plab=paste0(letters[1:6],")"),ylabb="Catch (million tons)")
  
  
  # CE reference points preserve populations - but do it through shutting down the fishery: 
  plot_ts(nm1="dat_019_CENaivecf_0_5_12",
          nm2="dat_2MT_019_CENaivecf1_0_5_13",
          valIn1="SSB_total_biom",
          valIn2="SSB_total_biom",
          saveIT=TRUE,
          plab=paste0(letters[1:6],")"))
  plot_ts(nm1="dat_019_CENaivecf_0_5_12",
          nm2="dat_2MT_019_CENaivecf1_0_5_13",
          valIn1="Catch_total_biom",
          valIn2="Catch_total_biom",
          saveIT=TRUE,
          plab=paste0(letters[1:6],")"),ylabb="Catch (million tons)")
  
  plot_ts(nm1="dat_2MT_019_CENaivecf1_0_5_13",
          nm2="dat_2MT_019_CENaivecf1_0_5_13",
          valIn1="SSB0_total_biom",
          valIn2="SSB0_total_biom",
          saveIT=TRUE,
          plab=paste0(letters[1:6],")"),ylabb="Unfished Spawning Biomass (million tons)")
  
  
  #-------------------------------------
  #   PLOT Catch projections spp X Fx%, F+HCR, F+HCR+cap
  #-------------------------------------  
    # compare 2_5_3, 2_5_12 based on 259, 2_5_13 based on 259, 
  
        # graphics.off()
        # quartz(h=8,w=8)
        # par(mar=c(1,1,1,1)) # margins of graph: (bottom,left, top, right)
        # par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
        # par(oma=c(2,2,1,1))# outer margins of graph: (bottom,left, top, right)
        # par(mfrow=c(2,1))
        # layout(cbind(c(1,3,5),c(2,4,6 )))
        # layout.show(6)
        # alphaAll  <-  ceiling(rep(50/(length(probbs)/2),length(probbs)/2))
        # plotListALL  <-  list()
        # plotListALL[[1]]  <-  list(c(2,rcp45_n))
        # plotListALL[[2]]  <-  list(c(2,rcp85NoBio_n))
        # names(plotListALL)  <-  c("RCP45","RCP85")
        # ylimmUP  <-  c(2e6,.25e6,.06e6)
        # #col2  <-  col1
        # for(sp in 1:3){
        #   for(j in 1:2){
        # 
        #     plot_aclimCEATTLE(
        #       dat=C_2_5_3,
        #       sp=sp,
        #       esnmSet=esnm[j],
        #       plotSet=plotListALL[[j]],
        #       coll=collIn,
        #       esnmCol=c(col2(3)[1],col3(2)[2])[1],
        #       ltyy=ltyall,
        #       lwdd=lwdall*c(1,1,rep(0,11)),
        #       w=6,h=4,
        #       alpha=alphaAll*c(0,5),
        #       prob=probbs,
        #       add0line=TRUE,
        #       alphaLine=100,
        #       addNew=TRUE,
        #       ylim1=c(0,ylimmUP[sp]))
        #     
        #     plot_aclimCEATTLE(
        #       dat=C_2_5_12,
        #       sp=sp,
        #       esnmSet=esnm[j],
        #       plotSet=plotListALL[[j]],
        #       coll=collIn,
        #       esnmCol=c(col2(3)[2],col3(2)[2])[1],
        #       ltyy=ltyall,
        #       lwdd=lwdall*c(1,1,rep(0,11)),
        #       w=6,h=4,
        #       alpha=alphaAll*c(0,5),
        #       prob=probbs,
        #       add0line=TRUE,alphaLine=100,addNew=FALSE)
        #    
        #     
        #     plot_aclimCEATTLE(
        #       dat=C_2_5_13,
        #       sp=sp,
        #       esnmSet=esnm[j],
        #       plotSet=plotListALL[[j]],
        #       coll=collIn,
        #       esnmCol=c(col2(3)[3],col3(2)[2])[1],
        #       ltyy=ltyall,
        #       lwdd=lwdall*c(1,1,rep(0,11)),
        #       w=6,h=4,
        #       alpha=alphaAll*c(0,6),
        #       prob=probbs,
        #       add0line=TRUE,alphaLine=100,addNew=FALSE)
        #   }
        # }
        # 
  #-------------------------------------
  # #   PLOT cumulative catch projections
  # #-------------------------------------  
  #   # compare 2_5_3, 2_5_12 based on 259, 2_5_13 based on 259, 
  #       graphics.off()
  #       quartz(h=8,w=8)
  #       par(mar=c(1,1,1,1)) # margins of graph: (bottom,left, top, right)
  #       par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
  #       par(oma=c(2,2,1,1))# outer margins of graph: (bottom,left, top, right)
  #       par(mfrow=c(2,1))
  #       layout(cbind(c(1,3,5),c(2,4,6 )))
  #       layout.show(6)
  #       alphaAll  <-  ceiling(rep(50/(length(probbs)/2),length(probbs)/2))
  #       plotListALL  <-  list()
  #       plotListALL[[1]]  <-  list(c(2,rcp45_n))
  #       plotListALL[[2]]  <-  list(c(2,rcp85NoBio_n))
  #       names(plotListALL)  <-  c("RCP45","RCP85")
  #       ylimmUP  <-  c(2e6,.25e6,.06e6)
  #       #col2  <-  col1
  #       
  #       for(sp in 1:3){
  #         for(j in 1:2){
  #           
  #           plot_aclimCEATTLE(
  #             dat=C_2_5_3,
  #             sp=sp,
  #             esnmSet=esnm[j],
  #             plotSet=plotListALL[[j]],
  #             coll=collIn,
  #             esnmCol=c(col2(3)[1],col3(2)[2])[1],
  #             ltyy=ltyall,
  #             lwdd=lwdall*c(1,1,rep(0,11)),
  #             w=6,h=4,
  #             alpha=alphaAll*c(0,5),
  #             prob=probbs,
  #             add0line=TRUE,
  #             alphaLine=100,
  #             addNew=TRUE,
  #             ylim1=c(0,ylimmUP[sp]))
  #           
  #           plot_aclimCEATTLE(
  #             dat=C_2_5_12,
  #             sp=sp,
  #             esnmSet=esnm[j],
  #             plotSet=plotListALL[[j]],
  #             coll=collIn,
  #             esnmCol=c(col2(3)[2],col3(2)[2])[1],
  #             ltyy=ltyall,
  #             lwdd=lwdall*c(1,1,rep(0,11)),
  #             w=6,h=4,
  #             alpha=alphaAll*c(0,5),
  #             prob=probbs,
  #             add0line=TRUE,alphaLine=100,addNew=FALSE)
  #           
  #           
  #           plot_aclimCEATTLE(
  #             dat=C_2_5_13,
  #             sp=sp,
  #             esnmSet=esnm[j],
  #             plotSet=plotListALL[[j]],
  #             coll=collIn,
  #             esnmCol=c(col2(3)[3],col3(2)[2])[1],
  #             ltyy=ltyall,
  #             lwdd=lwdall*c(1,1,rep(0,11)),
  #             w=6,h=4,
  #             alpha=alphaAll*c(0,6),
  #             prob=probbs,
  #             add0line=TRUE,alphaLine=100,addNew=FALSE)
  #         }
  #       }
  # 
  # #-------------------------------------        
  # # PLOT RISK
  #------------------------------------ 
  #       # probability of 10% decline from (2000-2015 levels) by:
  #       # 2025(2017-2025)
  #       # 2025-2050
  #       # 2051-2075
  #       # 2076-2100
  # rcp<-"rcp45"
  # mode<-"mode"
  # type<-"10% decline"
  # timeframe<-unique(timeframe)

  #       riskIN
  #       getRisk<-function(riskIN=risk12,spIN=1,rcpIN = "rcp45",modeIN="MSM",
  #         typeIN=unique(risk12$type)[1],
  #         timeframeIN=unique(risk12$timeframe)[1]){
  #         riskIN[
  #         riskIN$sp%in%spIN&
  #         riskIN$type%in%typeIN&
  #         riskIN$rcp%in%rcpIN&
  #         riskIN$mode%in%modeIN&
  #         riskIN$timeframe%in%timeframeIN, ] 
  #       }
  #       riskMSM_12<-getRisk(riskIN=risk12,spIN=1,rcpIN = "rcp45",modeIN="MSM",typeIN=unique(risk12$type),timeframeIN=unique(risk12$timeframe)[1])
  #       riskMSM_13<-getRisk(riskIN=risk1,spIN=1,rcpIN = "rcp45",modeIN="MSM",typeIN=unique(risk12$type),timeframeIN=unique(risk12$timeframe)[1])
        

  #       plotRisk(figname="Figures/PlotRisk1_SSM10.jpg", lim=10,y1risk=riskB1,y2risk=riskB2,xrisk1=risk1,xrisk2=risk2)
  #       plotRisk(figname="Figures/PlotRisk1_SSM50.jpg", lim=50, y1risk=riskB1_50,y2risk=riskB2_50,xrisk1=risk1_50,xrisk2=risk2_50)
  #       plotRisk(figname="Figures/PlotRisk1_SSM80.jpg", lim=80,y1risk=riskB1_80,y2risk=riskB2_80,xrisk1=risk1_80,xrisk2=risk2_80)

       
  #       coll  <-  rev(col2(4))
  #       ylimm  <-  c(-100,10)
  #       ylimm  <-  c(0,100)
        
  #       plot(as.numeric(risk1MSM[1,]),as.numeric(riskB1MSM[1,]),ylim=ylimm,xlim=ylimm,pch=3,col=coll)
  #       points(as.numeric(risk2MSM[1,]),as.numeric(riskB2MSM[1,]),ylim=ylimm,xlim=ylimm,pch=16,col=coll)
  #       arrows(x0=as.numeric(risk1MSM[1,]),x1=as.numeric(risk2MSM[1,]),y0=as.numeric(riskB1MSM[1,]),y1=as.numeric(riskB2MSM[1,]),length = 0.1,lwd=3,col=coll)
  #       legend("bottomright",col=c(coll,rep("black",2)),lwd=c(rep(2,length(risk1MSM[1,])),-1,-1),pch=c(rep(-1,length(risk1MSM[1,])),3,16),c(colnames(risk1MSM),"No cap", "2 MT cap"),box.lty=0)
  #       mtext(side=3,font=2,"a) RCP 4.5",outer=F,line=-1.1,adj=.01)
        
  #       plot(as.numeric(risk1MSM[2,]),as.numeric(riskB1MSM[2,]),ylim=ylimm,xlim=ylimm,pch=3,col=coll)
  #       points(as.numeric(risk2MSM[2,]),as.numeric(riskB2MSM[2,]),ylim=ylimm,xlim=ylimm,pch=16,col=coll)
  #       arrows(x0=as.numeric(risk1MSM[2,]),x1=as.numeric(risk2MSM[2,]),y0=as.numeric(riskB1MSM[2,]),y1=as.numeric(riskB2MSM[2,]),length = 0.1,lwd=3,col=coll)
        
  #       mtext(side=2,font=2,"Risk of 10% Biomass decline",outer=T,line=1)
  #       mtext(side=1,font=2,"Risk of 10% Catch decline",outer=T,line=1)
  #       mtext(side=3,font=2,"b) RCP 8.5",outer=F,line=-1.1,adj=.01)
  #       if(update.figs) quartz.save(file="Figures/PlotRisk1_MSM.jpg",type="jpg",dpi=500)  

  #     quartz(w=8,h=4)
  #       par(mar=c(1,1,1,1)) # margins of graph: (bottom,left, top, right)
  #       par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
  #       par(oma=c(2,2,1,1))# outer margins of graph: (bottom,left, top, right)
 
  #       par(mfrow=c(1,2))   
        
  #       plot(as.numeric(risk1[1,]),as.numeric(riskB1[1,]),ylim=ylimm,xlim=ylimm,pch=3,col=coll)
  #       points(as.numeric(risk2[1,]),as.numeric(riskB2[1,]),ylim=ylimm,xlim=ylimm,pch=5,col=coll)
  #       arrows(x0=as.numeric(risk1[1,]),x1=as.numeric(risk2[1,]),y0=as.numeric(riskB1[1,]),y1=as.numeric(riskB2[1,]),length = 0.1,lwd=2,lty=2,col=coll)
        
  #       points(as.numeric(risk1MSM[1,]),as.numeric(riskB1MSM[1,]),ylim=ylimm,xlim=ylimm,pch=3,col=coll)
  #       points(as.numeric(risk2MSM[1,]),as.numeric(riskB2MSM[1,]),ylim=ylimm,xlim=ylimm,pch=16,col=coll)
  #       arrows(x0=as.numeric(risk1MSM[1,]),x1=as.numeric(risk2MSM[1,]),y0=as.numeric(riskB1MSM[1,]),y1=as.numeric(riskB2MSM[1,]),length = 0.1,lwd=3,col=coll)
        
  #       legend("bottomright",col=c(coll,rep("black",2)),
  #         lwd=c(rep(2,length(risk1MSM[1,])),-1,-1),
  #         pch=c(rep(-1,length(risk1MSM[1,])),3,16),c(colnames(risk1MSM),"No cap", "2 MT cap"),box.lty=0)
  #       mtext(side=3,font=2,"a) RCP 4.5",outer=F,line=-1.1,adj=.01)
        
  #       plot(as.numeric(risk1[2,]),as.numeric(riskB1[2,]),ylim=ylimm,xlim=ylimm,pch=3,col=coll)
  #       points(as.numeric(risk2[2,]),as.numeric(riskB2[2,]),ylim=ylimm,xlim=ylimm,pch=5,col=coll)
  #       arrows(x0=as.numeric(risk1[2,]),x1=as.numeric(risk2[2,]),y0=as.numeric(riskB1[2,]),y1=as.numeric(riskB2[2,]),length = 0.1,lwd=2,lty=2,col=coll)
        
  #       points(as.numeric(risk1MSM[2,]),as.numeric(riskB1MSM[2,]),ylim=ylimm,xlim=ylimm,pch=3,col=coll)
  #       points(as.numeric(risk2MSM[2,]),as.numeric(riskB2MSM[2,]),ylim=ylimm,xlim=ylimm,pch=16,col=coll)
  #       arrows(x0=as.numeric(risk1MSM[2,]),x1=as.numeric(risk2MSM[2,]),y0=as.numeric(riskB1MSM[2,]),y1=as.numeric(riskB2MSM[2,]),length = 0.1,lwd=3,col=coll)
        
  #       mtext(side=2,font=2,"Risk of 10% Biomass decline",outer=T,line=1)
  #       mtext(side=1,font=2,"Risk of 10% Catch decline",outer=T,line=1)
  #       mtext(side=3,font=2,"b) RCP 8.5",outer=F,line=-1.1,adj=.01)
  #        legend("bottomright",col=rep("black",2),lwd=rep(2,2),lty=c(1,2),c("Single-species", "Multi-species"),box.lty=0)

       
      
  #       if(update.figs) quartz.save(file="Figures/PlotRisk1_both.jpg",type="jpg",dpi=500)     
        

  #       #_______________
  #       #----------------
        
  #       graphics.off()
  #       quartz(h=4,w=5)
  #       par(mar=c(1,1,1,1)) # margins of graph: (bottom,left, top, right)
  #       par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
  #       par(oma=c(2,2,1,1))# outer margins of graph: (bottom,left, top, right)
  #       par(mfrow=c(2,1))
        
  #       barplot(t(t(riskB1)),beside=T,col=col2(nlist*2)[c(1,nlist+1)],las=2,main="2_5_12",horiz = F,ylim=c(0,100))
  #       risk<-risk1MSM
  #       #delta  <-  tmpC  <-  C_2_5_13[[sp]]
  #       delta  <-  tmpC  <-  grabDat(dat_2MT_219_CENaivecf1_2_5_13,valIn="Catch_total_biom")[[sp]]
  #       for(esm in 1:nlist){
  #         cc  <-  esmlist[[esm]]
  #         for(y in 1:(nbin-1)){
  #           rr  <-  which(tmpC$Year>Yrbin[y]&tmpC$Year<=Yrbin[y+1] )
  #           delta[,-1]  <-   round((tmpC[,-1]- tmpC$persistence)/ tmpC$persistence,5)*100
  #           risk[esm,y]  <-  100*round(length(which(unlist(delta[rr,cc])<=(lim[l])))/length(unlist(delta[rr,cc])),4)
  #         }
  #       }
  #       risk1  <-  risk1MSM
  #       risk2  <-  risk2MSM
  #       barplot(t(t(risk2)),beside=T,col=col2(nlist*2)[c(1,nlist+1)],las=2,main="2_5_13",horiz = F,ylim=c(0,100))
        
  #       barplot(t(t(rbind(risk1[1,],risk2[1,]))),beside=T,main="rcp45",col=col2(nlist*2)[c(1,nlist+1)],las=2,horiz = F,ylim=c(0,100))
  #       legend("topleft",col=col2(nlist*2)[c(1,nlist+1)],c("no Cap","2MT cap"),box.lty=0,fill=col2(nlist*2)[c(1,nlist+1)])
  #       barplot(t(t(rbind(risk1[2,],risk2[2,]))),beside=T,main="rcp85",col=col2(nlist*2)[c(1,nlist+1)],las=2,horiz = F,ylim=c(0,100))
  #       if(update.figs) quartz.save(file="Figures/PlotRisk.jpg",type="jpg",dpi=500)     
        
        #-------------------------------------        
        # CHANGE IN POLLOCK BIOMASS
        #------------------------------------      

        
       
        
        dd  <-  ceattle_219_2_5_13
        dd2  <-  ceattle_219_2_5_12
        dd3  <-  ceattle_2_5_15
        dd4  <-  ceattle_219_2_5_58
        #dd2  <-  ceattle_2_1_3
        s  <-  1;rr  <-  which(dd$species==s)
        pp  <-  which(names(dd)=="SSB")  #catch, SSB, SSB0, Frate, rec
        # pp  <-  which(names(dd)=="catch")  #catch, SSB, SSB0, Frate, rec
        graphics.off()
        quartz(h=8,w=7)
        par(mar=c(1,3,3,1)) # margins of graph: (bottom,left, top, right)
        par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
        par(oma=c(2,2,1,1))# outer margins of graph: (bottom,left, top, right)
        par(mfrow=c(2,1))
        layout(cbind(c(1,2,3),c(4,5,6 )))
        layout.show(6)
        A1B_n
        rcp85_n
        ii  <-  0
        for(i in (c(rcp45_n,rcp85NoBio_n)-1)){
          ii  <-  ii+1
          plot(dd$year,dd[[pp]][rr,][i,],type="l",ylim=c(0,1.2*max(dd[[pp]][rr,],dd[[pp]][rr,])),lwd=2,ylab="Spawning Biomass")
          
          mtext(side=3,line=-1.5,paste0(letters[1:6][ii],") ",simnames[i]),adj=.01,cex=.8,font=2)
          #lines(dd3$year,dd3[[pp]][rr,][i,],lwd=1,lty=1,col="red")
          lines(dd2$year,dd2[[pp]][rr,][i,])
          lines(dd$year,dd[[pp]][rr,][1,],lwd=2,lty=2)
         # lines(dd4$year,dd4[[pp]][rr,][i,],lwd=1,lty=1,col="red")
          
          if(ii==1){
            legend("topright",lwd=c(2,2,1),c("Persistence","2 MT cap","No cap"),lty=c(2,1,1),box.lty=0)
          }
         
        }
        
        
        if(update.figs) quartz.save(file="Figures/B12vsB13.jpg",type="jpg",dpi=500)     
        
        layout.show(6)
        pp  <-  which(names(dd)=="catch")
        A1B_n
        rcp85_n
        ii  <-  0
        for(i in (c(rcp45_n,rcp85NoBio_n)-1)){
          ii  <-  ii+1
          plot(dd$year,dd[[pp]][rr,][i,],type="l",ylim=c(0,1.2*max(dd[[pp]][rr,],dd[[pp]][rr,])),lwd=2,ylab="Catch")
          
          mtext(side=3,line=-1.5,paste0(letters[1:6][ii],") ",simnames[i]),adj=.01,cex=.8,font=2)
          lines(dd2$year,dd2[[pp]][rr,][i,])
          lines(dd$year,dd[[pp]][rr,][1,],lwd=2,lty=2)
          #lines(dd4$year,dd4[[pp]][rr,][i,],lwd=1,lty=1,col="red")
          
          #lines(dd2$year,dd2[[pp]][rr,][1,],lwd=1,lty=2)
          if(ii==1){
            legend("topright",lwd=c(2,2,1),c("Persistence","2 MT cap","No cap"),lty=c(2,1,1),box.lty=0)
          }
          
        }
        
        if(update.figs) quartz.save(file="Figures/C12vsC13.jpg",type="jpg",dpi=500)     
        
        #-------------------------------------        
        # CHANGE IN CATCH RELATIVE TO PERSISTENCE:
        #------------------------------------      
        
        dd  <-  ceattle_219_2_5_13
        dd2  <-  ceattle_219_2_5_12
        dd3  <-  ceattle_2_5_15
        #dd2  <-  ceattle_2_1_3
       
       # pp  <-  which(names(dd)=="SSB")  #catch, SSB, SSB0, Frate, rec
        pp  <-  which(names(dd)=="catch")
        # pp  <-  which(names(dd)=="catch")  #catch, SSB, SSB0, Frate, rec
        graphics.off()
        quartz(h=8,w=7)
        par(mar=c(1,3,3,1)) # margins of graph: (bottom,left, top, right)
        par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
        par(oma=c(2,2,1,1))# outer margins of graph: (bottom,left, top, right)
        par(mfrow=c(2,1))
        layout(cbind(c(1,3,5),c(2,4,6 )))
        
        layout.show(6)
        plotDelta  <-  function(
          type="catch",
          deltaIt=TRUE,
          esmset=list(rcp45_n,rcp85NoBio_n),
          ylabb=expression(paste(Delta, " catch")),
          ylimm=100*c(-1.2,1.2),
          dd=ceattle_219_2_5_13,
          dd2=ceattle_219_2_5_12,
          dd3=ceattle_2_5_15){
              quartz(h=8,w=7)
              par(mar=c(1,3,3,1)) # margins of graph: (bottom,left, top, right)
              par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
              par(oma=c(2,2,1,1))# outer margins of graph: (bottom,left, top, right)
              par(mfrow=c(2,1))
              layout(cbind(c(1,3,5),c(2,4,6 )))
              pp  <-  which(names(dd)==type)
              nsim  <-  dim(dd[[pp]][rr,])[1]
              iii  <-  0
              calcYLIM  <-  FALSE
              if(is.na(ylimm[1])) calcYLIM  <-  TRUE
              for(s in 1:3){
                iii  <-  1+iii
                rr  <-  which(dd$species==s)
                plotD  <-  plotD2  <-  plotD3  <-  dd[[pp]][rr,]*0
                if(deltaIt){
                  for(i in 1:nsim){
                    plotD[i,]  <-  100*(dd[[pp]][rr,][i,]-dd[[pp]][rr,][1,])/dd[[pp]][rr,][1,]
                    plotD2[i,]  <-  100*(dd2[[pp]][rr,][i,]-dd2[[pp]][rr,][1,])/dd2[[pp]][rr,][1,]
                    plotD3[i,]  <-  100*(dd3[[pp]][rr,][i,]-dd3[[pp]][rr,][1,])/dd3[[pp]][rr,][1,]
                  }
                }else{
                  for(i in 1:nsim){
                    plotD[i,]  <-  dd[[pp]][rr,][i,]
                    plotD2[i,]  <-  dd2[[pp]][rr,][i,]
                    plotD3[i,]  <-  dd3[[pp]][rr,][i,]
                  }
                }
                
                if(calcYLIM) ylimm=c(1.2*min(plotD),1.2*max(plotD))
                for(j in 1:2){
                  ii  <-  0
                  for(i in (esmset[[j]]-1)){
                    ii  <-  ii+1
                    if(ii==1){
                      plot(dd$year,plotD[i,],type="l",ylim=ylimm,lwd=2,xlim=c(2015,2100),ylab=ylabb,col=collIn[i+1])
                      mtext(side=3,line=-1.5,paste0(letters[1:6][iii],") ",c("pollock","P. cod","arrowtooth")[s],c("RCP 4.5","RCP 8.5")[j]),adj=.01,cex=.8,font=2)
                    }
                    abline(h=1)
                    lines(dd$year,plotD[i,],lwd=2,col=collIn[i+1])
                    lines(dd2$year,plotD2[i,],col=collIn[i+1])
                    #lines(dd3$year,plotD3[i,],col=collIn[i+1],lty=3)
                    #lines(dd$year,plotD[1,],lwd=2,lty=2)
                    
                    #lines(dd2$year,dd2[[pp]][rr,][1,],lwd=1,lty=2)
                    if(iii==1&ii==1){
                      legend("topright",lwd=c(2,2,2,2,1),col=c(collIn[esmset[[j]]],rep("gray",2)),c(simnames[esmset[[j]]],"2 MT cap","No cap"),lty=1,box.lty=0)
                    }
                    
                    
                  }
                }
              }
        }
        
        graphics.off()
        plotDelta()
        if(update.figs) quartz.save(file="Figures/deltaC.jpg",type="jpg",dpi=500)     
        plotDelta(deltaIt=F,ylimm=NA)
        if(update.figs) quartz.save(file="Figures/deltaCabs.jpg",type="jpg",dpi=500) 
        
        plotDelta(type="SSB",ylabb=expression(paste(Delta, " spawning biomass")))
        if(update.figs) quartz.save(file="Figures/deltaB.jpg",type="jpg",dpi=500)    
        plotDelta(type="SSB",deltaIt=F,ylimm=NA,ylabb=expression(paste(Delta, " spawning biomass")))
        
        if(update.figs) quartz.save(file="Figures/deltaB_abs.jpg",type="jpg",dpi=500)     
        
  # Do same as risk but for mean catch....      
  #-------------------------------------        
  # HOBDAY HEATWAVE 
  #------------------------------------    
      # Expand timeseries to daily
      ROMSNPZ_output[[1]]$SrvyReplicated
    
      wk  <-  data.frame(
        t=ROMSNPZ_output[["aclim_hindcast"]]$weekly[,"date"],
        temp=ROMSNPZ_output[["aclim_hindcast"]]$weekly[,covars[covn]],
        strat=ROMSNPZ_output[["aclim_hindcast"]]$weekly[,"Stratum"])
      aa  <-  tapply(wk$temp,wk$t,mean,na.rm=T)
      wk  <-  data.frame(t=names(aa),temp=aa)
      wk$t  <-  strptime(wk$t,format="%Y-%m-%d %H:%M:%S")
      nd  <-  wk$t[length(wk$t)]-wk$t[1]
      dd  <-  0:nd
      date  <-  wk$t[1]+(dd)*24*60*60
      ts  <-  data.frame(t=date,temp=NA)
      nn  <-  length(wk$t)
      plot(wk,type="l")
      #for(i in 1:(dim(wk)[1]-1)){
      for(i in 1:(nn-1)){
        start  <-  wk$t[i]-24*60*60
        end  <-  wk$t[i+1]
        rr  <-  which(ts$t>start&ts$t<=end)
        nd  <-  length(rr)
        #plot(c(ts$t[rr[nd]],ts$t[rr[1]]),c(ts$temp[rr[nd]],ts$temp[rr[1]]),pch=16)
        tttmp  <-   data.frame(temp=c(wk$temp[i],wk$temp[i+1]),day=c(0,(nd-1)))
        #tttmp  <-   data.frame(temp=c(ts$temp[rr[1]],ts$temp[rr[nd]]),day=c(0,(nd-1)))
        mm  <-  lm(temp~day,data=tttmp)
        intrp  <-  coef(mm)[1]+(0:(nd-1))*coef(mm)[2]
        #lines(ts$t[rr],intrp)
        ts$temp[rr]  <-  intrp
      }
      
      
      wk_dat  <-  ts
      ts  <-  make_whole(data.frame(wk_dat))
      
      ## DO CLIMATOLOGY : Now do climatalogy on thermal envelope:
      i  <-  1
      start  <-  as.character(ts$t[i])
      end  <-  strptime("2000-01-01",format="%Y-%m-%d")
      #end  <-  as.character(BT_dat$t[dim(ts)[1]])
      #mhw   <-   detect(ts, climatology_start = start,climatology_end = end)
      mhw   <-   detect(ts, climatology_start = start,climatology_end =as.character( ts$t[rev(which(ts$t<end))[1]]))
      mhw   <-   detect(ts, climatology_start = "1970-02-01",climatology_end ="1995-02-01")
      
      mhw$event %>% 
        dplyr::ungroup() %>%
        dplyr::select(event_no, duration, date_start, date_peak, int_mean, int_max, int_cum) %>% 
        dplyr::arrange(-int_cum) %>% 
        head(10)
      event_line(mhw, spread = 400, metric = "int_cum",
                 start_date = "2010-10-01", end_date = "2017-08-30")   
      
      
      
  #-------------------------------------        
  # plot tipping points:
  #------------------------------------     
     
      
      cols<-colIN1(10)
      #ylimm  <-  c(-.4,.4)
      xlimm<-c(-4,8)
      dev.new()
      par(mar=c(1,2,1,1)) # margins of graph: (bottom,left, top, right)
      par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
      par(oma=c(2,2,1,1))# outer margins of graph: (bottom,left, top, right)
      par(mfrow=c(2,3))
       
       for(s in 1:3){
         ylimm  <-  c(-1,1.5)
         eval(parse(text=paste0("tmpD13  <-  tmpall13_",s)))
         plot(tmpD13$deltaC~tmpD13$TempC,pch=16,col=makeTransparent("gray",20),xlim=xlimm,ylim=ylimm)
         abline(h=0)
         
         lines(tmpD13$hat$tmp,tmpD13$hat[,3],col=cols[4])
         lines(tmpD13$hat$tmp,tmpD13$hat[,2],lty=2,col=cols[4])
         lines(tmpD13$hat$tmp,tmpD13$hat[,4],lty=2,col=cols[4])
         lines(tmpD13$hat$tmp,tmpD13$signif,lwd=3,col=cols[4])
         lines(tmpD13$hat$tmp,tmpD13$sigthresh,lwd=5,col=cols[4])
         
          arrw13  <-  data.frame(startX=tmpD13$hat$tmp[tmpD13$thrsh_max2$ix],
                          endX=tmpD13$hat$tmp[tmpD13$thrsh_max2$ix],
                          startY=tmpD13$hat[,3][tmpD13$thrsh_max2$ix],
                          endY=ylimm[1])
         for(ii in 1:dim(arrw)[1])
         arrows(x0=arrw13$startX[ii],x1=arrw13$endX[ii],y0=arrw13$startY[ii],y1=arrw13$endY[ii],lty=2,length = 0.05,lwd=2,col=cols[4])
       }
      for(s in 1:3){
           eval(parse(text=paste0("tmpD13  <-  tmpall13_",s)))
            
          ylimm  <-  c(-.4,.4)
           plot(tmpD13$hat$tmp[-1],tmpD13$fdif1[,3],type="l",,xlim=c(-4,8),ylim=ylimm,col=cols[cc])
            abline(v=meanhistT,lty=2);abline(h=0)
            lines(tmpD13$hat$tmp[-1],tmpD13$fdif1[,2],lty=2,col=cols[cc])
            lines(tmpD13$hat$tmp[-1],tmpD13$fdif1[,4],lty=2,col=cols[cc])
            naVec  <-  naVec2  <-  rep(NA,length(tmpD13$fdif1$mn))
            naVec[which(is.na(tmpD13$signif)==FALSE)]  <-  tmpD13$fdif1$mn[which(is.na(tmpD13$signif)==FALSE)]
            naVec2[which(is.na(tmpD13$sigthresh)==FALSE)]  <-  tmpD13$fdif1$mn[which(is.na(tmpD13$sigthresh)==FALSE)]
            lines(tmpD13$hat$tmp[-1],naVec,lwd=3,col=cols[cc])
            lines(tmpD13$hat$tmp[-1],naVec2,lwd=5,col=cols[cc])
            
       }
       
       
       
       
       
       
       par(mfrow=c(3,2))
      for(dnm in c("tmp45","tmp85")){
       eval(parse(text=paste0("tmpD  <-  ",dnm)))
       eval(parse(text=paste0("tmpD13  <-  ",dnm,"_13")))
       plot(deltaC~TempC,pch=16,col=makeTransparent("gray",20),xlim=xlimm,ylim=ylimm)
       abline(h=0)
       lines(tmpD$hat$tmp,tmpD$hat[,3],col=cols[1])
       lines(tmpD$hat$tmp,tmpD$hat[,2],lty=2,col=cols[1])
       lines(tmpD$hat$tmp,tmpD$hat[,4],lty=2,col=cols[1])
       lines(tmpD$hat$tmp,tmpD$signif,lwd=3,col=cols[1])
       lines(tmpD$hat$tmp,tmpD$sigthresh,lwd=5,col=cols[1])
       
       lines(tmpD13$hat$tmp,tmpD13$hat[,3],col=cols[4])
       lines(tmpD13$hat$tmp,tmpD13$hat[,2],lty=2,col=cols[4])
       lines(tmpD13$hat$tmp,tmpD13$hat[,4],lty=2,col=cols[4])
       lines(tmpD13$hat$tmp,tmpD13$signif,lwd=3,col=cols[4])
       lines(tmpD13$hat$tmp,tmpD13$sigthresh,lwd=5,col=cols[4])
      
       arrw  <-  data.frame(startX=tmpD$hat$tmp[tmpD$thrsh_max2$ix],
                        endX=tmpD$hat$tmp[tmpD$thrsh_max2$ix],
                        startY=tmpD$hat[,3][tmpD$thrsh_max2$ix],
                        endY=ylimm[1])
       arrw13  <-  data.frame(startX=tmpD13$hat$tmp[tmpD13$thrsh_max2$ix],
                        endX=tmpD13$hat$tmp[tmpD13$thrsh_max2$ix],
                        startY=tmpD13$hat[,3][tmpD13$thrsh_max2$ix],
                        endY=ylimm[1])
       for(ii in 1:dim(arrw)[1])
         arrows(x0=arrw$startX[ii],x1=arrw$endX[ii],y0=arrw$startY[ii],y1=arrw$endY[ii],lty=2,length = 0.05,lwd=2,col=cols[1])
       for(ii in 1:dim(arrw)[1])
         arrows(x0=arrw13$startX[ii],x1=arrw13$endX[ii],y0=arrw13$startY[ii],y1=arrw13$endY[ii],lty=2,length = 0.05,lwd=2,col=cols[4])
       
       
      }
       legend("topright",c("no cap","2MT EBM cap"),col=cols[c(1,4)],lwd=2,box.lty=0)
      ylimm  <-  c(-.4,.4)
      for(dnm in c("tmp45","tmp85")){
        eval(parse(text=paste0("tmpD  <-  ",dnm)))
        eval(parse(text=paste0("tmpD13  <-  ",dnm,"_13")))
        cc  <-  1
        plot(tmpD$hat$tmp[-1],tmpD$fdif1[,3],type="l",xlim=xlimm,ylim=ylimm,col=cols[cc])
        abline(v=meanhistT,lty=2);abline(h=0)
        lines(tmpD$hat$tmp[-1],tmpD$fdif1[,2],lty=2,col=cols[cc])
        lines(tmpD$hat$tmp[-1],tmpD$fdif1[,4],lty=2,col=cols[cc])
        naVec  <-  naVec2  <-  rep(NA,length(tmpD$fdif1$mn))
        naVec[which(is.na(tmpD$signif)==FALSE)]  <-  tmpD$fdif1$mn[which(is.na(tmpD$signif)==FALSE)]
        naVec2[which(is.na(tmpD$sigthresh)==FALSE)]  <-  tmpD$fdif1$mn[which(is.na(tmpD$sigthresh)==FALSE)]
        lines(tmpD$hat$tmp[-1],naVec,lwd=3,col=cols[cc])
        lines(tmpD$hat$tmp[-1],naVec2,lwd=5,col=cols[cc])
        cc  <-  4
        lines(tmpD13$hat$tmp[-1],tmpD13$fdif1[,3],type="l",xlim=xlimm,ylim=ylimm,col=cols[cc])
        abline(v=meanhistT,lty=2);abline(h=0)
        lines(tmpD13$hat$tmp[-1],tmpD13$fdif1[,2],lty=2,col=cols[cc])
        lines(tmpD13$hat$tmp[-1],tmpD13$fdif1[,4],lty=2,col=cols[cc])
        naVec  <-  naVec2  <-  rep(NA,length(tmpD13$fdif1$mn))
        naVec[which(is.na(tmpD13$signif)==FALSE)]  <-  tmpD13$fdif1$mn[which(is.na(tmpD13$signif)==FALSE)]
        naVec2[which(is.na(tmpD13$sigthresh)==FALSE)]  <-  tmpD13$fdif1$mn[which(is.na(tmpD13$sigthresh)==FALSE)]
        lines(tmpD13$hat$tmp[-1],naVec,lwd=3,col=cols[cc])
        lines(tmpD13$hat$tmp[-1],naVec2,lwd=5,col=cols[cc])
        
      }
      ylimm  <-  c(-.4,.4)
      for(dnm in c("tmp45","tmp85")){
        eval(parse(text=paste0("tmpD  <-  ",dnm)))
        eval(parse(text=paste0("tmpD13  <-  ",dnm,"_13")))
        cc  <-  1
        plot(tmpD$hat$tmp[-(1:2)],tmpD$fdif2[,3],type="l",xlim=xlimm,ylim=ylimm,col=cols[cc])
        abline(v=meanhistT,lty=2);abline(h=0)
        lines(tmpD$hat$tmp[-(1:2)],tmpD$fdif2[,2],lty=2,col=cols[cc])
        lines(tmpD$hat$tmp[-(1:2)],tmpD$fdif2[,4],lty=2,col=cols[cc])
        naVec  <-  naVec2  <-  rep(NA,length(tmpD$fdif2$mn))
        naVec[which(is.na(tmpD$signif)==FALSE)]  <-  tmpD$fdif2$mn[which(is.na(tmpD$signif)==FALSE)]
        naVec2[which(is.na(tmpD$sigthresh)==FALSE)]  <-  tmpD$fdif2$mn[which(is.na(tmpD$sigthresh)==FALSE)]
        lines(tmpD$hat$tmp[-(1:2)],naVec,lwd=3,col=cols[cc])
        lines(tmpD$hat$tmp[-(1:2)],naVec2,lwd=5,col=cols[cc])
        cc  <-  4
        lines(tmpD13$hat$tmp[-(1:2)],tmpD13$fdif2[,3],type="l",xlim=xlimm,ylim=ylimm,col=cols[cc])
        abline(v=meanhistT,lty=2);abline(h=0)
        lines(tmpD13$hat$tmp[-(1:2)],tmpD13$fdif2[,2],lty=2,col=cols[cc])
        lines(tmpD13$hat$tmp[-(1:2)],tmpD13$fdif2[,4],lty=2,col=cols[cc])
        naVec  <-  naVec2  <-  rep(NA,length(tmpD13$fdif2$mn))
        naVec[which(is.na(tmpD13$signif)==FALSE)]  <-  tmpD13$fdif2$mn[which(is.na(tmpD13$signif)==FALSE)]
        naVec2[which(is.na(tmpD13$sigthresh)==FALSE)]  <-  tmpD13$fdif2$mn[which(is.na(tmpD13$sigthresh)==FALSE)]
        lines(tmpD13$hat$tmp[-(1:2)],naVec,lwd=3,col=cols[cc])
        lines(tmpD13$hat$tmp[-(1:2)],naVec2,lwd=5,col=cols[cc])
      }
      
      #  % percent decline per degree of warming:
      tt  <-  predict(tmpD$GAM,newdata=data.frame(TempC=seq(meanhistT,10,1)))
      tempStep  <-  seq(meanhistT,10,1)
      warming  <-  seq(meanhistT,10,1)-meanhistT
      nn  <-  length(tt)
      prcnt  <-  (tt[2:nn]-tt[1:(nn-1)])/(warming[2:nn]-warming[1:(nn-1)])
      deltaT  <-  (tempStep[2:nn]-tempStep[1])
      barplot(prcnt,names.arg=warming[-1])
      barplot(tt,type="l",names.arg=warming)
      tt-tt[1]
      
 quartz(h=5,w=5)
    par(mar=c(1,1,1,1)) # margins of graph: (bottom,left, top, right)
    par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
    par(oma=c(2,3,2,2))# outer margins of graph: (bottom,left, top, right)
    par(mfrow=c(2,3))      

      B0_target<-data.frame(
        SSM=c(5269933,383809,468654.5),
        MSM=c(3871752, 364800.6,454450.1))

for(sp in 1:3){

  sub<-dat_0_5_12
  sub<-sub[sub$age==1&sub$species==sp,]
  xx<-sub$SSB_total_biom/(0.4*B0_target[sp,1])
  yy<-sub$F/sub$F40
  plot(xx,yy,xlim=c(-.05,10),ylim=c(-.05,1.5))

  sub<-dat_0_5_13

  sub<-sub[sub$age==1&sub$species==sp,]
  xx<-sub$SSB_total_biom/(0.4*B0_target[sp,1])
  yy<-sub$F/sub$F40
  points(xx,yy,col=)

}
for(sp in 1:3){

  sub<-dat_2_5_12
  sub<-sub[sub$age==1&sub$species==sp,]
  xx<-sub$SSB_total_biom/(0.4*B0_target[sp,2])
  yy<-sub$F/sub$F40
  plot(xx,yy,xlim=c(-.05,10),ylim=c(-.05,1.5),pch=4,
    col=rev(heat.colors(20))[round(sub$bottomT_C*2+5)])

  sub<-dat_2_5_13

  sub<-sub[sub$age==1&sub$species==sp,]
  xx<-sub$SSB_total_biom/(0.4*B0_target[sp,2])
  yy<-sub$F/sub$F40
  points(xx,yy,col=rev(heat.colors(20))[round(sub$bottomT_C*2+5)])


}
plot(sub$future_year,sub$bottomT_C,col=rev(heat.colors(20))[round(sub$bottomT_C*2+5)])

  # plots for Alan:

    plot(sub$ABC_total_biom,sub$Catch_total_biom)
    plot(sub$Fabc,sub$F)
    #col1  <-  colorRampPalette(c("blue","gray"))
   
    quartz(h=3,w=9)
    par(mar=c(1,1,1,1)) # margins of graph: (bottom,left, top, right)
    par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
    par(oma=c(2,3,2,2))# outer margins of graph: (bottom,left, top, right)
    par(mfrow=c(1,3))
    for(s in 1:3){
      
      sub  <-  dat[dat$species==s&dat$future_year>40&dat$age==2&dat$Fabc!=0,]
      subsub  <-  sub[sub$Scenario==1,]
      
      plot(subsub$future_year+1979,subsub$F/subsub$Fabc,type="l",ylim=c(0,1.2),ylab="F:Fabc",xlab="Year",
           col=col1(15)[1],lwd=2)
      
      
      for(scn in unique(sub$Scenario)){
        subsub  <-  sub[sub$Scenario==scn,]
        lines(subsub$future_year+1979,subsub$F/subsub$Fabc,col=col1(15)[scn],lwd=2)
        
      }
    }
    quartz.save(file="/Users/kholsman/GitHub/FtoFabc.pdf",type="pdf",dpi=500)




