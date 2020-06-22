## ------------------------------------------------
## K. Holsman 
## June 2020
## Kirstin.holsman@noaa.gov
##
## make_plots.R
## This code creates the figures in Holsman et al. Nat Comm
## 
## ------------------------------------------------

source("R/make.R")       # loads packages, data, setup, etc.


load("data/raw/covariates.Rdata")

#-------------------------------------
# 3. Final figures:
#-------------------------------------
#fig 2: temperature
graphics.off()

simnames <- Scenarios
Years  <- sort(unique(dat_2_5_12$future_year)+start_yr-1)
nYrsTot <- length(Years )
GGplot_aclimTS(dat=reshape2::dcast(covariates%>%filter(Var=="BottomTemp"),t~Scenario)
               ,h=2*1.3,w=4.75*1.3,
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
                          nmLIST  = list("SSB0"="dat_2_5_12","SSB0"="dat_2_5_12"),
                          datLIST = list(dat1=dat_2_5_12_mc,dat2=dat_2_5_12_mc),
                          valLIST = list(valIn1="SSB0_total_biom", valIn2="SSB0_total_biom"),
                          prob= c(.01,.50,.9),plot_marginIN=c(-15,5,-10,5),alpha=c(10,5),
                          lgnpos= "bottom",coll=coll_use)

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
plot_figS6()
if(update.figs) quartz.save(file=paste0("Figures/Figs6_delta.pdf"), type = "pdf", 
                            width = 4.5*1.3, height = 3.25*1.3,dpi = dpiIN)  

