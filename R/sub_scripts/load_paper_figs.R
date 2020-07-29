#load_paper_figs.R



fig2 <-function(){  GGplot_aclimTS(dat=reshape2::dcast(covariates%>%filter(Var=="BottomTemp"),t~Scenario)
                                   ,h=2*1.3,w=4.75*1.3,threshold = 2.1,
                                   ylabb=expression(paste("Bottom temperature",'('^{o},"C)")),
                                   ltyy=c("solid",rep("solid",6)),
                                   subtitle_face="plain",
                                   plotSet=list(c(1,rcp45_n),c(1,rcp85NoBio_n)),
                                   coll=coll_use,tline=2,talpha=.5,
                                   xlabb="",lgnpos= "right",plot_marginIN=c(-10,-1,-10,1))}




#fig 3: delta B

fig3 <- function(){
  GGplot_aclimCEATTLE_delta(h=4.75*1.3,w=4.75*1.3,
                            esm_namesIN=simnames,
                            ageLIST = list(6,6),
                            ylabIN  = c(20,3,1.8),
                            nmLIST  = list("no cap"="dat_2_5_12","no cap2"="dat_2_5_12"),
                            nmLIST_mc = list(dat1=dat_2_5_12_mc,dat2=dat_2_5_12_mc),
                            hLIST   = list("H12_219_CENaivecf","H12_219_CENaivecf"),
                            valLIST = list(valIn1="SSB0_total_biom", valIn2="SSB0_total_biom"),
                            hind    = 1,
                            plotSet = list("RCP 4.5" = c(rcp45_n),"RCP 8.5" = c(rcp85NoBio_n)),
                            prob    = c(.1,.50,.9),
                            showlinetype = FALSE,
                            plot_marginIN=c(-15,5,-10,5),
                            coll    = list(colors()[320],coll_use[2:4],coll_use[5:7]),
                            plot_levels  = simnames[c(1,rcp45_n,rcp85NoBio_n)],
                            alpha   = c(0,15,40),
                            ylabb   = "Unfished spawning biomass (million tons)",
                            lgnpos= "bottom")
}


#fig 4: delta C

fig4 <- function(){GGplot_aclimCEATTLE_delta(h=4.75*1.3,w=4.75*1.3,
                                             ydiv   = 1,
                                             deltaIN = TRUE,
                                             xlimmIN = c(2010,2100),
                                             esm_namesIN=simnames,
                                             ageLIST = list(6,6),
                                             ylabIN  = c(1,1,1),
                                             nmLIST  = list("2 MT cap"="dat_2_5_13","no cap"="dat_2_5_12"),
                                             nmLIST_mc = list(dat1=dat_2_5_13_mc,dat2=dat_2_5_12_mc),
                                             hLIST   = list("H13_2MT_219_CENaivecf1","H12_219_CENaivecf"),
                                             valLIST = list(valIn1="Catch_total_biom", valIn2="Catch_total_biom"),
                                             hind    = 1,
                                             plotSet = list("RCP 4.5" = c(rcp45_n),"RCP 8.5" = c(rcp85NoBio_n)),
                                             prob    = c(.1,.50,.9),
                                             lwdd    = c(.7,.3),
                                             showlinetype = TRUE,
                                             plot_marginIN=c(-15,5,-10,5),
                                             coll    = list(colors()[320],coll_use[2:4],coll_use[5:7]),
                                             plot_levels  = simnames[c(1,rcp45_n,rcp85NoBio_n)],
                                             alpha   = c(100,0,0), 
                                             ylabb   = expression(paste(Delta," Catch (%)")),
                                             lgnpos= "bottom")}



#fig 5: risk

fig5 <- function(col4 = colorRampPalette(c(colIN1(6),"maroon"))){
  
  GGplot_aclimCEATTLE_risk_droppt2(lgnpos= "bottom",angleIN=0,spacer=1.1,spacer_groups=1.6,spacer_lines=.4,itr=100,
                                   colvar  ="type",
                                   between_group = "timeframe",
                                   rev_group=FALSE,rev_lines=FALSE,
                                   barsplit ="hcr",
                                   spIN=1,ylimm=c(-5,105),
                                   arrow_lvl=3,nrowlg  = c(1,1),
                                   h=4*1.3,w=5*1.3,
                                   arrow_start1 = 30,arrow_start2 = 90,arrow_adj1 =.4,
                                   arrow_adj2 =.6,yspace=c(8,-6))
  # 
  # grid.force()
  # # change shape of arrows
  # grid.gedit("segments", gp=gpar(linejoin ='mitre'))
  # # change the shape in legend also
  # grid.gedit("layout", gp=gpar(linejoin ='mitre'))
}



#fig 6: Threshold
fig6 <- function(H=4.75*1.3,W=4.5*1.3){
  dev.new(height=H,width=W)
  PLOT_THRESHOLD2(
    multIN=10,
    firstdiff=T,
    ntemps=3,
    ylimmIN =c(-100,150),
    binwidthIN =  c(0.2, 10),
    xlimmIN =c(1,7),
    trndln  = "white",
    trndln2 = Ornjazz[3],
    tipping = Ornjazz[5],
    sizeIN=c(0.1,.3,1.3,2))
}




# Fig S1: HCR
figS1 <- function(H=4, W= 8){
  dev.new(height=H,width=W)
  GG_HCRplot(h=4,w=8, 
             datIN0 = dat_0_5_3,
             datIN1 = dat_0_5_3,
             futScen="GFDL_rcp45",fontSize=3,yfont=c(2070,2073), Hin = "H3",Rin=as.character(rset))
  
}


# Fig S2: SSB with and without cap
# figS2 <- function(){
#   GGplot_aclimCEATTLE_delta(h=4.75*1.3,w=4.75*1.3,
#                             esm_namesIN=simnames,
#                             ageLIST = list(6,6),
#                             ylabIN  = c(20,3,1.8),
#                             nmLIST  = list("no cap"="dat_2_5_13","no cap2"="dat_2_5_12"),
#                             nmLIST_mc = list(dat1=dat_2_5_13_mc,dat2=dat_2_5_12_mc),
#                             hLIST   = list("H13_2MT_219_CENaivecf1","H12_219_CENaivecf"),
#                             valLIST = list(valIn1="SSB_total_biom", valIn2="SSB_total_biom"),
#                             hind    = 1,
#                             plotSet = list("RCP 4.5" = c(rcp45_n),"RCP 8.5" = c(rcp85NoBio_n)),
#                             prob    = c(.1,.50,.9),
#                             showlinetype = TRUE,
#                             plot_marginIN=c(-15,5,-10,5),
#                             coll    = list(coll_use[1],coll_use[2:4],coll_use[5:7]),
#                             plot_levels  = simnames[c(1,rcp45_n,rcp85NoBio_n)],
#                             alpha   = c(0,0,100),
#                             ylabb   = "Spawning biomass (million tons)",
#                             lgnpos= "bottom")}


figS2 <- function(){GGplot_aclimCEATTLE_delta(h=4.75*1.3,w=4.75*1.3,
                          ydiv   = 1,
                          deltaIN = FALSE,
                          xlimmIN = c(2010,2100),
                          esm_namesIN=simnames,
                          ageLIST = list(6,6),
                          ylabIN  = c(1,1,1),
                          nmLIST  = list("2 MT cap"="dat_2_5_13","no cap"="dat_2_5_12"),
                          nmLIST_mc = list(dat1=dat_2_5_13_mc,dat2=dat_2_5_12_mc),
                          hLIST   = list("H13_2MT_219_CENaivecf1","H12_219_CENaivecf"),
                          valLIST = list(valIn1="SSB_total_biom", valIn2="SSB_total_biom"),
                          hind    = 1,
                          plotSet = list("RCP 4.5" = c(rcp45_n),"RCP 8.5" = c(rcp85NoBio_n)),
                          prob    = c(.1,.50,.9),
                          lwdd    = c(.7,.3),
                          ltyy    = c("solid","dashed"),
                          showlinetype = TRUE,
                          plot_marginIN=c(-15,5,-10,5),
                          coll    = list(colors()[320],coll_use[2:4],coll_use[5:7]),
                          plot_levels  = simnames[c(1,rcp45_n,rcp85NoBio_n)],
                          alpha   = c(100,0,0), 
                          ylabb   = "Spawning biomass (million tons)",
                          lgnpos= "bottom")}

# Fig S3: effective F

figS3 <- function(H=3.5,W=7){
  dev.new(height=H,width=W)
  plot_Feffective()
}



# Fig S4: risk plot
figS4 <- function(){
  GGplot_aclimCEATTLE_risk(h=4.75*1.3,w=3.2*1.3,coll= c(col2(12)[c(1,5,8,12)]),colvar="type",rowvar="sp",alpha=c(.9,1),sizeIN = 6,
                           plot_marginIN=c(-15,5,-10,5),mode="MSM",lwdd=c(.7,.4,.4),rcpIN=c("RCP 8.5"="rcp85"),pchh=c(15,16),
                           lgnpos= "bottom",RISKTYPES = riskTypes[c(1,3)],ltyy=c("solid","solid"))
  # grid.force()
  # # change shape of arrows
  # grid.gedit("segments", gp=gpar(linejoin ='mitre'))
  # # change the shape in legend also
  # grid.gedit("layout", gp=gpar(linejoin ='mitre'))
  
}



# Fig S5: threshold 1
figS5 <- function(H=3*1.3, W= 4.75*1.3){
  dev.new(height=H,width=W)
  PLOT_THRESHOLD(
    firstdiff=F,
    dataIN    = list(
      "no cap"   = list(C_thresh_12_1,C_thresh_12_2,C_thresh_12_3)),
    ntemps=3,
    multIN=10,
    ylimmIN =list(c(-200,200),c(-30,30)),
    xlimmIN =c(1,7),
    trndln  = "white",
    binW = c(.2,10),
    trndln2 = Ornjazz[3],
    tipping = Ornjazz[5],
    sizeIN=c(0.1,.3,1.5,2))
}


# Fig S6: hindcast years
figS6 <- function( H = 3.25*1.3, W= 4.5*1.3){
  dev.new( height =H, width= W)
  plot_figS6(thrsh   = 2.1)
}

# for reviewer:
figS7 <-function(){  GGplot_aclimTS(dat=reshape2::dcast(covariates%>%filter(Var=="fallZavg"),t~Scenario)
                                    ,h=2*1.3,w=4.75*1.3,threshold = 0,
                                    ylabb=expression(paste("Scasled Fall Zooplankton (Z-scored)")),
                                    ltyy=c("solid",rep("solid",6)),
                                    subtitle_face="plain",
                                    plotSet=list(c(1,rcp45_n),c(1,rcp85NoBio_n)),
                                    coll=coll_use,tline=2,talpha=.5,
                                    xlabb="",lgnpos= "right",plot_marginIN=c(-10,-1,-10,1))}


