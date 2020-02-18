

#________________________________________________
# GG plots for EBFM CEATTLE climate paper
# Holsman et al. 2018
# Kirstin.holsman@noaa.gov
#________________________________________________
  
  # Load Data
  #load("EBM_ceattle.Rdata")

  # Load libraries
  require(data.table)
  require(mgcv)
  library(reshape)
  require(ggplot2)
  require(dplyr)
  require(wesanderson)
  library(ggplot2)
  library(scales)
  library(grid)
  library(ggforce)
  if (!require("processx")) install.packages("processx")
  if (!require("plotly")) install.packages("plotly")
  
  # load Kir's theme:
  source("~/Documents/D_AFSC_Files/AFSC_code/R_scripts/Misc_scripts/THEMES_GGPLOT.r")
  

  # set up colors
  blues <- RColorBrewer::brewer.pal(5, "Blues")
  BG <- RColorBrewer::brewer.pal(9, "GnBu")  #5
  Ornjazz <- RColorBrewer::brewer.pal(5, "Oranges")
  YGB<-(RColorBrewer::brewer.pal(5, "YlGnBu"))
  bg<-colorRampPalette(BG)
  YlGnBu<-colorRampPalette(YGB[-1])
  blu<-colorRampPalette(blues[-1])
  night<-colorRampPalette(colors()[c(653,47,474,72,491,477)])
  dawn<-colorRampPalette(c(colors()[c(477,491,72,474,47,653)],"orange","red"))
  orng<-colorRampPalette(Ornjazz[1:5])
  plt<-c("Zissou1","Darjeeling1","Darjeeling2","FantasticFox1")
  colIN1<-colorRampPalette(c(wes_palette(n=5, name=plt[1])[1:5]))
  col_in<-colorRampPalette(colors()[c(408,44,73)])
  col_in<-colorRampPalette(colors()[c(459,122,73)])
  col_in2<-colorRampPalette(c("orange","red"))
  wes<-colorRampPalette(c(wes_palette(n=5, name=plt[1])[1:5]))
  wes(6)

  sppINFO<-list(
    plk=list(abv="plk",
             guildIN="Walleye pollock",
             plotSPP="walleye pollock",
             bin2=c(seq(0,300,10),1000),
             binJvAD=c(0,40,1000),
             splistIN="W. Pollock",doNEBS=T,plotIT=T),
    pcod=list(abv="pcod",
              guildIN="Pacific cod",
              plotSPP="Pacific cod",
              bin2=c(seq(0,300,10),1000),
              binJvAD=c(0,40,1000),
              splistIN="P. Cod",doNEBS=T,plotIT=T),
    atf=list(abv="atf",
             guildIN="Arrowtooth or Kamchatka",
             plotSPP="arrowtooth flounder",
             bin2=c(seq(0,300,10),1000),
             binJvAD=c(0,40,1000),
             splistIN=c("Arrowtooth","Arrow or Kam", "Kamchat fl"),doNEBS=F,plotIT=T)
  )


  binIT           <-  function(x,bins=seq(0,1000,10)){unlist(lapply(x,function(x) bins[rev(which(x>=bins))[1]] ))}
  
# moving average:
ma2 <- function(x, n = 10){stats::filter(x, rep(1 / n, n), sides = 2)}
#The width of figures, when printed, will usually be 5.5 cm (2.25 inches or 1 column) or 12.0 cm (4.75 inches or 2 columns). 
  #-------------------------------------
  # GGplot_aclimTS
  #-------------------------------------
  #' PLot ACLIM covariates using GGplot
  #'
  #' @param dat is a data.frame with t=date and dat=data to plot
  #' @param plotSet is the columns of dat that you want to plot (col=1 is the date, 2:ncol are options)
  #' @param lwdd vector of the line widths; length of the projection simulation set + 1 for hindcast (dim of dat)
  #' @param ltyy vector of the line types; length of the projection simulation set + 1 for hindcast (dim of dat)
  #' @param ylab y axis label
  #' @param xlab x axis label
  #' @param title title for the graph
  #' @param coll vector colors for each line; length of the projection simulation set + 1 for hindcast (dim of dat)
  #' @param esnmCol colors for each ensemble polygon
  #' @param esnmSet is a list with the set for each ensemble groups
  #' @param alpha vector of transparencies of the ensemble mean; corresponds to number quantiles
  #' @param add0line Add a horizontal line at 0?
  #' @keywords Temperature, plot, data, ACLIM
  #' @export 
  #' @examples
GGplot_aclimTS<-function(
    dat,
    plotSet=list(c(2,rcp45_n),c(2,rcp85NoBio_n)),
    h=3,
    w=4.75,
    sublab=TRUE,
    sublab_adj=0.95,
    lgnpos= "bottom",  #c(.95,1.12),
    fn="BT",
    lwdd=rep(2,13),
    coll=c(colors()[491],col2(6)[c(1,3,5)],col3(6)[c(2,3,6)]),
    ltyy=rep("solid",7),
    ylabb=expression(paste("Bottom temperature",'( '^{o},"C)")),
    xlabb="Year", 
    xlimmIN = NULL,
    titleIN="",
    captionIN="",
    subtitleIN="",
    projLine=2017,
    threshold = 2.5,
    tline=5,
    talpha=.5,
    smooth_yr=20,
    add0line=FALSE,
    plot_marginIN= c(1, 1, 1, 1),
    plot_title_marginIN = 0,
    subtitle_marginIN = 0,
    subtitle_faceIN = "bold",
    caption_marginIN = 0){

    dev.new(height=h,width=w)
      make_plotdat<-function(datIN=dat){

          mlt<-reshape::melt(datIN[,1+plotSet[[1]]])
          mlt$Year<-datIN[,1]
          mlt$rcp=" RCP 4.5 "
          mlt$num<-factor(mlt$variable,levels=simnames[c(plotSet[[1]],plotSet[[2]][-1])])
          mlt$col<-coll[mlt$num]
          mlt$lty<-ltyy[mlt$num]
          mlt2<-reshape::melt(datIN[,1+plotSet[[2]]])
          mlt2$Year<-datIN[,1]
          mlt2$rcp=" RCP 8.5 "
          mlt2$num<-factor(mlt2$variable,levels=simnames[c(plotSet[[1]],plotSet[[2]][-1])])
          mlt2$col<-coll[mlt2$num]
          mlt2$lty<-ltyy[mlt2$num]
          # mlt2$col<-collIN[length(plotSet[[1]])+2:length(plotSet[[2]])]
          # mlt2$lty<-ltyyIN[length(plotSet[[1]])+2:length(plotSet[[2]])]
          dt<-rbind(mlt,mlt2)
          dt$rcp<-factor(dt$rcp,levels=c(" RCP 4.5 "," RCP 8.5 "))
          
          return(dt)
        }
      dt      <-   make_plotdat(dat)
      m_dat   <-   dat
      dat[,2] <-   NA*dat[,2]
      for(i in 3:dim(dat)[2])
       m_dat[,i]  <-  as.numeric(ma2(x=(dat[,i]),n=smooth_yr))
      m_dat2  <-  dat
      for(i in 2:dim(dat)[2])
       m_dat2[,i]  <-  as.numeric(ma2(x=(dat[,i]),n=smooth_yr))

      
      m_dt    <-  make_plotdat(m_dat)
      m_dt2   <-  make_plotdat(m_dat2)

      dt$zeroline     <- 0;    dt$projLine    <- projLine
      m_dt$zeroline   <- 0;    m_dt$projLine  <- projLine
      m_dt2$zeroline  <- 0;    m_dt2$projLine <- projLine

      p <-     ggplot(data=dt, aes(x = Year, y = value),colour=variable)
      p <- p + facet_grid(~rcp) 
      if(add0line)   p <- p + geom_hline(data=dt, aes(yintercept = zeroline),col="lightgray")
      p <- p + geom_vline(data=dt, aes(xintercept=projLine),col="gray",size=1,linetype="dashed") 
      if(!is.null(threshold)) p <- p + geom_hline (vdata=dt, aes(yintercept=threshold),col="gray",size=tline, alpha =talpha) 
      p <- p + geom_line(aes(x = Year, y = value,group=variable,colour=variable,linetype=variable),alpha=.6,inherit.aes=TRUE,size=.4)
      # add moving average:
      p <- p + geom_line(data=m_dt,aes(x = Year, y = value,group=variable,colour=variable,linetype=variable),alpha=1,inherit.aes=FALSE,size=.75)
      p <- p + geom_line(data=m_dt2,aes(x = Year, y = value,group=variable,colour=variable,linetype=variable),alpha=1,inherit.aes=FALSE,size=.75)

      p <- p + scale_color_manual(values=c(coll))
      p <- p + scale_linetype_manual(values=ltyy)
      p
      
      p<- p + theme_light() +
      labs(x=NULL, y=NULL,
           title=titleIN,
           subtitle=subtitleIN,
           caption=captionIN) +
      theme(plot.subtitle=element_text(margin=margin(b=20))) +
      theme(legend.title=element_blank()) +
      theme(legend.position=lgnpos) +
      theme(legend.key.width = unit(.5, "cm")) +
      theme(legend.text=element_text(size=5)) +
      theme(legend.key.size=unit(.01, "cm")) +
      labs(x= xlabb,y=ylabb)+
      #labs(tag=letters(1:6)) +
      theme(plot.margin=margin(t = 10, r = 10, b = 10, l =10)) 

      p<- p+ theme_kir_EBM(sub_title_size=12,
                                       sub_title_just="l",
                                       plot_margin = margin(plot_marginIN),
                                       plot_title_margin = plot_title_marginIN,
                                       subtitle_margin = subtitle_marginIN,
                                       caption_margin = caption_marginIN,
                                       axis_title_just = "cm") +
      labs(x=xlabb, y= ylabb)+
      theme(plot.subtitle=element_text(face=subtitle_faceIN)) +
      theme(legend.title=element_blank()) 
      p <- p + theme(legend.position=lgnpos)
      
      if(!is.null(sublab)){
        ann_text<-dt
        ann_text[-(1:2),]<-NA
        ann_text[1:2,]$value<-max(dt$value,na.rm=T)
        ann_text[1:2,]$Year<-min(dt$Year,na.rm=T)
        ann_text[1:2,]$rcp<-factor(levels(dt$rcp),levels=levels(dt$rcp))
        ann_text$lab2<-NA
        ann_text[1:2,]$lab2<-letters[1:2]
        ann_text<- na.omit(ann_text)
        
        p <-  p + geom_text(data = ann_text,aes(x = Year, y =value *sublab_adj,label = lab2,fontface=2))
      }
      p
  }


GGplot_aclimCEATTLE_delta<-function(
    esm_namesIN=simnames,
    nmLIST  = list("2 MT cap"="dat_2MT_219_CENaivecf1_2_5_13","no cap"="dat_219_CENaivecf_2_5_12"),
    datLIST = list(dat1=B0_2MT_219_CENaivecf1_2_5_13_mc,dat2=B0_219_CENaivecf_2_5_12_mc),
    valLIST = list(valIn1="SSB0_total_biom", valIn2="SSB0_total_biom"),
    deltaIN = FALSE,
    plotSet = list("RCP 4.5" = c(1,rcp45_n_sim),"RCP 8.5" = c(1,rcp85NoBio_n_sim)),
    h       = 3,
    w       = 4.75,
    plotpersist = TRUE,
    ylimm_up    = c(20,2,1.5)*1e6,
    ylimm_dwn   = c(0,0,0),
    xlimmIN = NULL,
    scalesIN= "free_y",
    sublab  = TRUE,
    sublab_adj = 0.95,
    lgnpos  = "bottom",
    fn      = "BT",
    ltyy    = c("solid","solid"),
    lwdd    = c(.7,.4),
    coll    = c(colors()[320],col2(6)[c(2,3,4)],col3(6)[c(3,4,6)]),
    ylabb   = "Spawning biomass (million tons)",
    xlabb   = "Year", 
    ylabIN  =  c(sp1=20,sp2=2,sp3=1.4),
    xlabIN  =  c(sp1=1979,sp2=1979,sp3=1979),
    nspp    = 3,
    ydiv    = 1e6,
    titleIN = "",
    captionIN  = "",
    subtitleIN = "",
    projLine   = 2017,
    smooth_yr  = 20,
    add0line   = FALSE,
    alpha      = c(20,40), 
    prob       = c(.1,.50,.9),
    plot_marginIN         = c(1, 1, 1, 1),
      plot_title_marginIN = 0,
      subtitle_marginIN   = 0,
      caption_marginIN    = 0
    ){
   
        dev.new(height=h,width=w)
        ndat   <- length(nmLIST)
        DAT    <- list()
        for(ll in 1:ndat)
          eval(parse(text = paste0("DAT[[ll]]<-grabDat(datIn=",nmLIST[ll],",valIn='",valLIST[ll],"')") ))
        nspp   <-  length(DAT[[1]])

        if(deltaIN)
          for(ll in 1:ndat)
            for(sp in 1:nspp)
              DAT[[ll]][[sp]][,-(1)]  <-  100*(DAT[[ll]][[sp]][,-(1)]-DAT[[ll]][[sp]][,2])/DAT[[ll]][[sp]][,2]
            
        yr     <-  DAT[[ll]][[1]]$Year
        nset   <-  length(plotSet)  # number of contrasts
        maxx   <-  0
        for(i in 1:length(plotSet))
          maxx <-  max(length(plotSet[[i]]),maxx)

        
        #splevels        <- 1:3
        splevels        <- paste0(letters[1:3],") ",c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        splabels        <- paste0(letters[1:3],") ",c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        
        splevels        <- paste0(c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        splabels        <- paste0(c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        
        #splabels        <- paste0(rep(" ",3),c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))

        rcplevels        <- names(plotSet)
        rcplabels        <- names(plotSet)

        sp.labs          <- splevels
        names(sp.labs)   <- splabels
        rcp.labs         <- rcplevels
        names(rcp.labs)  <- rcplabels

        print(sp.labs)

        #splevels<-c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        correctOrder    <- c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])  # system constant in correct sort order.
        #correctOrder    <- factor(paste(1:length(correctOrder),correctOrder))
        correctOrderLab <- c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])

        eval(parse(text= paste0("qnt1<-qnt2<-data.frame(Year=NA,sp=NA,scen=NA,scenario=NA,HCR=NA,scenario_hcr=NA,rcp=NA,order=NA,",
        paste0('prob',prob*100,'=NA',collapse=","),",",paste0('mnhind_prob',prob*100,'=NA',collapse=","),
        ",meanHindRun=NA,meanFutRun=NA,coll=NA,sublab=NA,laby=NA,labx=NA)")))
        
        for(ll in 1:ndat){
          kk  <-  1
          dat1<-datLIST[[ll]]
          for(i  in 1:nset){
            for(jj in 1:length(plotSet[[i]])){
              kk  <-  kk + 1
              for(sp in 1:nspp){
                  tq      <-  t(exp(apply(log(dat1[sp,,,plotSet[[i]][jj]]),2,quantile,probs=prob)))
                if(deltaIN)  
                  tq      <-  t((apply(100*( dat1[sp,,,plotSet[[i]][jj]]  - dat1[sp,,,1]  )/dat1[sp,,,1] ,2,quantile,probs=prob)))
                
                mnhind  <-  t(exp(apply(log(dat1[sp,,,1]),2,quantile,probs=prob)))
                if(deltaIN)  
                  mnhind  <-  mnhind*0

                if(any(tq > ylimm_up[sp]))  
                  tq[tq > ylimm_up[sp]]  <-  ylimm_up[sp]

                if(any(tq < ylimm_dwn[sp]))  
                  tq[tq < ylimm_dwn[sp]]  <-  ylimm_dwn[sp]
            
                if(any(mnhind  > ylimm_up[sp]))  
                 mnhind[mnhind > ylimm_up[sp]]  <-  ylimm_up[sp]

               if(any(mnhind  < ylimm_dwn[sp]))  
                 mnhind[mnhind < ylimm_dwn[sp]]  <-  ylimm_dwn[sp]

                tmpq1  <-  data.frame(Year=as.numeric(rownames(tq)),
                  sp=factor(sppINFO[[sp]]$plotSPP,levels=splevels),
                  scen=esm_namesIN[plotSet[[i]]][jj],
                  scenario = correctOrder[grep(esm_namesIN[plotSet[[i]]][jj],correctOrder)], 
                  HCR = names(nmLIST)[ll],
                  scenario_hcr = paste(correctOrder[grep(esm_namesIN[plotSet[[i]]][jj],correctOrder)],names(nmLIST)[ll]), 
                  rcp = names(plotSet)[i],
                  order = grep(esm_namesIN[plotSet[[i]]][jj],correctOrder),
                  tq,mnhind,
                  DAT[[ll]][[sp]][,-1][, plotSet[[i]][1] ],
                  DAT[[ll]][[sp]][,-1][, plotSet[[i]][jj] ],
                  coll=coll[kk],sublab=NA,laby=NA,labx=NA)
               
                colnames(tmpq1)<-colnames(qnt1)
                if(!is.null(sublab)){
                  tmpq1$sublab[1]  <-  letters[sp*i+(nspp-sp)*(i-1)]
                  tmpq1$laby  <-  as.numeric(ylabIN[sp])
                  tmpq1$labx  <-  as.numeric(xlabIN[sp])
                }
                
                qnt1<-rbind(qnt1,tmpq1)
              }
            }
          }
        }
        
        qnt<-qnt1[-1,]
        qnt$sp  <-factor(qnt$sp,levels=splevels)
        qnt$rcp <-factor(qnt$rcp,levels=unique(names(plotSet)))
        qnt$HCR <- factor(qnt$HCR, levels =unique(names(nmLIST)))
        qnt$scenario_hcr <- factor(qnt$scenario_hcr, levels =unique(qnt$scenario_hcr))
        #qnt$scen  <-  factor(qnt$scen,levels=unique( esm_namesIN[unlist(plotSet)])) 
        qnt$scen  <-  factor(qnt$scen,levels=correctOrder) 
        qnt$scenario  <-  factor(qnt$scenario,levels=correctOrder) 
      
        qnt$projLine<-projLine
        
        nHCR <-  length(names(nmLIST))
        hcr  <-  names(nmLIST)        
        mnNAME<-paste0('prob',prob[((length(prob)-1)/2)+1]*100,collapse=",")

        p <-     ggplot(data=qnt[qnt$HCR==hcr[1],], aes(x = Year, y = meanFutRun/ydiv),colour=scenario)
        # New facet label names for dose variable
        p <- p + facet_grid(sp~rcp,scales=scalesIN, labeller = labeller(sp = sp.labs, rcp = rcp.labs))
        
        # if(add0line)   p <- p + geom_hline(data=qnt, aes(yintercept = zeroline),col="lightgray")
        p <- p + geom_vline(data=qnt[qnt$HCR==hcr[1],], aes(xintercept=projLine),col="gray",size=1,linetype="dashed") 
        
        # add moving average:
        probNames<-paste0('prob',prob*100)
        nprob<- length(probNames)
        mnprobNames<-paste0('mnhind_prob',prob*100)
        
        if(plotpersist)
          p <-  p+ geom_line(data=qnt, aes(x = Year, y = meanHindRun/ydiv,colour=scenario,linetype=HCR,size=HCR))
      
       for(nn in 1:((nprob-1)/2))
       eval(parse(text=paste0("p <-  p+ geom_ribbon(data=qnt,aes(x = Year, ymin = ",probNames[nn],"/ydiv, ymax =",probNames[1+(nprob-nn)],"/ydiv,fill=scenario),alpha=alpha[2]/100)")))
        p <-  p + geom_line(data=qnt, aes(x = Year, y = prob50/ydiv,colour=scenario,linetype=HCR,size=HCR))
        p <-  p + geom_line(data=qnt, aes(x = Year, y = prob50/ydiv,colour=scenario,linetype=HCR,size=HCR))
        p <-  p + scale_color_manual(values=coll)
        p <-  p + scale_fill_manual(values=coll, name="fill")
        p <-  p + scale_linetype_manual(values=ltyy)
        p <-  p + scale_size_manual(values=lwdd)
        p <-  p + guides(fill=FALSE)
        p <-  p + theme_light() +
        labs(x=NULL, y=NULL,
           title=titleIN,
           subtitle=subtitleIN,
           caption=captionIN) +
        theme(plot.subtitle=element_text(margin=margin(b=20))) +
        theme(legend.title=element_blank()) +
        theme(legend.position=lgnpos) +
        theme(legend.key.width = unit(.5, "cm")) +
        theme(legend.text=element_text(size=5)) +
        theme(legend.key.size=unit(.01, "cm")) +
        labs(x= xlabb,y=ylabb)+
        #labs(tag=letters(1:6)) +
        theme(plot.margin=margin(t = 10, r = 10, b = 10, l =10)) 

        p<- p+ theme_kir_EBM(sub_title_size=12,
                                       sub_title_just="l",
                                       plot_margin = margin(plot_marginIN),
                                       plot_title_margin = plot_title_marginIN,
                                       subtitle_margin = subtitle_marginIN,
                                       caption_margin = caption_marginIN,
                                       axis_title_just = "cm") +
              labs(x=xlabb, y= ylabb)+
              theme(legend.title=element_blank(),
              legend.background = element_rect(colour = NA),
              legend.key = element_rect(colour = "white", fill = NA)) 
        p <- p + theme(legend.position=lgnpos)
        p <- p + guides(fill = FALSE)
        p <- p + guides(color = guide_legend(order = 1))
        p <- p + guides(color=guide_legend(override.aes=list(fill=NA)))
        p <- p + theme(plot.subtitle=element_text(face="bold"))
        if(!is.null(xlimmIN[1])) 
          p <- p + xlim(xlimmIN[1],xlimmIN[2]) 
        
        if(!is.null(sublab)){
          ann_text <-as_tibble(na.omit(data.frame(x=qnt$labx,y=qnt$laby,lab=qnt$sublab,sp=qnt$sp,rcp=qnt$rcp)))%>%
            group_by(sp,rcp,lab)%>%
            summarize(Year=mean(x),val=mean(y))
          ann_text$lab2<-letters[1:dim(ann_text)[1]]
          p <-  p + geom_text(data = ann_text,aes(x = Year, y = val*sublab_adj,label = lab2,fontface=2))
        }
        
        p
        

}

GGplot_aclimCEATTLE_delta_Spaghetti<-function(
    esm_namesIN=simnames,
    nmLIST  = list("2 MT cap"="dat_2MT_219_CENaivecf1_2_5_13","no cap"="dat_219_CENaivecf_2_5_12"),
    datLIST = list(dat1=B0_2MT_219_CENaivecf1_2_5_13_mc,dat2=B0_219_CENaivecf_2_5_12_mc),
    valLIST = list(valIn1="SSB0_total_biom", valIn2="SSB0_total_biom"),
    deltaIN = FALSE,
    plotSet = list("RCP 4.5" = c(1,rcp45_n_sim),"RCP 8.5" = c(1,rcp85NoBio_n_sim)),
    h       = 3,
    w       = 4.75,
    plotpersist = TRUE,
    ylimm_up    = c(20,2,1.5)*1e6,
    ylimm_dwn   = c(0,0,0),
    xlimmIN = NULL,
    scalesIN= "free_y",
    lgnpos  = "bottom",
    fn      = "BT",
    ltyy    = c("solid","solid"),
    lwdd    = c(.7,.4),
    coll    = c(colors()[320],col2(6)[c(2,3,4)],col3(6)[c(3,4,6)]),
    ylabb   = "Spawning biomass (million tons)",
    xlabb   = "Year", 
    nspp    = 3,
    ydiv    = 1e6,
    titleIN = "",
    captionIN  = "",
    subtitleIN = "",
    projLine   = 2017,
    smooth_yr  = 20,
    add0line   = FALSE,
    alpha      = c(20,40), 
    prob       = c(.1,.50,.9),
    plot_marginIN         = c(1, 1, 1, 1),
      plot_title_marginIN = 0,
      subtitle_marginIN   = 0,
      caption_marginIN    = 0
    ){

        dev.new(height=h,width=w)
        ndat   <- length(nmLIST)
        DAT    <- list()
        for(ll in 1:ndat)
          eval(parse(text = paste0("DAT[[ll]]<-grabDat(datIn=",nmLIST[ll],",valIn='",valLIST[ll],"')") ))
        nspp   <-  length(DAT[[1]])

        if(deltaIN)
          for(ll in 1:ndat)
            for(sp in 1:nspp)
              DAT[[ll]][[sp]][,-(1)]  <-  100*(DAT[[ll]][[sp]][,-(1)]-DAT[[ll]][[sp]][,2])/DAT[[ll]][[sp]][,2]
            
        yr     <-  DAT[[ll]][[1]]$Year
        nset   <-  length(plotSet)  # number of contrasts
        maxx   <-  0
        for(i in 1:length(plotSet))
          maxx <-  max(length(plotSet[[i]]),maxx)

        
        #splevels        <- 1:3
        
        splevels        <- paste0(letters[1:3],") ",c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        splabels        <- paste0(letters[1:3],") ",c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        #splabels        <- paste0(rep(" ",3),c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))

        rcplevels        <- names(plotSet)
        rcplabels        <- names(plotSet)

        sp.labs  <- splevels;names(sp.labs) <- splabels
        rcp.labs <- rcplevels;names(rcp.labs) <- rcplabels

        print(sp.labs)

        #splevels<-c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        correctOrder    <- c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])  # system constant in correct sort order.
        correctOrder    <- factor(paste(1:length(correctOrder),correctOrder))
        correctOrderLab <- c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])

        eval(parse(text= paste0("qnt1<-qnt2<-data.frame(Year=NA,sp=NA,scen=NA,scenario=NA,HCR=NA,scenario_hcr=NA,rcp=NA,order=NA,",
        paste0('prob',prob*100,'=NA',collapse=","),",",paste0('mnhind_prob',prob*100,'=NA',collapse=","),
        ",meanHindRun=NA,meanFutRun=NA,coll=NA)")))
        
        for(ll in 1:ndat){
          kk  <-  1
          dat1<-datLIST[[ll]]
          for(i  in 1:nset){
            for(jj in 1:length(plotSet[[i]])){
              kk  <-  kk + 1
              for(sp in 1:nspp){
                  tq      <-  t(exp(apply(log(dat1[sp,,,plotSet[[i]][jj]]),2,quantile,probs=prob)))
                if(deltaIN)  
                  tq      <-  t((apply(100*( dat1[sp,,,plotSet[[i]][jj]]  - dat1[sp,,,1]  )/dat1[sp,,,1] ,2,quantile,probs=prob)))
                  
                  mnhind  <-  t(exp(apply(log(dat1[sp,,,1]),2,quantile,probs=prob)))
                if(deltaIN)  
                  mnhind  <-  mnhind*0

                if(any(tq > ylimm_up[sp]))  
                  tq[tq > ylimm_up[sp]]  <-  ylimm_up[sp]

                if(any(tq < ylimm_dwn[sp]))  
                  tq[tq < ylimm_dwn[sp]]  <-  ylimm_dwn[sp]
            
                if(any(mnhind  > ylimm_up[sp]))  
                 mnhind[mnhind > ylimm_up[sp]]  <-  ylimm_up[sp]

               if(any(mnhind  < ylimm_dwn[sp]))  
                 mnhind[mnhind < ylimm_dwn[sp]]  <-  ylimm_dwn[sp]

                tmpq1  <-  data.frame(Year=as.numeric(rownames(tq)),
                  #sp=factor(paste0(letters[sp],") ",sppINFO[[sp]]$plotSPP),levels=splevels),
                  sp=factor(paste0(letters[sp],") ",sppINFO[[sp]]$plotSPP),levels=splevels),
                  scen=esm_namesIN[plotSet[[i]]][jj],
                  scenario = correctOrder[grep(esm_namesIN[plotSet[[i]]][jj],correctOrder)], 
                  HCR = names(nmLIST)[ll],
                  scenario_hcr = paste(correctOrder[grep(esm_namesIN[plotSet[[i]]][jj],correctOrder)],names(nmLIST)[ll]), 
                  rcp = names(plotSet)[i],
                  order = grep(esm_namesIN[plotSet[[i]]][jj],correctOrder),
                  tq,mnhind,
                  DAT[[ll]][[sp]][,-1][, plotSet[[i]][1] ],
                  DAT[[ll]][[sp]][,-1][, plotSet[[i]][jj] ],
                  coll=coll[kk])

                #cat(jj,"/",plotSet[[i]][jj],"/",coll[kk],"/", cat(kk),"\n")
                colnames(tmpq1)<-colnames(qnt1)
                qnt1<-rbind(qnt1,tmpq1)
              }
            }
          }
        }
        qnt<-qnt1[-1,]
        qnt$projLine<-projLine
        qnt$scen = factor(qnt$scen) 
        
        nHCR <-  length(names(nmLIST))
        hcr  <-  names(nmLIST)        
        mnNAME<-paste0('prob',prob[((length(prob)-1)/2)+1]*100,collapse=",")

        p <-     ggplot(data=qnt[qnt$HCR==hcr[1],], aes(x = Year, y = meanFutRun/ydiv),colour=scenario)
          # New facet label names for dose variable
          
          # spdat<-qnt[qnt$HCR==hcr[1],]$sp
          # rcpdat<-qnt[qnt$HCR==hcr[1],]$rcp
        p <- p + facet_grid(sp~rcp,scales=scalesIN, labeller = labeller(sp = sp.labs, rcp = rcp.labs))

        # if(add0line)   p <- p + geom_hline(data=qnt, aes(yintercept = zeroline),col="lightgray")
        p <- p + geom_vline(data=qnt[qnt$HCR==hcr[1],], aes(xintercept=projLine),col="gray",size=1,linetype="dashed") 
        #p <- p + geom_line(aes(x = Year, y = prob50/ydiv,group=scen,colour=scen,linetype="solid"),alpha=1,inherit.aes=TRUE,size=.4)
        # add moving average:
        probNames<-paste0('prob',prob*100)
        mnprobNames<-paste0('mnhind_prob',prob*100)
        
        for(j in 1:nHCR){
          subqnt<-qnt[qnt$HCR==hcr[j],]
          if(plotpersist){
            for(i in 1:((length(prob)-1)/2))
              eval(parse(text=paste0(" p <- p + geom_ribbon(data=subqnt, aes(x=Year,ymin = ",mnprobNames[i],"/ydiv, ymax = ",mnprobNames[rev(1:length(prob))[i]],"/ydiv,colour=scenario,fill = scenario), alpha=alpha[2]/100,size=0,inherit.aes=FALSE)")))
          }
          for(i in 1:((length(prob)-1)/2))
          eval(parse(text=paste0(" p <-  p + geom_ribbon(data=subqnt, aes(x=Year,ymin = ",probNames[i],"/ydiv, ymax = ",probNames[rev(1:length(prob))[i]],"/ydiv,group=scenario,fill = scenario), alpha=alpha[1]/100,size=0,inherit.aes=FALSE)")))
          coll2<-coll
        }
        if(plotpersist) {
          p <-  p+ geom_line(data=qnt, aes(x = Year, y = meanHindRun/ydiv,colour=scenario,linetype=HCR,size=HCR))
        }
        p <- p + geom_line(data=qnt, aes(x = Year, y = meanFutRun/ydiv,colour=scenario,linetype=HCR,size=HCR),alpha=1,inherit.aes=TRUE)
          
        p <- p + scale_color_manual(values=coll2)
        p <- p + scale_fill_manual(values=coll2, name="fill")
        #p <- p + guides(color = guide_legend(order = 2))
      
        p <- p + scale_linetype_manual(values=ltyy)
        p <- p + scale_size_manual(values=lwdd)
        

        p<- p + theme_light() +
        labs(x=NULL, y=NULL,
           title=titleIN,
           subtitle=subtitleIN,
           caption=captionIN) +
        theme(plot.subtitle=element_text(margin=margin(b=20))) +
        theme(legend.title=element_blank()) +
        theme(legend.position=lgnpos) +
        theme(legend.key.width = unit(.5, "cm")) +
        theme(legend.text=element_text(size=5)) +
        theme(legend.key.size=unit(.01, "cm")) +
        labs(x= xlabb,y=ylabb)+
        #labs(tag=letters(1:6)) +
        theme(plot.margin=margin(t = 10, r = 10, b = 10, l =10)) 

        p<- p+ theme_kir_EBM(sub_title_size=12,
                                       sub_title_just="l",
                                       plot_margin = margin(plot_marginIN),
                                       plot_title_margin = plot_title_marginIN,
                                       subtitle_margin = subtitle_marginIN,
                                       caption_margin = caption_marginIN,
                                       axis_title_just = "cm") +
              labs(x=xlabb, y= ylabb)+
              theme(legend.title=element_blank(),
              legend.background = element_rect(colour = NA),
              legend.key = element_rect(colour = "white", fill = NA)) 
        p <- p + theme(legend.position=lgnpos)
        p <- p + guides(fill = FALSE)
        p <- p + guides(color = guide_legend(order = 1))
        p <- p + guides(color=guide_legend(override.aes=list(fill=NA)))
        p <- p + theme(plot.subtitle=element_text(face="bold"))
        if(!is.null(xlimmIN[1])) 
          p <- p + xlim(xlimmIN[1],xlimmIN[2]) 

        p

}


GGplot_aclimCEATTLE_risk_droppt2<-function(
  esm_namesIN=simnames,
  RISK    = list("no cap" = risk12,"2 MT cap" = risk13),
  RISKTYPES = riskTypes[c(1,2,3)],
  modeIN  = c("MSM"),
  rcpIN   = c("RCP 8.5"="rcp85"),
  deltaIN = FALSE,
  plotSet = list("RCP 8.5" = c(1,rcp85NoBio_n_sim)),
  riskIN  = "riskC",
  colvar  ="timeframe",
  rowvar  ="sp",
  between_group = "type",
  barsplit ="hcr",
  revbar  = TRUE,
  flip    = TRUE,
  rev_lines    = FALSE,
  rev_group    = FALSE,
  rev_barsplit = FALSE,
  itr = 100,
  spacer  = 1.1,
  spacer_groups  = .4,
  spacer_lines  = 1.04,
  jitter_size =1.5,
  segment_size=.7,
  point_size =4,
  h       = 3,
  w       = 4.75,
  spIN    = 1,
  alphaIN =.1,
  scalesIN= "free_y",
  angleIN = 90,
  xtick_hjust = .8,
  xtick_vjust = .5,
  lgnpos  = "bottom",
  coll    = col4(6)[c(2:6)],
  ylabb   = "Risk of decline in catch", 
  xlabb   = "Time period", 
  nrowlg  = c(2,2),
  ylimm=c(0,100),
  titleIN = "",
  captionIN  = "",
  subtitleIN = "",
  plot_marginIN         = c(1, 1, 1, 1),
  backcol = "gray",
  plot_title_marginIN = 0,
  subtitle_marginIN   = 0,
  caption_marginIN    = 0,
  arrow_start1 = 30,
  arrow_start2 = 20,
  arrow_adj1 = .5,
  arrow_adj2 = .6,
  arrow_yadj = .5,
  arrow_lvl = 2,
  yspace = c(6,-6)
){
  
  tmp1      <-  as_tibble(RISK[[1]])%>%filter(sp%in%spIN,mode%in%modeIN, type%in%RISKTYPES,rcp%in%rcpIN)
  tmp2      <-  as_tibble(RISK[[2]])%>%filter(sp%in%spIN,mode%in%modeIN, type%in%RISKTYPES,rcp%in%rcpIN)
  hcrs      <-  names(RISK)
  tmp1$hcr  <-  hcrs[1]
  tmp2$hcr  <-  hcrs[2]
  eval(parse(text=paste0("tmp1$Size <-  inv.logit((as.numeric(scale(1/((tmp1$",riskIN,"cv))))))")))
  eval(parse(text=paste0("tmp2$Size <-  inv.logit(as.numeric(scale(1/((tmp2$",riskIN,"cv)))))")))
  tmp1$Size[is.na(tmp1$Size)]  <-  1
  tmp2$Size[is.na(tmp2$Size)]  <-  1
  tmp1      <-  data.frame(tmp1)
  tmp2      <-  data.frame(tmp2)
  tmpall    <-  rbind(tmp1,tmp2)
  
  tmpall$timeframe  <-  factor(tmpall$timeframe,levels=levels(tmp1$timeframe))
  tmpall$rcp        <-  factor(tmpall$rcp,levels=levels(tmpall$rcp))
  tmpall$hcr        <-  factor(tmpall$hcr,levels=unique(tmpall$hcr))
  tmpall$sp         <-  factor(tmpall$sp,levels=unique(tmpall$sp))
  tmpall$mode       <-  factor(tmpall$mode,levels=levels(tmpall$mode))
  tmpall$type       <-  factor(tmpall$type,levels=levels(tmpall$type))
  tmpall$riskbin    <-  binIT(tmpall$Risk,bins=Ybins)
  
  if(rev_lines) tmpall[,colvar]<-factor(tmpall[,colvar],levels =rev(levels(tmpall[,colvar])))
  if(rev_group) tmpall[,between_group]<-factor(tmpall[,between_group],levels =rev(levels(tmpall[,between_group])))
  if(rev_barsplit) tmpall[,barsplit]<-factor(tmpall[,barsplit],levels =rev(levels(tmpall[,barsplit])))
  
  dev.new(height=h,width=w)
  maxx      <-  0
  for(i in 1:length(plotSet))
    maxx    <-  max(length(plotSet[[i]]),maxx)
  
  splevels        <-  paste0(letters[1:3],") ",c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
  splevels        <-  paste0(c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
  if(length(plotSet)>1){
    correctOrder    <-  c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])  # system constant in correct sort order.
    correctOrder    <-  factor(paste(1:length(correctOrder),correctOrder))
    correctOrderLab <-  c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])
  }
  
  tmpall$sp       <-  factor(splevels[tmpall$sp],levels=splevels)
  tmp1$sp         <-  factor(splevels[tmp1$sp],levels=splevels)
  tmp2$sp         <-  factor(splevels[tmp2$sp],levels=splevels)
  Ybins           <-  seq(0,100,10)
  binIT           <-  function(x,bins=seq(0,1000,10)){unlist(lapply(x,function(x) bins[rev(which(x>=bins))[1]] ))}
  tmp1$riskbin    <-  binIT(tmp1$riskB,bins=Ybins)
  tmpall$riskbin  <-  binIT(tmpall$riskB,bins=Ybins)
  set.seed(999)
  xvar        <-  c(rnorm(1500, mean = -1), rnorm(1500, mean = 1.5))
  yvar        <-  c(rnorm(1500, mean = 1), rnorm(1500, mean = 1.5))
  zvar        <-  as.factor(c(rep(1, 1500), rep(2, 1500)))
  xy          <-  data.frame(xvar, yvar, zvar)
  itr         <-  itr
  riskINsd    <-  paste0(riskIN,"sd")
  tmpall$mid_label<- as.numeric(tmpall[,between_group])
  tmpall$mid      <-  as.numeric(tmpall[,between_group])*spacer+ ((as.numeric(tmpall[,colvar])-1)/10)*spacer_groups +
    (((as.numeric(tmpall[,barsplit])-1)/10)*spacer_lines)
  
  tmpall$Risk     <-  tmpall[,riskIN]
  tmpall$linetype <-  tmpall[,colvar]
  tmpall$shape    <-  tmpall[,colvar]
  tmpall$linesize <-  tmpall[,barsplit]
  
  jitterTMP   <-  data.frame(timeframe=NA,hcr=NA,rcp=NA,sp=NA,mode=NA,type=NA,riskbin=NA,Risk=NA,mid=NA)
  for( i in 1:dim(tmpall)[1])
    jitterTMP  <-  rbind(jitterTMP,data.frame(
      timeframe  =  tmpall$timeframe[i],
      rcp        =  tmpall$rcp[i],
      hcr        =  tmpall$hcr[i],
      sp         =  tmpall$sp[i],
      mode       =  tmpall$mode[i],
      type       =  tmpall$type[i],
      riskbin    =  tmpall$riskbin[i],
      Risk       =  rnorm(itr, 
                          mean = tmpall[,riskIN][i],
                          sd=tmpall[,riskINsd][i]),
      mid        =  tmpall$mid[i]))
  
  jitterTMP            <-  jitterTMP[-1,]
  jitterTMP$timeframe  <-  factor(jitterTMP$timeframe,levels=levels(tmp1$timeframe))
  jitterTMP$rcp        <-  factor(jitterTMP$rcp,levels=levels(tmpall$rcp))
  jitterTMP$sp         <-  factor(jitterTMP$sp,levels=levels(tmpall$sp))
  jitterTMP$mode       <-  factor(jitterTMP$mode,levels=levels(tmpall$mode))
  jitterTMP$type       <-  factor(jitterTMP$type,levels=levels(tmpall$type))
  jitterTMP$riskbin    <-  binIT(jitterTMP$Risk,bins=Ybins)
  jitterTMP$linetype   <-  jitterTMP[,colvar]
  jitterTMP$shape      <-  jitterTMP[,colvar]
  jitterTMP$linesize   <-  jitterTMP[,barsplit]
  
  alphas    <-  c(1,0)
  if(revbar) 
    alphas  <-  rev(alphas)
  lvl<- levels(tmpall[,barsplit])
  if(revbar) lvl<-rev(lvl)
  
  dataUSE <- dataUSE2 <- tmpall
  dataUSE$Risk <- dataUSE2$Risk <-dataUSE2$Riskend <- dataUSE2$Risk*NA
  cc  <-  which(dataUSE2$hcr==lvl[1])
  dataUSE2$Risk[cc]      <-  1
  dataUSE2$Riskend[cc]   <-  1
  dataUSE$Risk[-cc]  <-  1
  dataUSE2$Risk <- dataUSE2$Risk*tmpall$Risk
  dataUSE2$Riskend[cc] <- tmpall$Risk[-cc]
  dataUSE$Risk  <- dataUSE$Risk*tmpall$Risk
  
  jitterUSE <- jitterUSE2 <- jitterTMP
  jitterUSE$Risk <- jitterUSE2$Risk <- jitterUSE$Risk*NA
  cc  <-  which(jitterUSE$hcr==lvl[1])
  jitterUSE2$Risk[cc]  <-  1
  jitterUSE$Risk[-cc]  <-  1
  jitterUSE2$Risk <- jitterUSE2$Risk*jitterTMP$Risk
  jitterUSE$Risk  <- jitterUSE$Risk*jitterTMP$Risk
  
  
  p <- ggplot(data = dataUSE2, aes(x =mid-.4 , xend = mid+.4, y = Risk, yend = Risk,
                                   color = Risk,
                                   shape=shape,linesize=linesize))
  if(flip) 
    p <-   p+ coord_flip() 
  p <-  p +geom_jitter(data=jitterUSE,aes(x =mid ,y = Risk),size = jitter_size, alpha = alphaIN, width = 0.03,col="gray")+
    scale_y_continuous(limits = c(0, 100), expand = c(0.005, 0.005)) 
  #p <-  p + geom_point(data = dataUSE, aes(x =mid ,y = Risk),size = 5,color="gray")
  
  p <- p +geom_jitter(data=jitterUSE2,aes(x =mid ,y = Risk,color=Risk),size = jitter_size, alpha = alphaIN, width = 0.03,
                      inherit.aes = FALSE)
  p <-  p + geom_segment(data = dataUSE, aes(x =mid-spacer/5 , xend = mid+spacer/5+spacer_lines/10, y = Risk, yend = Risk),
                         linetype="solid",size = segment_size,col="gray50") 
  p <-   p + geom_segment(data = dataUSE2, aes(x =mid , xend = mid, y =Riskend , yend = Risk),size=1.4) 
  p <-  p + geom_point(data = dataUSE2, aes(x =mid ,y = Risk),size = point_size,alpha=.9) +
    scale_y_continuous(limits = c(0, 100), expand = c(0.005, 0.005)) 
  p <- p + facet_grid(tmpall[,rowvar]~1,scales=scalesIN)
  # p <- p + scale_color_gradient2(low = coll[1], mid = coll[round(length(coll)/2)], high = coll[length(coll)],midpoint = max(d2$Risk)/2) 
  p <- p + scale_colour_gradientn(colours = coll)
  p
  
  
  p <- p + theme_light() +
    labs(x=NULL, y=NULL,
         title=titleIN,
         subtitle=subtitleIN,
         caption=captionIN) +
    theme(plot.subtitle=element_text(margin=margin(t = 0, r = 0, b = 20, l = 0))) +
    theme(legend.title=element_blank()) +
    theme(strip.text.y=element_blank()) +
    theme(strip.text.y = element_text(color = "white"))+
    theme(legend.position=lgnpos) +
    theme(legend.key.width = unit(.5, "cm")) +
    theme(legend.text=element_text(size=5)) +
    theme(legend.key.size=unit(.01, "cm")) +
    labs(x= xlabb,y=ylabb)+
    #labs(tag=letters(1:6)) +
    theme(plot.margin=margin(t = 10, r = 10, b = 20, l =10)) 
  
  p <- p+ theme_kir_EBM(sub_title_size=12,
                        sub_title_just="l",
                        plot_margin = margin(plot_marginIN),
                        plot_title_margin = plot_title_marginIN,
                        subtitle_margin = subtitle_marginIN,
                        caption_margin = caption_marginIN,
                        axis_title_just = "cm") +
    labs(x=xlabb, y= ylabb)+
    theme(legend.background = element_rect(colour = NA),
          legend.key = element_rect(colour = "white", fill = NA)) 
  
  p <- p + theme(legend.position=lgnpos)
  p <- p + guides(color = guide_legend(order = 1,nrow = 2))
  p <- p + guides(size = "none", colour = "legend")
  p <- p + guides(shape = guide_legend(title="",nrow = nrowlg[1]))
  p <- p + guides(linetype = guide_legend(nrow = nrowlg[1]))
  p <- p + guides(color = guide_legend(title="",nrow = nrowlg[1]))
  # if(!is.null(xlimmIN[1])) 
  p <- p + xlim(min(tmpall$mid)-.025,max(tmpall$mid)+.025) 
  p <- p + ylim(ylimm[1],ylimm[2]) 
  p <- p + theme(axis.text.x = element_text(angle=angleIN,vjust=xtick_vjust, hjust=xtick_hjust))
  
  xticks <- tapply(tmpall$mid,tmpall[,between_group],mean)   
  
  revy<-revbar
  if(revy){
    p <- p +  scale_x_discrete(name=xlabb,
                               limits = rev(xticks), 
                               labels=rev(names(xticks) ))
  }else{
    p <- p +  scale_x_discrete(name=xlabb,
                               limits = xticks, 
                               labels=names(xticks) )
  }
  
  p
  tmpp<-tmpall
  cc<-intersect( which(tmpp[,between_group]==(levels(tmpp[,between_group]))[arrow_lvl]),
                 which(tmpp[,barsplit]==levels(tmpp[,barsplit])[1]))
  tmpp[-cc,]$Risk<-tmpp[-cc,]$mid<-NA
  
  arrows <- data.frame(tibble(
    x2 = tmpp$mid+.2,
    x1 = max(tmpp$mid,na.rm=T)+arrow_adj1,
    y2 = tmpp$Risk-arrow_yadj,
    y1 = arrow_start1
  ))
  tmpp<-tmpall
  cc<-intersect( which(tmpp[,between_group]==(levels(tmpp[,between_group]))[arrow_lvl]),
                 which(tmpp[,barsplit]==rev(levels(tmpp[,barsplit]))[1]))
  tmpp[-cc,]$Risk<-tmpp[-cc,]$mid<-NA
  
  arrows2 <- data.frame(tibble(
    x2 = tmpp$mid-.1,
    x1 = max(tmpp$mid,na.rm=T)-arrow_adj2,
    y2 = tmpp$Risk+arrow_yadj,
    y1 = arrow_start2
  ))
  arrows21<-arrows2*NA
  arrows21$label<-NA
  arrows21[1,] <- arrows2[cc,][1,] 
  arrows21$y1[1]<-arrows21$y1[1]+yspace[1]
  arrows21$label[1]<-hcrs[2]
  arrows21[2,] <- arrows[cc,][2,] 
  arrows21$label[2]<-hcrs[1]
  arrows21$y1[2]<-arrows21$y1[2]+yspace[2]
  
  
  g_text <- p + geom_curve(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
                           arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
                           color = "gray50", curvature = -0.3,inherit.aes = FALSE)
  g_text <- g_text+ geom_curve(data = arrows2, aes(x = x1, y = y1, xend = x2, yend = y2),
                               arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
                               color = "gray50", curvature = -0.3,inherit.aes = FALSE)
  g_text2 <- g_text+ annotate("text", x =arrows21$x1, y = arrows21$y1,
                              size = 2.8, color = "gray20",
                              label = arrows21$label)
  #label = glue::glue("Worldwide average:\n{round(world_avg, 1)} students per teacher"))
  g_text2    
  
  
}


#GGplot_aclimCEATTLE_risk_droppt2(lgnpos= "bottom",angleIN=0,spacer=1.1,spacer_groups=1.6,spacer_lines=.4,itr=100,
#                                 colvar  ="timeframe",
#                                 between_group = "type",
#                                 barsplit ="hcr")



####_______________________

GGplot_aclimTSv2<-function(
    DAT =ROMSNPZdat,
    hindtype =".raw",
    #futdat = TempC_019_CENaivecf_0_5_12_mc[1,1,,"MIROC_rcp85"],
    futtype =".raw", #c("raw","bc","rc","bcs")
    persistence = TRUE,
    biascorrect= FALSE, 
    threshcol="gray",
    hindset = "aclim_hindcast",
    plotSetIN = list("RCP 4.5" = c("GFDL_rcp45","MIROC_rcp45","CESM_rcp45"),"RCP 8.5" = c("GFDL_rcp85","MIROC_rcp85","CESM_rcp85")),
    h=3,
    w=4.75,
    nrowIN=1,
    futyr=2018:2100,
    refyr=c(2006:2016),
    lgnpos= "bottom",  #c(.95,1.12),
    fn="BottomTemp",
    lwdd=rep(2,13),
    coll=c(colors()[491],col2(6)[c(1,3,5)],col3(6)[c(2,3,6)]),
    ltyy=rep("solid",7),
    ylabb=expression(paste("Bottom Temperature",'( '^{o},"C)")),
    xlabb="Year", 
    xlimmIN = NULL,
    titleIN="",
    captionIN="",
    subtitleIN="",
    projLine=2017,
    threshold = 2.1,
    threshold_range=NULL,
    tline=5,
    talpha=.5,
    talpha2=.2,
    smooth_yr=20,
    add0line=FALSE,
    plot_marginIN= c(1, 1, 1, 1),
    plot_title_marginIN = 0,
    subtitle_marginIN = 0,
    caption_marginIN = 0){

    plotSet   <- plotSetIN
    hindyr    <- DAT[[paste0(hindset,hindtype)]][,1]
    hind      <- DAT[[paste0(hindset,hindtype)]][,fn]
    indx      <- c(hindset,unlist(plotSet))
    futscens  <- paste0(c(hindset,unlist(plotSet)),futtype)
    #futyr<-ROMS_NPZ_covars[[futscens[2]]][,1]
    if(persistence){
      plotSet[[1]]<-c("persistence",plotSet[[1]])
      plotSet[[2]]<-c("persistence",plotSet[[2]])
    }
    
    nfutyr<-length(futyr)
    nscen<-length(futscens)

    allyr<-union(hindyr,futyr) #c(hindyr,futyr)
    datt<-data.frame(matrix(NA,length(allyr),nscen))
    rownames(datt)<-allyr
    colnames(datt)<-indx 
    colnames(datt)[1]<-indx[1]<-"persistence"
    
    datt[,1]<-c(DAT[[futscens[1]]][,fn], rep(NA,nfutyr))
    
      mnHIND<-0
      sdHIND<-1
    #if(biascorrect){
      mnHIND<-mean(DAT[[futscens[1]]][hindyr%in%refyr,fn] )
      sdHIND<-sd(DAT[[futscens[1]]][hindyr%in%refyr,fn] )
    #}
    if(persistence) datt[,1]<-c(DAT[[futscens[1]]][,fn], rep(mnHIND,nfutyr))
    

    for(i in 2:nscen){
      ftyr<-DAT[[futscens[i]]][,"year"]
      sdFUT<-1
      mnfutRef<-0
     if(biascorrect){
        sdFUT<-sd(DAT[[futscens[i]]][ftyr%in%refyr,fn])
        mnfutRef<-mean(DAT[[futscens[i]]][ftyr%in%refyr,fn] )
      }
      tmp<-rep(NA,nfutyr)

      tmp[futyr%in%ftyr]<-(mnHIND+ ((DAT[[futscens[i]]][ftyr%in%futyr,fn]-mnfutRef)*(sdHIND/sdFUT)))
      
      datt[,i]<-c(DAT[[futscens[1]]][,fn], tmp)
    }
    
    datt<-cbind(t=allyr,datt)

    dev.new(height=h,width=w)
      make_plotdat2<-function(datIN=datt){

          mlt<-reshape::melt(datIN[,colnames(datIN)%in%plotSet[[1]]])
          mlt$Year<-datIN[,1]
          mlt$rcp="RCP 4.5 "
          mlt$num<-factor(mlt$variable,levels=unique(plotSet[[1]]) )
          mlt$col<-coll[mlt$num]
          mlt$lty<-ltyy[mlt$num]
          mlt2<-reshape::melt(datIN[,colnames(datIN)%in%(plotSet[[2]])])
          mlt2$Year<-datIN[,1]
          mlt2$rcp="RCP 8.5 "
          mlt2$num<-factor(mlt2$variable,levels=unique(plotSet[[2]]))
          mlt2$col<-coll[mlt2$num]
          mlt2$lty<-ltyy[mlt2$num]
          # mlt2$col<-collIN[length(plotSet[[1]])+2:length(plotSet[[2]])]
          # mlt2$lty<-ltyyIN[length(plotSet[[1]])+2:length(plotSet[[2]])]
          dt<-rbind(mlt,mlt2)
          dt$rcp<-as.factor(dt$rcp)
          
          return(dt)
        }
      dt      <-   make_plotdat2(datt)
      # dt$plotname<-strsplit(mlt$variable,split=futtype)
      dat<-datt
      m_dat   <-   dat
      dat[,2] <-   NA*dat[,2]
      for(i in 3:dim(dat)[2])
       m_dat[,i]  <-  as.numeric(ma2(x=(dat[,i]),n=smooth_yr))
      m_dat2  <-  dat
      for(i in 2:dim(dat)[2])
       m_dat2[,i]  <-  as.numeric(ma2(x=(dat[,i]),n=smooth_yr))

      
      m_dt    <-  make_plotdat2(m_dat)
      m_dt2   <-  make_plotdat2(m_dat2)

      dt$zeroline     <- 0;    dt$projLine    <- projLine
      m_dt$zeroline   <- 0;    m_dt$projLine  <- projLine
      m_dt2$zeroline  <- 0;    m_dt2$projLine <- projLine

      p <-     ggplot(data=dt, aes(x = Year, y = value),colour=variable)
      p <- p + facet_wrap(dt$rcp,nrow=nrowIN) 
      if(add0line)   p <- p + geom_hline(data=dt, aes(yintercept = zeroline),col="lightgray")
      p <- p + geom_vline(data=dt, aes(xintercept=projLine),col="gray",size=1,linetype="dashed") 
      
      if(!is.null(threshold_range[1])) 
        p <- p + geom_ribbon(aes(ymin=threshold_range[1], ymax=threshold_range[2]),fill=threshcol,col=NA, alpha=talpha2)  
      
      if(!is.null(threshold)) p <- p + geom_hline (data=dt, aes(yintercept=threshold),col=threshcol,size=tline, alpha =talpha) 
      p <- p + geom_line(aes(x = Year, y = value,group=variable,colour=variable,linetype=variable),alpha=.6,inherit.aes=TRUE,size=.4)
      # add moving average:
      p <- p + geom_line(data=m_dt,aes(x = Year, y = value,group=variable,colour=variable,linetype=variable),alpha=1,inherit.aes=FALSE,size=.75)
      p <- p + geom_line(data=m_dt2,aes(x = Year, y = value,group=variable,colour=variable,linetype=variable),alpha=1,inherit.aes=FALSE,size=.75)

      p <- p + scale_color_manual(values=c(coll))
      p <- p + scale_linetype_manual(values=ltyy)
      p
      
      p<- p + theme_light() +
      labs(x=NULL, y=NULL,
           title=titleIN,
           subtitle=subtitleIN,
           caption=captionIN) +
      theme(plot.subtitle=element_text(margin=margin(b=20))) +
      theme(legend.title=element_blank()) +
      theme(legend.position=lgnpos) +
      theme(legend.key.width = unit(.5, "cm")) +
      theme(legend.text=element_text(size=5)) +
      theme(legend.key.size=unit(.01, "cm")) +
      labs(x= xlabb,y=ylabb)+
      #labs(tag=letters(1:6)) +
      theme(plot.margin=margin(t = 10, r = 10, b = 10, l =10)) 

      p<- p+ theme_kir_EBM(sub_title_size=12,
                                       sub_title_just="l",
                                       plot_margin = margin(plot_marginIN),
                                       plot_title_margin = plot_title_marginIN,
                                       subtitle_margin = subtitle_marginIN,
                                       caption_margin = caption_marginIN,
                                       axis_title_just = "cm") +
      labs(x=xlabb, y= ylabb)+
      theme(plot.subtitle=element_text(face="bold")) +
      theme(legend.title=element_blank()) 
      p <- p + theme(legend.position=lgnpos)


      p
  }

GGplot_aclimTSv3<-function(
    DAT =ROMSNPZdat,
    hindtype =".raw",
    #futdat = TempC_019_CENaivecf_0_5_12_mc[1,1,,"MIROC_rcp85"],
    futtype =".raw", #c("raw","bc","rc","bcs")
    persistence = TRUE,
    biascorrect= FALSE, 
    threshcol="gray",
    hindset = "aclim_hindcast",
    plotSetIN = list("RCP 4.5" = c("GFDL_rcp45","MIROC_rcp45","CESM_rcp45"),"RCP 8.5" = c("GFDL_rcp85","MIROC_rcp85","CESM_rcp85")),
    h=3,
    w=4.75,
    nrowIN=1,
    futyr=2018:2100,
    refyr=c(2006:2016),
    lgnpos= "bottom",  #c(.95,1.12),
    fn="BottomTemp",
    lwdd=rep(2,13),
    coll=c(colors()[491],col2(6)[c(1,3,5)],col3(6)[c(2,3,6)]),
    ltyy=rep("solid",7),
    ylabb=expression(paste("Bottom Temperature",'( '^{o},"C)")),
    xlabb="Year", 
    xlimmIN = NULL,
    titleIN="",
    captionIN="",
    subtitleIN="",
    projLine=2017,
    threshold = 2.1,
    tline=5,
    talpha=.5,
    smooth_yr=20,
    add0line=FALSE,
    plot_marginIN= c(1, 1, 1, 1),
    plot_title_marginIN = 0,
    subtitle_marginIN = 0,
    caption_marginIN = 0){

    plotSet   <- plotSetIN
    hindyr    <- DAT[[paste0(hindset,hindtype)]][,1]
    hind      <- DAT[[paste0(hindset,hindtype)]][,fn]
    indx      <- c(hindset,unlist(plotSet))
    futscens  <- paste0(c(hindset,unlist(plotSet)),futtype)
    #futyr<-ROMS_NPZ_covars[[futscens[2]]][,1]
    if(persistence){
      plotSet[[1]]<-c("persistence",plotSet[[1]])
      plotSet[[2]]<-c("persistence",plotSet[[2]])
    }
    
    nfutyr<-length(futyr)
    nscen<-length(futscens)

    allyr<-union(hindyr,futyr) #c(hindyr,futyr)
    datt<-data.frame(matrix(NA,length(allyr),nscen))
    rownames(datt)<-allyr
    colnames(datt)<-indx 
    colnames(datt)[1]<-indx[1]<-"persistence"
    
    datt[,1]<-c(DAT[[futscens[1]]][,fn], rep(NA,nfutyr))
    
      mnHIND<-0
      sdHIND<-1
    #if(biascorrect){
      mnHIND<-mean(DAT[[futscens[1]]][hindyr%in%refyr,fn] )
      sdHIND<-sd(DAT[[futscens[1]]][hindyr%in%refyr,fn] )
    #}
    if(persistence) datt[,1]<-c(DAT[[futscens[1]]][,fn], rep(mnHIND,nfutyr))
    

    for(i in 2:nscen){
      ftyr<-DAT[[futscens[i]]][,"year"]
      sdFUT<-1
      mnfutRef<-0
     if(biascorrect){
        sdFUT<-sd(DAT[[futscens[i]]][ftyr%in%refyr,fn])
        mnfutRef<-mean(DAT[[futscens[i]]][ftyr%in%refyr,fn] )
      }
      tmp<-rep(NA,nfutyr)

      tmp[futyr%in%ftyr]<-(mnHIND+ ((DAT[[futscens[i]]][ftyr%in%futyr,fn]-mnfutRef)*(sdHIND/sdFUT)))
      
      datt[,i]<-c(DAT[[futscens[1]]][,fn], tmp)
    }
    
    datt<-cbind(t=allyr,datt)

    dev.new(height=h,width=w)
      make_plotdat2<-function(datIN=datt){

          mlt<-reshape::melt(datIN[,colnames(datIN)%in%plotSet[[1]]])
          mlt$Year<-datIN[,1]
          mlt$rcp="RCP 4.5 "
          mlt$num<-factor(mlt$variable,levels=unique(plotSet[[1]]) )
          mlt$col<-coll[mlt$num]
          mlt$lty<-ltyy[mlt$num]
          mlt2<-reshape::melt(datIN[,colnames(datIN)%in%(plotSet[[2]])])
          mlt2$Year<-datIN[,1]
          mlt2$rcp="RCP 8.5 "
          mlt2$num<-factor(mlt2$variable,levels=unique(plotSet[[2]]))
          mlt2$col<-coll[mlt2$num]
          mlt2$lty<-ltyy[mlt2$num]
          # mlt2$col<-collIN[length(plotSet[[1]])+2:length(plotSet[[2]])]
          # mlt2$lty<-ltyyIN[length(plotSet[[1]])+2:length(plotSet[[2]])]
          dt<-rbind(mlt,mlt2)
          dt$rcp<-as.factor(dt$rcp)
          
          return(dt)
        }
      dt      <-   make_plotdat2(datt)
      # dt$plotname<-strsplit(mlt$variable,split=futtype)
      dat<-datt
      m_dat   <-   dat
      dat[,2] <-   NA*dat[,2]
      for(i in 3:dim(dat)[2])
       m_dat[,i]  <-  as.numeric(ma2(x=(dat[,i]),n=smooth_yr))
      m_dat2  <-  dat
      for(i in 2:dim(dat)[2])
       m_dat2[,i]  <-  as.numeric(ma2(x=(dat[,i]),n=smooth_yr))

      
      m_dt    <-  make_plotdat2(m_dat)
      m_dt2   <-  make_plotdat2(m_dat2)

      dt$zeroline     <- 0;    dt$projLine    <- projLine
      m_dt$zeroline   <- 0;    m_dt$projLine  <- projLine
      m_dt2$zeroline  <- 0;    m_dt2$projLine <- projLine

      p <-     ggplot(data=dt, aes(x = Year, y = value),colour=variable)
      p <- p + facet_wrap(dt$rcp,nrow=nrowIN) 
      p<- p+ facet_zoom(x = Year==between(Year, 2018, 2040, incbounds = TRUE) )
      if(add0line)   p <- p + geom_hline(data=dt, aes(yintercept = zeroline),col="lightgray")
      p <- p + geom_vline(data=dt, aes(xintercept=projLine),col="gray",size=1,linetype="dashed") 
      if(!is.null(threshold)) p <- p + geom_hline (data=dt, aes(yintercept=threshold),col=threshcol,size=tline, alpha =talpha) 
      p <- p + geom_line(aes(x = Year, y = value,group=variable,colour=variable,linetype=variable),alpha=.6,inherit.aes=TRUE,size=.4)
      # add moving average:
      p <- p + geom_line(data=m_dt,aes(x = Year, y = value,group=variable,colour=variable,linetype=variable),alpha=1,inherit.aes=FALSE,size=.75)
      p <- p + geom_line(data=m_dt2,aes(x = Year, y = value,group=variable,colour=variable,linetype=variable),alpha=1,inherit.aes=FALSE,size=.75)

      p <- p + scale_color_manual(values=c(coll))
      p <- p + scale_linetype_manual(values=ltyy)
      p
      
      p<- p + theme_light() +
      labs(x=NULL, y=NULL,
           title=titleIN,
           subtitle=subtitleIN,
           caption=captionIN) +
      theme(plot.subtitle=element_text(margin=margin(b=20))) +
      theme(legend.title=element_blank()) +
      theme(legend.position=lgnpos) +
      theme(legend.key.width = unit(.5, "cm")) +
      theme(legend.text=element_text(size=5)) +
      theme(legend.key.size=unit(.01, "cm")) +
      labs(x= xlabb,y=ylabb)+
      #labs(tag=letters(1:6)) +
      theme(plot.margin=margin(t = 10, r = 10, b = 10, l =10)) 

      p<- p+ theme_kir_EBM(sub_title_size=12,
                                       sub_title_just="l",
                                       plot_margin = margin(plot_marginIN),
                                       plot_title_margin = plot_title_marginIN,
                                       subtitle_margin = subtitle_marginIN,
                                       caption_margin = caption_marginIN,
                                       axis_title_just = "cm") +
      labs(x=xlabb, y= ylabb)+
       theme(plot.subtitle=element_text(face="bold")) +
      theme(legend.title=element_blank()) 

      p <- p + theme(legend.position=lgnpos)


      p
     

  }
 

  #-------------------------------------
  # GGplot_aclimCEATTLE
  #-------------------------------------
  #' PLot ACLIM CEATTLE projections covariates using GGplot
  #'
  #' @param dat is a data.frame with t=date and dat=data to plot
  #' @param plotSet is the columns of dat that you want to plot (col=1 is the date, 2:ncol are options)
  #' @param lwdd vector of the line widths; length of the projection simulation set + 1 for hindcast (dim of dat)
  #' @param ltyy vector of the line types; length of the projection simulation set + 1 for hindcast (dim of dat)
  #' @param ylab y axis label
  #' @param xlab x axis label
  #' @param title title for the graph
  #' @param coll vector colors for each line; length of the projection simulation set + 1 for hindcast (dim of dat)
  #' @param esnmCol colors for each ensemble polygon
  #' @param esnmSet is a list with the set for each ensemble groups
  #' @param alpha vector of transparencies of the ensemble mean; corresponds to number quantiles
  #' @param add0line Add a horizontal line at 0?
  #' @keywords Temperature, plot, data, ACLIM
  #' @export 
  #' @examples 
  #  
  #577

          #  "###########################################################"  



GG_HCRplot<-function(h=4,w=7,lgnpos="none",futScen="persistence",ylimm2=c(0,1.2),fontSize=.8,yfont=c(2070,2080)){
  dev.new(height=h,width=w)
        noF <-data.frame(F="Unfished",Year=B0_0_5_3[[1]]$Year,B=B0_0_5_3[[1]]$persistence)
        Fished   <-data.frame(F="Fished",Year=B_0_5_3[[1]]$Year,B=B_0_5_3[[1]][,futScen])
        
        noF$B40=noF$B*.40;noF$B40[noF$Year<2017]<-NA
        Fished$B40=noF$B*.40;Fished$B40<-NA
        noF$B35=noF$B*.35;noF$B35[noF$Year<2017]<-NA
        Fished$B35=noF$B*.35;Fished$B35<-NA

        Bdat<-as_tibble(rbind(noF,  Fished))
        Bdat$F<-factor(Bdat$F,levels=c("Unfished","Fished"))


        ylabb   = "Spawning biomass (million tons)"
        xlabb   = "Year" 
        plot_marginIN         = c(1,1, .5, .5)
        plot_title_marginIN = 0
        subtitle_marginIN   = 0
        caption_marginIN    = 0
        coll    = c(colors()[320],col2(6)[c(2,3,4)],col3(6)[c(3,4,6)])

        p <-     ggplot(data=Bdat, aes(x = Year, y = B),colour=Bdat$F)
        p <- p + geom_vline(data=Bdat, aes(xintercept=2017),col="gray",size=1,linetype="dashed") 
        p <- p + geom_hline(data=noF, aes(yintercept=tail(noF$B)[1]),col=coll[1],size=.5,linetype="dashed") 
        p <- p + geom_hline(data=noF, aes(yintercept=tail(noF$B)[1]*.32),col=coll[2],size=.5,linetype="dashed") 
        p <- p + geom_hline(data=noF, aes(yintercept=tail(noF$B)[1]*.40),col=coll[3],size=.5,linetype="dashed") 
        #p <- p + geom_hline(data=Fished, aes(yintercept=tail(Fished$B)[1]),col=coll[4],size=.5,linetype="dashed") 

        p <-  p+ geom_line(data=Bdat, aes(x = Year, y = B,colour=Bdat$F),size=1)
        # p <-  p+ geom_line(data=Bdat, aes(x = Year, y = B40,colour=Bdat$F),size=1)
        # p <-  p+ geom_line(data=Bdat, aes(x = Year, y = B35,colour=Bdat$F),size=1) 

        p <- p + scale_color_manual(values=c(coll[c(1,4)]))

        p <- p + theme_light() +
        labs(x=NULL, y=NULL) +
        theme(plot.subtitle=element_text(margin=margin(b=20))) +
        theme(legend.title=element_blank()) +
        theme(legend.key.width = unit(.5, "cm")) +
        theme(legend.text=element_text(size=5)) +
        theme(legend.key.size=unit(.01, "cm")) +
        labs(x= xlabb,y=ylabb)+
        #labs(tag=letters(1:6)) +
        theme(plot.margin=margin(t = 10, r = 10, b = 10, l =10)) 

        p<- p+ theme_kir_EBM(sub_title_size=12,
                                       sub_title_just="l",
                                       
                                       axis_title_just = "cm") +
              labs(x=xlabb, y= ylabb,subtitle="A) Biological reference points") +
              theme(legend.title=element_blank(),
              legend.background = element_rect(colour = NA),
              legend.key = element_rect(colour = "white", fill = NA)) 
        p <- p + theme(legend.position=lgnpos)
        p <- p + guides(fill = FALSE)
        p <- p + guides(color = guide_legend(order = 1))
        p <- p + guides(color=guide_legend(override.aes=list(fill=NA)))
        p <- p + ylim(0,2e07) 
        p <- p + xlim(1975,2120) 

        p

        p <-  p + annotate("text", x = 2115, y = tail(noF$B)[1]+1e6,  label = "paste(italic(B[0]))", parse = TRUE)
       # p <-  p + annotate("text", x = 2116, y = tail(Fished$B)[1]+1e6,  label = "paste(B[target])", parse = TRUE)
        p <-  p + annotate("text", x = 2116, y = tail(noF$B)[1]*.32-1e6,  label = "paste(italic(B[35]))", parse = TRUE)
        p <-  p + annotate("text", x = 2116, y = tail(noF$B)[1]*.40+1e6,  label = "paste(italic(B[40]))", parse = TRUE)
        
        p <-  p + annotate("text", x = yfont[1], y = tail(noF$B)[1]+2e6,     label = "paste('without climate; ',italic(F),' = 0')",  color=coll[1],parse = T,size=fontSize)
        p <-  p + annotate("text", x = yfont[2], y = tail(Fished$B)[1]+3e6,  label = "paste('with climate; ',italic(F),' = ' , italic(F[target]))", color=coll[4],parse = T,size=fontSize)

        # p <-  p + annotate("segment", x=2105, y=tail(noF$B)[1]+1e6,xend=2100,yend=tail(noF$B)[1]+.1e6, size=.5, arrow=arrow(length=unit(.2, “cm”)))
        p <-  p + geom_segment(aes(x=2110, y=tail(noF$B)[1]+1e6,xend=2105,yend=tail(noF$B)[1]+.1e6),arrow = arrow(length = unit(0.1,"cm"))) 
        p <-  p + geom_segment(aes(x=2110, y=tail(noF$B)[1]*.32-1e6,xend=2105,yend=tail(noF$B)[1]*.32-.1e6),arrow = arrow(length = unit(0.1,"cm"))) 
        p <-  p + geom_segment(aes(x=2110, y=tail(noF$B)[1]*.40+1e6,xend=2105,yend=tail(noF$B)[1]*.40+.1e6),arrow = arrow(length = unit(0.1,"cm"))) 

      #  p <-  p + geom_segment(aes(x=2110, y=tail(Fished$B)[1]+1e6,xend=2105,yend=tail(Fished$B)[1]),arrow = arrow(length = unit(0.1,"cm"))) 
        #p <- p + ggtitle("Biological reference points")
       
        # This example uses the ChickWeight dataset, which comes with ggplot2
        # First plot
        slopingHCR<-function(Ftarget=tail(Fished$B)[1]/tail(noF$B)[1],Bratio=.2,alpha=.05,Cbeta=.2)
        {
          maxFabc<-0*Bratio
          
          if(any(Bratio<1)){
            
            if(any(Bratio>=alpha)) 
               maxFabc[Bratio>=alpha]<-Ftarget*((Bratio[Bratio>=alpha]-alpha)/(1-alpha))
            if(any(Bratio<alpha))
              maxFabc[Bratio<alpha]<-0
            if(any(Bratio<=Cbeta))
              maxFabc[Bratio<=Cbeta]<-0
          }
          if(any(Bratio>=1))
            maxFabc[Bratio>1]<-Ftarget
          
          return(maxFabc)
        }
        Ftarget<-tail(Fished$B)[1]/tail(noF$B)[1]

        Bratio<-seq(0,2,.001)
        dat2<-data.frame(
        Bratio=Bratio*tail(Fished$B)[1],
        Feffective=slopingHCR(Ftarget=Ftarget,Bratio=Bratio,alpha=.05,Cbeta=.05),
        FeffectiveEBM=slopingHCR(Ftarget=Ftarget,Bratio=Bratio,alpha=.05,Cbeta=.2))
        
        dat2<-data.frame(
        Bratio=Bratio,
        Feffective=slopingHCR(Ftarget=Ftarget,Bratio=Bratio,alpha=.05,Cbeta=.05)/Ftarget,
        FeffectiveEBM=slopingHCR(Ftarget=Ftarget,Bratio=Bratio,alpha=.05,Cbeta=.2)/Ftarget)


        # Second plot
        p2 <- ggplot(dat2, aes(x=Bratio, y=Feffective)) 

         
        p2 <- p2 + geom_hline(data=dat2, aes(yintercept=1),col=coll[1],size=.5,linetype="dashed") 
        p2 <- p2 + geom_vline(data=Fished, aes(xintercept=1),col=coll[1],size=.5,linetype="dashed") 
        p2 <- p2 + geom_vline(data=Fished, aes(xintercept=.2),col=coll[1],size=.5,linetype="dashed") 

        p2 <-  p2+ geom_line(data=dat2, aes(x = Bratio, y=Feffective),col=coll[2],size=1,linetype="dashed") 
        p2 <-  p2+ geom_line(data=dat2, aes(x = Bratio, y=FeffectiveEBM),col=coll[4],size=1)

        p2 <- p2 + scale_color_manual(values=c(coll[c(1,4)]))
        p2 <- p2 + theme_light() +
        labs(x=NULL, y=NULL) +
        theme(legend.position=lgnpos)+
        theme(plot.subtitle=element_text(margin=margin(b=20))) +
        theme(legend.title=element_blank()) +
        theme(legend.key.width = unit(.5, "cm")) +
        theme(legend.text=element_text(size=5)) +
        theme(legend.key.size=unit(.01, "cm")) +
        #labs(tag=letters(1:6)) +
        theme(plot.margin=margin(t = 10, r = 10, b = 10, l =10)) 

        p2<- p2+ theme_kir_EBM(sub_title_size=12,
                                       sub_title_just="l",
                                       axis_title_just = "cm") +
              labs(x=expression(italic(B[y])/italic(B[40])), y=expression(~italic(F[y])/italic(F[target])),subtitle="B) Sloping harvest control rule") +
              theme(legend.title=element_blank(),
              legend.background = element_rect(colour = NA),
              legend.key = element_rect(colour = "white", fill = NA)) 
        p2 <- p2 + guides(fill = FALSE)
        p2 <- p2 + guides(color = guide_legend(order = 1))
        p2 <- p2 + guides(color=guide_legend(override.aes=list(fill=NA)))
        p2 <- p2 + ylim(ylimm2[1],ylimm2[2]) 

        #p2<- p2 + xlim(0,1.2) 
        p2


        multiplot(p, p2,cols=2) 
   
}

  
GGplot_aclimCEATTLE_BarPlot_delta<-function(
    esm_namesIN=simnames,
    nmLIST  = list("2 MT cap"="dat_2MT_219_CENaivecf1_2_5_13","no cap"="dat_219_CENaivecf_2_5_12"),
    datLIST = list(dat1=B0_2MT_219_CENaivecf1_2_5_13_mc,dat2=B0_219_CENaivecf_2_5_12_mc),
    valLIST = list(valIn1="SSB0_total_biom", valIn2="SSB0_total_biom"),
    deltaIN = FALSE,
    plotSet = list("RCP 4.5" = c(1,rcp45_n_sim),"RCP 8.5" = c(1,rcp85NoBio_n_sim)),
    h       = 3,
    w       = 4.75,
    plotpersist = TRUE,
    ylimm_up    = c(20,2,1.5)*1e6,
    ylimm_dwn   = c(0,0,0),
    xlimmIN = NULL,
    scalesIN= "free_y",
    lgnpos  = "bottom",
    fn      = "BT",
    ltyy    = c("solid","solid"),
    lwdd    = c(.7,.4),
    coll    = c(colors()[320],col2(6)[c(2,3,4)],col3(6)[c(3,4,6)]),
    ylabb   = "Spawning Biomass (million tons)",
    xlabb   = "Year", 
    nspp    = 3,
    ydiv    = 1e6,
    titleIN = "",
    captionIN  = "",
    subtitleIN = "",
    projLine   = 2017,
    smooth_yr  = 20,
    add0line   = FALSE,
    alpha      = c(20,40), 
    prob       = c(.1,.50,.9),
    plot_marginIN         = c(1, 1, 1, 1),
      plot_title_marginIN = 0,
      subtitle_marginIN   = 0,
      caption_marginIN    = 0
    ){

        dev.new(height=h,width=w)
        ndat   <- length(nmLIST)
        DAT    <- list()
        for(ll in 1:ndat)
          eval(parse(text = paste0("DAT[[ll]]<-grabDat(datIn=",nmLIST[ll],",valIn='",valLIST[ll],"')") ))
        nspp   <-  length(DAT[[1]])

        if(deltaIN)
          for(ll in 1:ndat)
            for(sp in 1:nspp)
              DAT[[ll]][[sp]][,-(1)]  <-  100*(DAT[[ll]][[sp]][,-(1)]-DAT[[ll]][[sp]][,2])/DAT[[ll]][[sp]][,2]
            
        yr     <-  DAT[[ll]][[1]]$Year
        nset   <-  length(plotSet)  # number of contrasts
        maxx   <-  0
        for(i in 1:length(plotSet))
          maxx <-  max(length(plotSet[[i]]),maxx)

        splevels        <- paste0(letters[1:3],") ",c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        #splevels<-c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        correctOrder    <- c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])  # system constant in correct sort order.
        correctOrder    <- factor(paste(1:length(correctOrder),correctOrder))
        correctOrderLab <- c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])

        eval(parse(text= paste0("qnt1<-qnt2<-data.frame(Year=NA,sp=NA,scen=NA,scenario=NA,HCR=NA,scenario_hcr=NA,rcp=NA,order=NA,",
        paste0('prob',prob*100,'=NA',collapse=","),",",paste0('mnhind_prob',prob*100,'=NA',collapse=","),
        ",meanHindRun=NA,meanFutRun=NA,coll=NA)")))
        
        for(ll in 1:ndat){
          kk  <-  1
          dat1<-datLIST[[ll]]
          for(i  in 1:nset){
            for(jj in 1:length(plotSet[[i]])){
              kk  <-  kk + 1
              for(sp in 1:nspp){
                  tq      <-  t(exp(apply(log(dat1[sp,,,plotSet[[i]][jj]]),2,quantile,probs=prob)))
                if(deltaIN)  
                  tq      <-  t((apply(100*( dat1[sp,,,plotSet[[i]][jj]]  - dat1[sp,,,1]  )/dat1[sp,,,1] ,2,quantile,probs=prob)))
                  
                  mnhind  <-  t(exp(apply(log(dat1[sp,,,1]),2,quantile,probs=prob)))
                if(deltaIN)  
                  mnhind  <-  mnhind*0

                if(any(tq > ylimm_up[sp]))  
                  tq[tq > ylimm_up[sp]]  <-  ylimm_up[sp]

                if(any(tq < ylimm_dwn[sp]))  
                  tq[tq < ylimm_dwn[sp]]  <-  ylimm_dwn[sp]
            
                if(any(mnhind  > ylimm_up[sp]))  
                 mnhind[mnhind > ylimm_up[sp]]  <-  ylimm_up[sp]

               if(any(mnhind  < ylimm_dwn[sp]))  
                 mnhind[mnhind < ylimm_dwn[sp]]  <-  ylimm_dwn[sp]

                tmpq1  <-  data.frame(
                  Year=as.numeric(rownames(tq)),
                  sp=factor(paste0(letters[sp],") ",sppINFO[[sp]]$plotSPP),levels=splevels),
                  scen=esm_namesIN[plotSet[[i]]][jj],
                  scenario = correctOrder[grep(esm_namesIN[plotSet[[i]]][jj],correctOrder)], 
                  HCR = names(nmLIST)[ll],
                  scenario_hcr = paste(correctOrder[grep(esm_namesIN[plotSet[[i]]][jj],correctOrder)],names(nmLIST)[ll]), 
                  rcp = names(plotSet)[i],
                  order = grep(esm_namesIN[plotSet[[i]]][jj],correctOrder),
                  tq,mnhind,
                  DAT[[ll]][[sp]][,-1][, plotSet[[i]][1] ],
                  DAT[[ll]][[sp]][,-1][, plotSet[[i]][jj] ],
                  coll=coll[kk])

                #cat(jj,"/",plotSet[[i]][jj],"/",coll[kk],"/", cat(kk),"\n")
                colnames(tmpq1)<-colnames(qnt1)
                qnt1<-rbind(qnt1,tmpq1)
              }
            }
          }
        }
        qnt<-qnt1[-1,]
        qnt$projLine<-projLine
        qnt$scen = factor(qnt$scen) 
        
        nHCR <-  length(names(nmLIST))
        hcr  <-  names(nmLIST)        
        mnNAME<-paste0('prob',prob[((length(prob)-1)/2)+1]*100,collapse=",")

        p <-     ggplot(data=qnt[qnt$HCR==hcr[1],], aes(x = Year, y = meanFutRun/ydiv),colour=scenario)
        p <- p + geom_bar(stat="identity")
        p <- p + facet_grid(qnt[qnt$HCR==hcr[1],]$sp~qnt[qnt$HCR==hcr[1],]$rcp,scales=scalesIN) 

        # if(add0line)   p <- p + geom_hline(data=qnt, aes(yintercept = zeroline),col="lightgray")
        p <- p + geom_vline(data=qnt[qnt$HCR==hcr[1],], aes(xintercept=projLine),col="gray",size=1,linetype="dashed") 
        #p <- p + geom_line(aes(x = Year, y = prob50/ydiv,group=scen,colour=scen,linetype="solid"),alpha=1,inherit.aes=TRUE,size=.4)
        # add moving average:
        probNames<-paste0('prob',prob*100)
        mnprobNames<-paste0('mnhind_prob',prob*100)
        
        for(j in 1:nHCR){
          subqnt<-qnt[qnt$HCR==hcr[j],]
          if(plotpersist){
            for(i in 1:((length(prob)-1)/2))
              eval(parse(text=paste0(" p <- p + geom_ribbon(data=subqnt, aes(x=Year,ymin = ",mnprobNames[i],"/ydiv, ymax = ",mnprobNames[rev(1:length(prob))[i]],"/ydiv,colour=scenario,fill = scenario), alpha=alpha[2]/100,size=0,inherit.aes=FALSE)")))
          }
          for(i in 1:((length(prob)-1)/2))
          eval(parse(text=paste0(" p <-  p + geom_ribbon(data=subqnt, aes(x=Year,ymin = ",probNames[i],"/ydiv, ymax = ",probNames[rev(1:length(prob))[i]],"/ydiv,group=scenario,fill = scenario), alpha=alpha[1]/100,size=0,inherit.aes=FALSE)")))
          coll2<-coll
        }
        if(plotpersist) {
          p <-  p+ geom_line(data=qnt, aes(x = Year, y = meanHindRun/ydiv,colour=scenario,linetype=HCR,size=HCR))
        }
        p <- p + geom_bar(data=qnt, aes(x = Year, y = meanFutRun/ydiv,colour=scenario,linetype=HCR,size=HCR),alpha=1,inherit.aes=TRUE)

        p <- p + geom_line(data=qnt, aes(x = Year, y = meanFutRun/ydiv,colour=scenario,linetype=HCR,size=HCR),alpha=1,inherit.aes=TRUE)
          
        p <- p + scale_color_manual(values=coll2)
        p <- p + scale_fill_manual(values=coll2, name="fill")
        #p <- p + guides(color = guide_legend(order = 2))
      
        p <- p + scale_linetype_manual(values=ltyy)
        p <- p + scale_size_manual(values=lwdd)
        

        p<- p + theme_light() +
        labs(x=NULL, y=NULL,
           title=titleIN,
           subtitle=subtitleIN,
           caption=captionIN) +
        theme(plot.subtitle=element_text(margin=margin(b=20))) +
        theme(legend.title=element_blank()) +
        theme(legend.position=lgnpos) +
        theme(legend.key.width = unit(.5, "cm")) +
        theme(legend.text=element_text(size=5)) +
        theme(legend.key.size=unit(.01, "cm")) +
        labs(x= xlabb,y=ylabb)+
        #labs(tag=letters(1:6)) +
        theme(plot.margin=margin(t = 10, r = 10, b = 10, l =10)) 

        p<- p+ theme_kir_EBM(sub_title_size=12,
                                       sub_title_just="l",
                                       plot_margin = margin(plot_marginIN),
                                       plot_title_margin = plot_title_marginIN,
                                       subtitle_margin = subtitle_marginIN,
                                       caption_margin = caption_marginIN,
                                       axis_title_just = "cm") +
              labs(x=xlabb, y= ylabb)+
              theme(legend.title=element_blank(),
              legend.background = element_rect(colour = NA),
              legend.key = element_rect(colour = "white", fill = NA)) 
        p <- p + theme(legend.position=lgnpos)
        p <- p + guides(fill = FALSE)
        p <- p + guides(color = guide_legend(order = 1))
        p <- p + guides(color=guide_legend(override.aes=list(fill=NA)))
        if(!is.null(xlimmIN[1])) 
          p <- p + xlim(xlimmIN[1],xlimmIN[2]) 

        p

}
  

GGplot_aclimCEATTLE_risk<-function(
    esm_namesIN=simnames,
    RISK    = list("no cap" = risk12,"2 MT cap" = risk13),
    RISKTYPES = riskTypes[c(1,3)],
    modeIN  = c("SSM","MSM"),
    rcpIN   = c("RCP 4.5"="rcp45","RCP 8.5"="rcp85"),
    deltaIN = FALSE,
    plotSet = list("RCP 4.5" = c(1,rcp45_n_sim),"RCP 8.5" = c(1,rcp85NoBio_n_sim)),
    colvar="rcp",
    rowvar="sp",
    h       = 3,
    w       = 4.75,
    spIN    = 1:3,
    plotMEAN = FALSE,
    plotpersist = TRUE,
    ylimm_up    = c(20,2,1.5)*1e6,
    ylimm_dwn   = c(0,0,0),
    xlimmIN = NULL,
    pchh    = c(2,3),
    sizeIN  = 6,
    scalesIN= "free_y",
    lgnpos  = "bottom",
    fn      = "BT",
    ltyy    = c("solid","solid"),
    lwdd    = c(.7,.4),
    coll    = c(colors()[320],col2(6)[c(2,3,4)],col3(6)[c(3,4,6)]),
    xlabb   = "Risk of decline in catch", 
    ylabb   = "Risk of decline in biomass", 
    nspp    = 3,
    nrowlg  = c(2,2),
    ydiv    = 1e6,
    titleIN = "",
    captionIN  = "",
    subtitleIN = "",
    projLine   = 2017,
    smooth_yr  = 20,
    add0line   = FALSE,
    alpha      = c(.4,1), 
    prob       = c(.1,.50,.9),
    plot_marginIN         = c(1, 1, 1, 1),
      plot_title_marginIN = 0,
      subtitle_marginIN   = 0,
      caption_marginIN    = 0
    ){

        tmp1<-as_tibble(RISK[[1]])%>%filter(sp%in%spIN,mode%in%modeIN, type%in%RISKTYPES,rcp%in%rcpIN)
        tmp2<-as_tibble(RISK[[2]])%>%filter(sp%in%spIN,mode%in%modeIN, type%in%RISKTYPES,rcp%in%rcpIN)
        

        hcrs<-names(RISK)
        tmp1$hcr<- hcrs[1]
        tmp2$hcr<-hcrs[2]
        tmp1$Size<-inv.logit((as.numeric(scale(1/((tmp1$riskCcv+tmp1$riskBcv))))))
        tmp2$Size<-inv.logit(as.numeric(scale(1/((tmp2$riskCcv+tmp2$riskBcv)))))
        tmp1$Size[is.na(tmp1$Size)]<-1
        tmp2$Size[is.na(tmp2$Size)]<-1
        tmp1<-data.frame(tmp1)
        tmp2<-data.frame(tmp2)
        tmpall<-rbind(tmp1,tmp2)


        dev.new(height=h,width=w)
        maxx   <-  0
        for(i in 1:length(plotSet))
          maxx <-  max(length(plotSet[[i]]),maxx)

       
        splevels        <- paste0(letters[1:3],") ",c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        splevels        <- paste0(letters[1:3],") ",c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        #splevels        <- paste0(c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        #splevels<-c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        correctOrder    <- c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])  # system constant in correct sort order.
        correctOrder    <- factor(paste(1:length(correctOrder),correctOrder))
        correctOrderLab <- c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])

        tmpall$sp<-splevels[tmpall$sp]
        tmp1$sp<-splevels[tmp1$sp]
        tmp2$sp<-splevels[tmp2$sp]

        
        p <-     ggplot(data=tmp1, aes(x = riskC, y = riskB,colour=timeframe))
        p <- p + facet_grid(tmp1[,rowvar]~tmp1[,colvar],scales=scalesIN) 
        
        p <- p + geom_point(data=tmpall,aes(x = riskC,y = riskB, colour=timeframe,shape=hcr),size=tmpall$Size*sizeIN,inherit.aes=TRUE,alpha = alpha[1])
        p <- p + geom_point(data=tmpall,aes(x = riskC,y = riskB, colour=timeframe,shape=hcr),colour="white",size=.9,inherit.aes=TRUE)
       
        if(plotMEAN){
           p <- p + geom_segment(data=tmp1, aes( 
            x = (tmp1$riskCmn)   , y    = (tmp1$riskBmn),    
            xend = (tmp2$riskCmn), yend = (tmp2$riskBmn), colour = timeframe,linetype=factor(type),group = type),
            size=1,alpha=alpha[2],arrow = arrow(length = unit(5, "points"),type="open", angle = 40)  )
       
         }else{
          p <- p + geom_segment( data=tmp1,aes( 
            x = tmp1$riskC,y = tmp1$riskB, 
            xend = tmp2$riskC,yend = tmp2$riskB,  
          colour = timeframe,linetype=factor(type),group = type),inherit.aes=FALSE,
          size=1,alpha=alpha[2],arrow = arrow(length = unit(5, "points"),type="open", angle = 40)  )
          p
         }
         
 
        p
        
        p <- p + scale_color_manual(values=coll)
        p <- p + scale_fill_manual(values=coll, name="fill")
        #p <- p + guides(color = guide_legend(order = 2))
      
        p <- p + scale_linetype_manual(values=ltyy)
        p <- p + scale_size_manual(values=as.numeric(lwdd))
        p <- p + scale_shape_manual(values=as.numeric(pchh))

        p<- p + theme_light() +
        labs(x=NULL, y=NULL,
           title=titleIN,
           subtitle=subtitleIN,
           caption=captionIN) +
        theme(plot.subtitle=element_text(margin=margin(b=20))) +
        theme(legend.title=element_blank()) +
        theme(strip.text.y=element_blank()) +
        theme(strip.text.y = element_text(color = "white"))+

        theme(legend.position=lgnpos) +
        theme(legend.key.width = unit(.5, "cm")) +
        theme(legend.text=element_text(size=5)) +
        theme(legend.key.size=unit(.01, "cm")) +
        labs(x= xlabb,y=ylabb)+
        #labs(tag=letters(1:6)) +
        theme(plot.margin=margin(t = 10, r = 10, b = 10, l =10)) 

        p<- p+ theme_kir_EBM(sub_title_size=12,
                                       sub_title_just="l",
                                       plot_margin = margin(plot_marginIN),
                                       plot_title_margin = plot_title_marginIN,
                                       subtitle_margin = subtitle_marginIN,
                                       caption_margin = caption_marginIN,
                                       axis_title_just = "cm") +
              labs(x=xlabb, y= ylabb)+
              theme(legend.title=element_blank(),
              legend.background = element_rect(colour = NA),
              legend.key = element_rect(colour = "white", fill = NA)) 

        p <- p + theme(legend.position=lgnpos)
        p <- p + guides(fill = FALSE)
        p <- p + guides(color = guide_legend(order = 1,nrow = 2))
        p <- p + guides(color = guide_legend(override.aes=list(fill=NA)))
        p <- p + guides(size = "none", colour = "legend",linetype="none")
        p <- p + guides(shape = guide_legend(nrow = nrowlg[1]))
        p <- p + guides(color = guide_legend(nrow = nrowlg[1]))
        # if(!is.null(xlimmIN[1])) 
        p <- p + xlim(-10,110) 
        p <- p + ylim(-10,110) 

        p


}

GGplot_aclimCEATTLE_riskBAR<-function(
  esm_namesIN=simnames,
  RISK    = list("no cap" = risk12,"2 MT cap" = risk13),
  RISKTYPES = riskTypes[c(1,2,3)],
  modeIN  = c("MSM"),
  rcpIN   = c("RCP 8.5"="rcp85"),
  deltaIN = FALSE,
  plotSet = list("RCP 8.5" = c(1,rcp85NoBio_n_sim)),
  riskIN  = "riskC",
  colvar  ="type",
  rowvar  ="sp",
  barsplit ="hcr",
  revbar  = TRUE,
  h       = 3,
  w       = 4.75,
  spIN    = 1,
  alphaIN =.02,
  scalesIN= "free_y",
  angleIN = 90,
  xtick_hjust = .8,
  xtick_vjust = .5,
  lgnpos  = "bottom",
  coll    = col3(6)[c(2,3,6)],
  ylabb   = "Risk of decline in catch", 
  xlabb   = "Time period", 
  nrowlg  = c(2,2),
  titleIN = "",
  captionIN  = "",
  subtitleIN = "",
  plot_marginIN         = c(1, 1, 1, 1),
  backcol = "gray",
  plot_title_marginIN = 0,
  subtitle_marginIN   = 0,
  caption_marginIN    = 0
){
  
  tmp1      <-  as_tibble(RISK[[1]])%>%filter(sp%in%spIN,mode%in%modeIN, type%in%RISKTYPES,rcp%in%rcpIN)
  tmp2      <-  as_tibble(RISK[[2]])%>%filter(sp%in%spIN,mode%in%modeIN, type%in%RISKTYPES,rcp%in%rcpIN)
  hcrs      <-  names(RISK)
  tmp1$hcr  <-  hcrs[1]
  tmp2$hcr  <-  hcrs[2]
  eval(parse(text=paste0("tmp1$Size <-  inv.logit((as.numeric(scale(1/((tmp1$",riskIN,"cv))))))")))
  eval(parse(text=paste0("tmp2$Size <-  inv.logit(as.numeric(scale(1/((tmp2$",riskIN,"cv)))))")))
  tmp1$Size[is.na(tmp1$Size)]  <-  1
  tmp2$Size[is.na(tmp2$Size)]  <-  1
  tmp1      <-  data.frame(tmp1)
  tmp2      <-  data.frame(tmp2)
  tmpall    <-  rbind(tmp1,tmp2)
  
  dev.new(height=h,width=w)
  maxx      <-  0
  for(i in 1:length(plotSet))
    maxx    <-  max(length(plotSet[[i]]),maxx)
  
  
  splevels        <-  paste0(letters[1:3],") ",c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
  splevels        <-  paste0(letters[1:3],") ",c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
  if(length(plotSet)>1){
    correctOrder    <-  c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])  # system constant in correct sort order.
    correctOrder    <-  factor(paste(1:length(correctOrder),correctOrder))
    correctOrderLab <-  c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])
  }
  
  tmpall$sp       <-  splevels[tmpall$sp]
  tmp1$sp         <-  splevels[tmp1$sp]
  tmp2$sp         <-  splevels[tmp2$sp]
  Ybins           <-  seq(0,100,10)
  binIT           <-  function(x,bins=seq(0,1000,10)){unlist(lapply(x,function(x) bins[rev(which(x>=bins))[1]] ))}
  tmp1$riskbin    <-  binIT(tmp1$riskB,bins=Ybins)
  tmpall$riskbin  <-  binIT(tmpall$riskB,bins=Ybins)
  set.seed(999)
  xvar        <-  c(rnorm(1500, mean = -1), rnorm(1500, mean = 1.5))
  yvar        <-  c(rnorm(1500, mean = 1), rnorm(1500, mean = 1.5))
  zvar        <-  as.factor(c(rep(1, 1500), rep(2, 1500)))
  xy          <-  data.frame(xvar, yvar, zvar)
  itr         <-  100
  jitterTMP   <-  data.frame(timeframe=NA,hcr=NA,rcp=NA,sp=NA,mode=NA,type=NA,riskbin=NA,riskB=NA)
  for( i in 1:dim(tmpall)[1])
    jitterTMP  <-  rbind(jitterTMP,data.frame(timeframe=tmpall$timeframe[i],
                                              rcp=tmpall$rcp[i],
                                              hcr=tmpall$hcr[i],
                                              sp=tmpall$sp[i],
                                              mode=tmpall$mode[i],
                                              type=tmpall$type[i],
                                              riskbin=tmpall$riskbin[i],
                                              riskB=rnorm(itr, mean = tmpall$riskB,sd=tmpall$riskBsd)))
  
  jitterTMP            <-  jitterTMP[-1,]
  jitterTMP$timeframe  <-  factor(jitterTMP$timeframe,levels=levels(tmp1$timeframe))
  jitterTMP$riskbin    <-  binIT(jitterTMP$riskB,bins=Ybins)
  
  
  d    <-  data.frame(x = as.numeric(tmpall$timeframe), y = tmpall[,riskIN])
  # interpolate values from zero to y and create corresponding number of x values
  vals <-  lapply(d$y, function(y) seq(0, y, by = 0.1))
  y    <-  unlist(vals)
  mid  <-  rep(d$x, lengths(vals))
  d2   <-  data.frame(
    x    = mid - 0.4,
    xend = mid + 0.4,
    xmid = mid,
    y    = y,
    yend = y,
    timeframe = rep(tmpall$timeframe, lengths(vals)),
    rcp       = rep(tmpall$rcp, lengths(vals)),
    hcr       = rep(tmpall$hcr, lengths(vals)),
    sp        = rep(tmpall$sp, lengths(vals)),
    mode      = rep(tmpall$mode, lengths(vals)),
    type      = rep(tmpall$type, lengths(vals)),
    riskbin   = rep(tmpall$riskbin, lengths(vals)))
  alphas    <-  c(1,0)
  if(revbar) 
    alphas  <-  rev(alphas)
  d2$y2     <-  alphas[as.numeric(d2[,barsplit])]*d2$y
  d2$y1     <-  abs(alphas-1)[as.numeric(d2[,barsplit])]*d2$y
  lvl<- levels(d2[,barsplit])
  if(revbar) lvl<-rev(lvl)
  
  d3        <- d2
  d3$y<-d3$yend <- d2$y*NA
  cc  <-  which(d3$hcr[cumsum(lengths(vals))]==lvl[1])
  d3$y[cumsum(lengths(vals))][cc]<-d3$yend[cumsum(lengths(vals))][cc]<-1
  d3$y<-d3$yend<-d3$y*d2$y
  
  #d2$col2  <-  factor(d2$alpha1)
  
  p <-     ggplot(data = d2, aes(x = x, xend = xend, y = y, yend = yend, color = y))
  p <- p + facet_grid(d2[,rowvar]~d2[,colvar],scales=scalesIN)
  
  p <- p + geom_segment(data = d2, aes(x = x, xend = xend, y = y, yend = yend),color=backcol,size = 2,alpha=alphaIN)
  p <- p + geom_segment(data = d3, aes(x = x, xend = xend, y = y, yend = yend),color=backcol,size = 1,alpha=.5,linetype="solid")
  
  p <- p + geom_segment(data = d2, aes(x = x+.1, xend = xend+.1, y = y1, yend = y1,color=y1),linetype="solid",size = 1)
  # p <- p + geom_segment(data = d2, aes(x = x, xend = xend, y = y1, yend = y1, color = y1,alpha=alpha1),alpha=.05,size = 2)
  p <- p + scale_color_gradient2(low = coll[1], mid = coll[round(length(coll)/2)], high = coll[length(coll)],midpoint = max(d2$y)/2) 
  
  p <- p + geom_segment(data = d2, aes(x = .5, xend = max(d2$xend)+.5, y = 0, yend = 0),color="gray",linetype="solid",size = .5)
  
  p<- p + theme_light() +
    labs(x=NULL, y=NULL,
         title=titleIN,
         subtitle=subtitleIN,
         caption=captionIN) +
    theme(plot.subtitle=element_text(margin=margin(t = 0, r = 0, b = 20, l = 0))) +
    theme(legend.title=element_blank()) +
    theme(strip.text.y=element_blank()) +
    theme(strip.text.y = element_text(color = "white"))+
    theme(legend.position=lgnpos) +
    theme(legend.key.width = unit(.5, "cm")) +
    theme(legend.text=element_text(size=5)) +
    theme(legend.key.size=unit(.01, "cm")) +
    labs(x= xlabb,y=ylabb)+
    #labs(tag=letters(1:6)) +
    theme(plot.margin=margin(t = 10, r = 10, b = 20, l =10)) 
  
  p<- p+ theme_kir_EBM(sub_title_size=12,
                       sub_title_just="l",
                       plot_margin = margin(plot_marginIN),
                       plot_title_margin = plot_title_marginIN,
                       subtitle_margin = subtitle_marginIN,
                       caption_margin = caption_marginIN,
                       axis_title_just = "cm") +
    labs(x=xlabb, y= ylabb)+
    theme(legend.title=element_blank(),
          legend.background = element_rect(colour = NA),
          legend.key = element_rect(colour = "white", fill = NA)) 
  
  p <- p + theme(legend.position=lgnpos)
  p <- p + guides(fill = FALSE)
  p <- p + guides(color = guide_legend(order = 1,nrow = 2))
  p <- p + guides(color = guide_legend(override.aes=list(fill=NA)))
  p <- p + guides(size = "none", colour = "legend",linetype="none")
  p <- p + guides(shape = guide_legend(nrow = nrowlg[1]))
  p <- p + guides(color = guide_legend(nrow = nrowlg[1]))
  # if(!is.null(xlimmIN[1])) 
  p <- p + xlim(0.5,max(d2$xend)+.25) 
  p <- p + ylim(-10,110) 
  p <- p + theme(axis.text.x = element_text(angle=angleIN,vjust=xtick_vjust, hjust=xtick_hjust))
  
  xticks <- tapply(d2$xmid,d2$timeframe,mean)                        
  p <- p +  scale_x_discrete(xlabb, 
                             limits = xticks, 
                             labels=names(xticks) )
  p
}


GGplot_aclimCEATTLE_riskV2<-function(
    esm_namesIN=simnames,
    RISK    = list("no cap" = risk12,"2 MT cap" = risk13),
    RISKTYPES = riskTypes[c(1,3)],
    modeIN  = c("SSM","MSM"),
    rcpIN   = c("RCP 4.5"="rcp45","RCP 8.5"="rcp85"),
    deltaIN = FALSE,
    plotSet = list("RCP 4.5" = c(1,rcp45_n_sim),"RCP 8.5" = c(1,rcp85NoBio_n_sim)),
    colvar="rcp",
    rowvar="sp",
    h       = 3,
    w       = 4.75,
    spIN    = 1:3,
    plotMEAN = FALSE,
    plotpersist = TRUE,
    ylimm_up    = c(20,2,1.5)*1e6,
    ylimm_dwn   = c(0,0,0),
    xlimmIN = NULL,
    pchh    = c(2,3),
    sizeIN  = 6,
    scalesIN= "free_y",
    lgnpos  = "bottom",
    fn      = "BT",
    ltyy    = c("solid","solid"),
    lwdd    = c(.7,.4),
    coll    = c(colors()[320],col2(6)[c(2,3,4)],col3(6)[c(3,4,6)]),
    xlabb   = "Risk of decline in Catch", 
    ylabb   = "Risk of decline in Biomass", 
    nspp    = 3,
    nrowlg  = c(2,2),
    ydiv    = 1e6,
    titleIN = "",
    captionIN  = "",
    subtitleIN = "",
    projLine   = 2017,
    smooth_yr  = 20,
    add0line   = FALSE,
    alpha      = c(.4,1), 
    prob       = c(.1,.50,.9),
    plot_marginIN         = c(1, 1, 1, 1),
      plot_title_marginIN = 0,
      subtitle_marginIN   = 0,
      caption_marginIN    = 0
    ){

        tmp1<-as_tibble(RISK[[1]])%>%filter(sp%in%spIN,mode%in%modeIN, type%in%RISKTYPES,rcp%in%rcpIN)
        tmp2<-as_tibble(RISK[[2]])%>%filter(sp%in%spIN,mode%in%modeIN, type%in%RISKTYPES,rcp%in%rcpIN)
        

        hcrs<-names(RISK)
        tmp1$hcr<- hcrs[1]
        tmp2$hcr<-hcrs[2]
        tmp1$Size<-inv.logit((as.numeric(scale(1/((tmp1$riskCcv+tmp1$riskBcv))))))
        tmp2$Size<-inv.logit(as.numeric(scale(1/((tmp2$riskCcv+tmp2$riskBcv)))))
        tmp1$Size[is.na(tmp1$Size)]<-1
        tmp2$Size[is.na(tmp2$Size)]<-1
        tmp1<-data.frame(tmp1)
        tmp2<-data.frame(tmp2)
        tmpall<-rbind(tmp1,tmp2)


        dev.new(height=h,width=w)
        maxx   <-  0
        for(i in 1:length(plotSet))
          maxx <-  max(length(plotSet[[i]]),maxx)

        splevels        <- paste0(letters[1:3],") ",c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        #splevels<-c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
        correctOrder    <- c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])  # system constant in correct sort order.
        correctOrder    <- factor(paste(1:length(correctOrder),correctOrder))
        correctOrderLab <- c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])

        tmpall$sp<-splevels[tmpall$sp]
        tmp1$sp<-splevels[tmp1$sp]
        tmp2$sp<-splevels[tmp2$sp]

        
        p <-     ggplot(data=tmp1, aes(x = timeframe, y = riskC,colour=timeframe))
        p <- p + facet_grid(tmp1[,rowvar]~tmp1[,colvar],scales=scalesIN) 
        p <- p + geom_point(data=tmpall,aes(x = timeframe, y = riskC,colour=timeframe,shape=hcr),size=tmpall$Size*sizeIN,inherit.aes=TRUE,alpha = alpha[1])
        p <- p + geom_point(data=tmpall,aes(x = timeframe, y = riskC,colour=timeframe,shape=hcr),colour="white",size=.9,inherit.aes=TRUE)
      
       p
        if(plotMEAN){
           p <- p + geom_segment(data=tmp1, aes( 
            x = (tmp1$timeframe),y    = (tmp1$riskCmn),    
            xend = (tmp2$timeframe),yend = (tmp2$riskCmn), colour = timeframe,linetype=factor(type),group = type),
            size=1,alpha=alpha[2],arrow = arrow(length = unit(5, "points"),type="open", angle = 40)  )
       
         }else{
          p <- p + geom_segment( data=tmp1,aes( x = tmp1$timeframe,y = tmp1$riskC, 
           xend = tmp2$timeframe,  yend = tmp2$riskC, 
          colour = timeframe,linetype=factor(type),group = type),inherit.aes=FALSE,
          size=1,alpha=alpha[2],arrow = arrow(length = unit(5, "points"),type="open", angle = 40)  )
          p
         }
         
 
        p
        
        p <- p + scale_color_manual(values=coll)
        p <- p + scale_fill_manual(values=coll, name="fill")
        #p <- p + guides(color = guide_legend(order = 2))
      
        p <- p + scale_linetype_manual(values=ltyy)
        p <- p + scale_size_manual(values=as.numeric(lwdd))
        p <- p + scale_shape_manual(values=as.numeric(pchh))

        p<- p + theme_light() +
        labs(x=NULL, y=NULL,
           title=titleIN,
           subtitle=subtitleIN,
           caption=captionIN) +
        theme(plot.subtitle=element_text(margin=margin(b=20))) +
        theme(legend.title=element_blank()) +
        theme(legend.position=lgnpos) +
        theme(legend.key.width = unit(.5, "cm")) +
        theme(legend.text=element_text(size=5)) +
        theme(legend.key.size=unit(.01, "cm")) +
        labs(x= xlabb,y=ylabb)+
        #labs(tag=letters(1:6)) +
        theme(plot.margin=margin(t = 10, r = 10, b = 10, l =10)) 

        p<- p+ theme_kir_EBM(sub_title_size=12,
                                       sub_title_just="l",
                                       plot_margin = margin(plot_marginIN),
                                       plot_title_margin = plot_title_marginIN,
                                       subtitle_margin = subtitle_marginIN,
                                       caption_margin = caption_marginIN,
                                       axis_title_just = "cm") +
              labs(x=xlabb, y= ylabb)+
              theme(legend.title=element_blank(),
              legend.background = element_rect(colour = NA),
              legend.key = element_rect(colour = "white", fill = NA)) 

        p <- p + theme(legend.position=lgnpos)
        p <- p + guides(fill = FALSE)
        p <- p + guides(color = guide_legend(order = 1,nrow = 2))
        p <- p + guides(color = guide_legend(override.aes=list(fill=NA)))
        p <- p + guides(size = "none", colour = "legend",linetype="none")
        p <- p + guides(shape = guide_legend(nrow = nrowlg[1]))
        p <- p + guides(color = guide_legend(nrow = nrowlg[1]))
        # if(!is.null(xlimmIN[1])) 
        p <- p + xlim(-10,110) 
        p <- p + ylim(-10,110) 

        p


}


  #________________________________________________
  # Comparaitve timeseries
  #________________________________________________

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

  
  #________________________________________________
  # Threshold plots
  #________________________________________________    
PLOT_THRESHOLD<-function(
 sppIN="plk",
 sppINFOIN=sppINFO,
 multIN=10,
 collin=colorRampPalette(rev(RColorBrewer::brewer.pal(9, "YlGnBu"))),
 firstdiff=TRUE,
 ntemps=5,
 plot_marginIN= c(5,5,5,5),
 lgnpos=c(.95,1.12),
 trndfill="white",
 trndln="white",
 trndln2="white",
 tipping =Ornjazz[3],
 shp=16,
 ptsize=2,
 sizeIN=c(0.5,.75,1.2,2),
 ylimmIN=c(-2,2),
 xlimmIN=c(-2,8),
 nspp=3,
 binW = c(0.2, 0.1),
 alphacore=1,altTheme=T,
 dataIN_1=tmpall13_1,
 dataIN_2=tmpall13_2,
 dataIN_3=tmpall13_3
 ){
  
       makeNA<-function(x){
            if(length(x)==0){
              return(NA)
            }else{
              return(x)
            }
        }
      diftxt<-"s'(x)"
      if(!firstdiff)  diftxt<-"s''(x)"
      p_thresh<-rep(FALSE,nspp)
      # create plotting matrix for facet_wrap
      for(s in 1:3){
        eval(parse(text=paste0("tmpD13<-dataIN_",s)))
         
        if(!is.na(unlist(tmpD13$thrsh_max1)[1]))
          p_thresh[s]<-TRUE   
        
        if(firstdiff)  diffn<-tmpD13$fdif1
        if(!firstdiff) diffn<-tmpD13$fdif2  # use second differential
        
        if(s==1){
          plotALL         <-  data.frame(deltaC=tmpD13$deltaC,TempC=tmpD13$TempC,species=sppINFO[[s]]$plotSPP)
          hat_se          <-  (tmpD13$hat$up-tmpD13$hat$mn)
          tmpD13$hat$up   <-  tmpD13$hat$mn+multIN*(hat_se)
          tmpD13$hat$dwn  <-  tmpD13$hat$mn-multIN*(hat_se)
          hatALL          <-  data.frame(tmpD13$hat,species=sppINFO[[s]]$plotSPP)
          fdif1ALL        <- data.frame(diffn,species=sppINFO[[s]]$plotSPP)
          arrws           <- data.frame(phase=NA, thrsh=NA, ix=NA,    absval=NA,dwn=NA,
                                    TempC=NA,hatC=NA,sigdf1=NA,sigdf2=NA,
                                    species=sppINFO[[s]]$plotSPP)  # tresholds
          if(p_thresh[s])
            arrws       <- data.frame(
                                    phase=NA, 
                                    thrsh=tmpD13$hat$tmp[tmpD13$thrsh_max1], 
                                    ix=tmpD13$thrsh_max1,
                                    absval=NA,
                                    dwn=tmpD13$hat$up[tmpD13$thrsh_max1],
                                    TempC=tmpD13$hat$tmp[tmpD13$thrsh_max1],
                                    hatC=tmpD13$hat$mn[tmpD13$thrsh_max1],
                                    sigdf1 =tmpD13$signif1[tmpD13$thrsh_max1],
                                    sigdf2 =tmpD13$signif2[tmpD13$thrsh_max1],
                                    species=sppINFO[[s]]$plotSPP)  # tresholds
          
          tmpth                   <-  tmpD13$hat$mn*NA
          tmpth[tmpD13$signif1]   <-  as.numeric(tmpD13$hat$mn[tmpD13$signif1])
          tmpth2                  <-  tmpD13$hat$mn*NA
          tmpth2[tmpD13$signif2]  <-  as.numeric(tmpD13$hat$mn[tmpD13$signif2])
          threshALL   <- data.frame(
            TempC=tmpD13$hat$tmp,
            sigdf1=tmpth,
            sigdf2=tmpth2,
            thrsh=makeNA(tmpD13$thrsh_max1),
            species=sppINFO[[s]]$plotSPP)
      }else{
          plotALL          <-  rbind(plotALL,data.frame(deltaC=tmpD13$deltaC,TempC=tmpD13$TempC,species=sppINFO[[s]]$plotSPP))
          hat_se           <-  (tmpD13$hat$up-tmpD13$hat$mn)
          tmpD13$hat$up    <-  tmpD13$hat$mn+multIN*(hat_se)
          tmpD13$hat$dwn   <-  tmpD13$hat$mn-multIN*(hat_se)
          hatALL           <-  rbind(hatALL,data.frame(tmpD13$hat,species=sppINFO[[s]]$plotSPP))
          fdif1ALL         <- rbind(fdif1ALL,data.frame(diffn,species=sppINFO[[s]]$plotSPP))
          tmparrws         <- data.frame(phase=NA, thrsh=NA, ix=NA,    absval=NA,dwn=NA,
                                    TempC=NA, hatC=NA,sigdf1 =NA,sigdf2 =NA,
                                    species=sppINFO[[s]]$plotSPP)  # tresholds
          if(p_thresh[s]) 
            tmparrws       <- data.frame(
                                    phase=NA, 
                                    thrsh=tmpD13$hat$tmp[tmpD13$thrsh_max1], 
                                    ix=tmpD13$thrsh_max1,
                                    absval=NA,
                                    dwn=tmpD13$hat$up[tmpD13$thrsh_max1],
                                    TempC=tmpD13$hat$tmp[tmpD13$thrsh_max1],
                                    hatC=tmpD13$hat$mn[tmpD13$thrsh_max1],
                                    sigdf1 =tmpD13$signif1[tmpD13$thrsh_max1],
                                    sigdf2 =tmpD13$signif2[tmpD13$thrsh_max1],
                                    species=sppINFO[[s]]$plotSPP)
          arrws                  <-  rbind(arrws,tmparrws)
          tmpth                  <-  tmpD13$hat$mn*NA
          tmpth[tmpD13$signif1]  <-  as.numeric(tmpD13$hat$mn[tmpD13$signif1])
          tmpth2                 <-  tmpD13$hat$mn*NA
          tmpth2[tmpD13$signif2] <-  as.numeric(tmpD13$hat$mn[tmpD13$signif2])
          for(ii in 1:length(makeNA(tmpD13$thrsh_max1)))
            threshALL   <- rbind(threshALL,data.frame(
              TempC=tmpD13$hat$tmp,
              sigdf1=tmpth,
              sigdf2=tmpth2,
              thrsh=makeNA(tmpD13$thrsh_max1)[ii],
              species=sppINFO[[s]]$plotSPP))
        }
      }

      arrws$TempC        <- tmpD13$hat$tmp[arrws$ix]
      colnames(fdif1ALL)[1:4] <- colnames(hatALL)[1:4]<-c("TempC","up","deltaC","dwn")

      hatALL$type        <- as.factor("s(x)")
      fdif1ALL$type      <- as.factor(diftxt)
      hatALL$species     <- as.factor(hatALL$species);fdif1ALL$species<-as.factor(fdif1ALL$species)
      fdif1ALL$group     <- (paste0(fdif1ALL$type,"_",fdif1ALL$species))
      hatALL$group       <- (paste0(hatALL$type,"_",hatALL$species))
      ALLDAT             <- rbind(hatALL,fdif1ALL)
      nc                 <- length(plotALL$TempC)
      alpha1             <- .2
      nc                 <- length(plotALL$TempC)
      alpha1             <- .4

      require(RColorBrewer)
      colorscale = scale_fill_gradientn(
        colors = collin(9),
        values = c(0, exp(seq(-5, 0, length.out = 100))))
      # 
      plotALL$type       <- "s(x)"
      plotALL2           <- plotALL
      plotALL2$type      <- diftxt
      plotALL2$deltaC    <- plotALL2$deltaC*0*NA
      plotALL2$TempC     <- plotALL2$TempC*0*NA
      plotALL            <- rbind(plotALL,plotALL2)
      plotALL$deltaCAKA  <- plotALL$deltaC
      plotALL$deltaC[plotALL$deltaC>ylimmIN[2]+.5]<-NA
      plotALL$deltaC[plotALL$deltaC<ylimmIN[1]-.5]<-NA

      threshALL$type     <- "s(x)"
      threshALL2         <- threshALL
      threshALL2$type    <- diftxt
      threshALL2$sigdf1  <- threshALL2$sigdf1*0*NA
      #threshALL2$sigdf2  <- threshALL2$sigdf1*0*NA
      threshALL2$TempC   <- threshALL2$TempC*0*NA
      threshALL          <- rbind(threshALL,threshALL2)
      threshALL$type     <- factor(threshALL$type, levels=c("s(x)",diftxt))

      arrws$type         <- "s(x)"
      arrws$end          <- ylimmIN[1]-.5
      arrws2             <- arrws
      arrws2$absval      <- arrws2$absval*0*NA
      arrws2$TempC       <- arrws2$TempC*0*NA
      arrws2$dwn         <- arrws2$dwn*0*NA
      arrws2$end         <- arrws2$end*0+-.01
      arrws2$type        <- diftxt
      
      arrws              <- rbind(arrws,arrws2)
      arrws$type         <- factor(arrws$type, levels=c("s(x)",diftxt))

      ulist                 <- data.frame(group=unique(ALLDAT$group),species=NA,type=NA)
      for (uu in 1:length(ulist$group)){
          ulist$species[uu]   <- as.character(ALLDAT$species[ALLDAT$group==ulist$group[uu]][1])
          ulist$type[uu]      <- as.character(ALLDAT$type[ALLDAT$group==ulist$group[uu]][1])
      }
      blank_data            <- data.frame(species=rep(ulist[,2],2),
                             type=rep(ulist[,3],2),x=0,
                             y = c(rep(ylimmIN[1],3),rep(-.5,3),rep(ylimmIN[2],3),rep(.5,3)))

      species_labeller<- function(variable,value){
        species_names        <- as.list(as.character(ulist$species))
        names(species_names) <- as.character(ulist$group)
        return(species_names[value])
      }

      
      i            <- 1
      plotarrows   <- data.frame(
                           phase   =  arrws$phase[i],
                           thrsh   =  arrws$thrsh[i],
                           ix      =  arrws$ix[i],
                           absval  =  arrws$absval[i],
                           y       =  as.numeric(c(arrws$dwn[i],arrws$end[i])),
                           TempC   =  arrws$TempC[i],
                           species =  arrws$species[i],
                           type    =  arrws$type[i],
                           plotset =  i)

      for(i in 2:dim(arrws)[1]){
           plotarrows   <- rbind(plotarrows,data.frame(
                phase   =  arrws$phase[i],
                thrsh   =  arrws$thrsh[i],
                ix      =  arrws$ix[i],
                absval  =  arrws$absval[i],
                y       =  as.numeric(c(arrws$dwn[i],arrws$end[i])),
                TempC   =  arrws$TempC[i],
                species =  arrws$species[i],
                type    =  arrws$type[i],
                plotset =  i))
      }


      maxnarrw<-max(tapply(plotarrows$species,plotarrows$species,length)/4)
      ncol<-dim(plotarrows)[2]
      plotarrows[plotarrows$type=="s(x)",]$plotset
      for(s in 1:nspp){
        sub                  <-  plotarrows[which(plotarrows$species==sppINFO[[s]]$plotSPP),]
        tmpm                 <-  data.frame(matrix(NA,maxnarrw*4,ncol))
        colnames(tmpm)       <-  colnames(plotarrows)

        tmpm[1:dim(sub)[1],] <-  sub
        tmpm$type            <-  as.factor(plotarrows$type[which(plotarrows$species==sppINFO[[s]]$plotSPP)])
        tmpm$species         <-  as.factor((plotarrows$species[which(plotarrows$species==sppINFO[[s]]$plotSPP)]))

        tmpm$plotset2        <-  0
        nsets                <-  dim(sub)[1]/4
        ncol2                <-  dim(tmpm)[2]
        for(i in 1:maxnarrw){
          srt  <- (i*2)-1
          nd   <- i*2
          tmpm$plotset2[c((srt:nd),(srt:nd)+maxnarrw*2)]<-i
          if(i>nsets){
            tmpm2<-sub[c((1:2)+nsets*2,(1:2)+nsets*2),]
            tmpm2$type<-factor(c("s(x)","s(x)",diftxt,diftxt),levels=c("s(x)",diftxt))
            tmpm[c((srt:nd),(srt:nd)+maxnarrw*2),-ncol2]<-tmpm2
            tmpm[(dim(sub)[1]+1):dim(tmpm)[1],-ncol2]<-tmpm2
            tmpm$type[(maxnarrw*2+1):(maxnarrw*4)]<-factor(diftxt,levels=c("s(x)",diftxt))
            
            i<-maxnarrw
          }
        }
        if(s==1) plotarrows2<-tmpm
        if(s>1) plotarrows2<-rbind(plotarrows2,tmpm)

      }
      plotALL$type<-factor(plotALL$type,levels=c("s(x)",diftxt))

      topdat    <-ALLDAT[ALLDAT$type=="s(x)",]
      bottomdat <-ALLDAT[ALLDAT$type==diftxt,]

      p <-     ggplot(data=plotALL, aes(x = TempC, y = deltaC),colour=TempC)
      p <- p + geom_hline(yintercept=0,colour="gray",size=1)                 
      #geom_point(data=plotALL,aes(x = TempC, y = deltaC,color = TempC, fill = TempC,size= 1), alpha = alphacore) +
      p <- p + geom_hex(binwidth = binW) + colorscale
      #p <- p + scale_alpha_manual(c(0.8, 0.8, 0.8,0, 0, 0)) sizeIN<-c(0.5,.75,1.2,2)
      p <- p + geom_ribbon(data=topdat,aes(x = TempC, ymin=dwn, ymax=up,group=species),fill=trndfill,col=trndln, linetype=1,size=sizeIN[1], alpha=alpha1)  
      p <- p + geom_line(data=topdat,aes(x = TempC, y = deltaC,group=species),alpha=1,colour = trndln,inherit.aes=FALSE,size=sizeIN[2]) 
      p <- p + geom_line(data=threshALL[ALLDAT$type=="s(x)",],aes(x = TempC, y = sigdf1,group=species),alpha=1,colour = trndln,inherit.aes=FALSE,size=sizeIN[3]) 
      p <- p + geom_line(data=threshALL[ALLDAT$type=="s(x)",],aes(x = TempC, y = sigdf2,group=species),alpha=1,colour = trndln2,inherit.aes=FALSE,size=sizeIN[4]) # Ornjazz[3]
      if(any(p_thresh)) p <- p + geom_point(data=arrws[arrws$type=="s(x)",],aes(x = thrsh, y = hatC,group=species),alpha=1,shape=shp,colour = tipping,inherit.aes=FALSE,size=ptsize) # Ornjazz[3]
      #for(i in 1:maxnarrw) p <- p + geom_path(data=plotarrows2[plotarrows2$plotset2==i,],colour = Ornjazz[3],aes(x=TempC,y=y),arrow = arrow(length=unit(0.30,"cm"),ends="first", type = "closed"),size=1)
      p <- p + facet_grid(type ~ species,scales = "free_y") + geom_blank(data = blank_data, aes(x = x, y = y))
      p <- p + expand_limits(y = 0) + scale_y_continuous(expand = c(0, 0))   
      p <- p + geom_ribbon(data=bottomdat,aes(x = TempC, ymin=dwn, ymax=up,group=species),fill=night(10)[6],col=NA, linetype=1,size=.5, alpha=alpha1,inherit.aes=FALSE)  
      p <- p + geom_line(data=bottomdat,aes(x = TempC, y = deltaC,group=species),alpha=1,colour = night(10)[6],inherit.aes=FALSE,size=.75) 

      # set the custom color scale
      p <- p + scale_colour_gradientn(name = "TempC",colours = collin(ntemps)) 
      #p <- p + coord_cartesian(ylim=c(1,1))
      #geom_smooth(alpha=alpha2,se=T, method = "loess",fill=trndfill,color=trndln )  +
      if(ylimmIN[1]!=FALSE) p<- p + ylim(ylimmIN[1],ylimmIN[2]) 
      if(xlimmIN[1]!=FALSE) p<- p + xlim(xlimmIN[1],xlimmIN[2]) 
      p<- p + theme_light() +
      labs(x=NULL, y=NULL,
       title="",
       subtitle="",
       caption="") +
      theme(plot.subtitle=element_text(margin=margin(b=20))) +
      theme(legend.title=element_blank()) +
      theme(legend.position=lgnpos) +
      theme(legend.key.width = unit(.5, "cm")) +
      theme(legend.text=element_text(size=5)) +
      theme(legend.key.size=unit(.01, "cm")) +
      labs(x= expression(paste("Temperature ",'( '^{o},"C)")),y=expression(paste("", Delta,"Catch")) )+
      #labs(tag=letters(1:6)) +
      theme(plot.margin=margin(t = 3, r = 3, b = 3, l =3)) 




      if(altTheme) p<- p+ theme_kir_EBM(sub_title_size=12,
                                   sub_title_just="l",
                                   plot_margin = margin(plot_marginIN),
                                   plot_title_margin = 1,
                                   subtitle_margin = 0,
                                   caption_margin = 0,
                                   axis_title_just = "cm") 
      

      p

     
} 



  #________________________________________________
  # Threshold plots
  #________________________________________________    
PLOT_THRESHOLD2<-function( 
  ntemps=3,
  byrow= "species",
  bycol = "cap",
  ylabbIN =expression(paste("", Delta,"Catch")) ,
  sppINFOIN=sppINFO,
  multIN=5,
  collin=colorRampPalette(rev(RColorBrewer::brewer.pal(9, "YlGnBu"))),
  firstdiff=TRUE,
  plot_marginIN= c(5,5,5,5),
  lgnpos=c(.95,1.12),
  trndfill="white",
  shp=16,
  ptsize=2,
  binW=c(0.2,0.1),
  ylimmIN =c(-1,1.5),
  xlimmIN =c(1,7),
  trndln  = "white",
  trndln2 = Ornjazz[3],
  tipping = Ornjazz[5],
  sizeIN=c(0.1,.3,.75,2),
  nspp=3,
  alphacore=1,
  altTheme=T,
  sublab=TRUE,
  sublab_adj=.95,
  dataIN_1  = tmpall12_1,
  dataIN_2  = tmpall12_2,
  dataIN_3  = tmpall12_3,
  dataIN2_1 = tmpall13_1,
  dataIN2_2 = tmpall13_2,
  dataIN2_3 = tmpall13_3){
  
      makeNA<-function(x){ if(length(x)==0){ return(NA)}else{return(x)} }
      diftxt<-"s'(x)"
      if(!firstdiff)  diftxt<-"s''(x)"
     
      # create plotting matrix for facet_wrap
      p_thresh<-matrix(FALSE,2,nspp)
      for(d in 1:2){
       
        for(s in 1:3){
          if(d==1) eval(parse(text=paste0("tmpD13<-dataIN_",s)))
          if(d==2) eval(parse(text=paste0("tmpD13<-dataIN2_",s)))
           
          if(!is.na(unlist(tmpD13$thrsh_max1)[1]))
            p_thresh[d,s]<-TRUE   
          if(firstdiff)  diffn<-tmpD13$fdif1
          if(!firstdiff) diffn<-tmpD13$fdif2  # use second differential
          
          if(s==1&d==1){
            plotALL     <- data.frame(deltaC=tmpD13$deltaC,TempC=tmpD13$TempC,species=sppINFO[[s]]$plotSPP,cap=c("no cap","2 MT cap")[d])
            hat_se<-(tmpD13$hat$up-tmpD13$hat$mn)
            tmpD13$hat$up<-tmpD13$hat$mn+multIN*(hat_se)
            tmpD13$hat$dwn<-tmpD13$hat$mn-multIN*(hat_se)


            hatALL      <- data.frame(tmpD13$hat,species=sppINFO[[s]]$plotSPP,cap=c("no cap","2 MT cap")[d])
            fdif1ALL    <- data.frame(diffn,species=sppINFO[[s]]$plotSPP)
            arrws       <- data.frame(cap=c("no cap","2 MT cap")[d],phase=NA, thrsh=NA, ix=NA,    absval=NA,dwn=NA,
                                      TempC=NA,hatC=NA,sigdf1=NA,sigdf2=NA,
                                      species=sppINFO[[s]]$plotSPP)  # tresholds
            
            if(p_thresh[d,s]) arrws       <- data.frame(
                                      cap=c("no cap","2 MT cap")[d],
                                      phase=NA, 
                                      thrsh=tmpD13$hat$tmp[tmpD13$thrsh_max1], 
                                      ix=tmpD13$thrsh_max1,
                                      absval=NA,
                                      dwn=tmpD13$hat$up[tmpD13$thrsh_max1],
                                      TempC=tmpD13$hat$tmp[tmpD13$thrsh_max1],
                                      hatC=tmpD13$hat$mn[tmpD13$thrsh_max1],
                                      sigdf1 =tmpD13$signif1[tmpD13$thrsh_max1],
                                      sigdf2 =tmpD13$signif2[tmpD13$thrsh_max1],
                                      species=sppINFO[[s]]$plotSPP)  # tresholds
            
            tmpth<-tmpD13$hat$mn*NA
            tmpth[tmpD13$signif1]<-as.numeric(tmpD13$hat$mn[tmpD13$signif1])

            tmpth2<-tmpD13$hat$mn*NA
            tmpth2[tmpD13$signif2]<-as.numeric(tmpD13$hat$mn[tmpD13$signif2])

            threshALL   <- data.frame(
              TempC=tmpD13$hat$tmp,
              cap=c("no cap","2 MT cap")[d],
              sigdf1=tmpth,
              sigdf2=tmpth2,
              thrsh=makeNA(tmpD13$thrsh_max1),
              species=sppINFO[[s]]$plotSPP)

          }else{
            plotALL    <- rbind(plotALL,data.frame(deltaC=tmpD13$deltaC,TempC=tmpD13$TempC,species=sppINFO[[s]]$plotSPP,cap=c("no cap","2 MT cap")[d]))
            hat_se<-(tmpD13$hat$up-tmpD13$hat$mn)
            tmpD13$hat$up<-tmpD13$hat$mn+multIN*(hat_se)
            tmpD13$hat$dwn<-tmpD13$hat$mn-multIN*(hat_se)

            hatALL     <- rbind(hatALL,data.frame(tmpD13$hat,species=sppINFO[[s]]$plotSPP,cap=c("no cap","2 MT cap")[d]))
            fdif1ALL   <- rbind(fdif1ALL,data.frame(diffn,species=sppINFO[[s]]$plotSPP))
            tmparrws   <- data.frame(cap=c("no cap","2 MT cap")[d],phase=NA, thrsh=NA, ix=NA,    absval=NA,dwn=NA,
                                      TempC=NA, hatC=NA,sigdf1 =NA,sigdf2 =NA,
                                      species=sppINFO[[s]]$plotSPP)  # tresholds
            if(p_thresh[d,s]) tmparrws <- data.frame(
                                      cap=c("no cap","2 MT cap")[d],
                                      phase=NA,
                                      thrsh=tmpD13$hat$tmp[tmpD13$thrsh_max1], 
                                      ix=tmpD13$thrsh_max1,
                                      absval=NA,
                                      dwn=tmpD13$hat$up[tmpD13$thrsh_max1],
                                      TempC=tmpD13$hat$tmp[tmpD13$thrsh_max1],
                                      hatC=tmpD13$hat$mn[tmpD13$thrsh_max1],
                                      sigdf1 =tmpD13$signif1[tmpD13$thrsh_max1],
                                      sigdf2 =tmpD13$signif2[tmpD13$thrsh_max1],
                                      species=sppINFO[[s]]$plotSPP)
            arrws      <- rbind(arrws,tmparrws)
            tmpth<-tmpD13$hat$mn*NA
            tmpth[tmpD13$signif1]<-as.numeric(tmpD13$hat$mn[tmpD13$signif1])

            tmpth2<-tmpD13$hat$mn*NA
            tmpth2[tmpD13$signif2]<-as.numeric(tmpD13$hat$mn[tmpD13$signif2])
          for(ii in 1:length(makeNA(tmpD13$thrsh_max1)))
            threshALL   <- rbind(threshALL,data.frame(
              TempC=tmpD13$hat$tmp,
              cap=c("no cap","2 MT cap")[d],
              sigdf1=tmpth,
              sigdf2=tmpth2,
              thrsh=makeNA(tmpD13$thrsh_max1)[ii],
              species=sppINFO[[s]]$plotSPP))
          }
        }
      }

      arrws$TempC        <- tmpD13$hat$tmp[arrws$ix]
      colnames(fdif1ALL)[1:4] <- colnames(hatALL)[1:4]<-c("TempC","up","deltaC","dwn")
      hatALL$type        <- as.factor("s(x)")
      fdif1ALL$type      <- as.factor(diftxt)
      hatALL$species     <- as.factor(hatALL$species);fdif1ALL$species<-as.factor(fdif1ALL$species)
      fdif1ALL$group     <- (paste0(fdif1ALL$type,"_",fdif1ALL$species))
      hatALL$group       <- (paste0(hatALL$type,"_",hatALL$species))
      #ALLDAT             <- rbind(hatALL,fdif1ALL)
      ALLDAT<- hatALL
      nc                 <- length(plotALL$TempC)
      alpha1             <- .2
      nc                 <- length(plotALL$TempC)
      alpha1             <- .4

      require(RColorBrewer)
      colorscale = scale_fill_gradientn(
          colors = collin(9),
          values = c(0, exp(seq(-5, 0, length.out = 100))))
      # 
      plotALL$type       <- "s(x)"
      plotALL$deltaCAKA  <- plotALL$deltaC
      plotALL$deltaC[plotALL$deltaC>ylimmIN[2]+.5]<-NA
      plotALL$deltaC[plotALL$deltaC<ylimmIN[1]-.5]<-NA

      threshALL$type     <- "s(x)"
      arrws$type         <- "s(x)"
      arrws$end          <- ylimmIN[1]-.5
      ulist                 <- data.frame(group=unique(ALLDAT$group),species=NA,type=NA)
      for (uu in 1:length(ulist$group)){
      ulist$species[uu]   <- as.character(ALLDAT$species[ALLDAT$group==ulist$group[uu]][1])
      ulist$type[uu]      <- as.character(ALLDAT$type[ALLDAT$group==ulist$group[uu]][1])
      }
      blank_data            <- data.frame(species=rep(ulist[,2],2),
                             type=rep(ulist[,3],2),x=0,
                             y = c(rep(ylimmIN[1],3),rep(-.5,3),rep(ylimmIN[2],3),rep(.5,3)))

      species_labeller<- function(variable,value){
        species_names        <- as.list(as.character(ulist$species))
        names(species_names) <- as.character(ulist$group)
        return(species_names[value])
      }

      
      i            <- 1
      plotarrows   <- data.frame(phase   =  arrws$phase[i],
                           thrsh   =  arrws$thrsh[i],
                           ix      =  arrws$ix[i],
                           absval  =  arrws$absval[i],
                           y       =  as.numeric(c(arrws$dwn[i],arrws$end[i])),
                           TempC   =  arrws$TempC[i],
                           species =  arrws$species[i],
                           type    =  arrws$type[i],
                           plotset =  i)

      for(i in 2:dim(arrws)[1]){
           plotarrows   <- rbind(plotarrows, 
      data.frame(phase   =  arrws$phase[i],
                thrsh   =  arrws$thrsh[i],
                ix      =  arrws$ix[i],
                absval  =  arrws$absval[i],
                y       =  as.numeric(c(arrws$dwn[i],arrws$end[i])),
                TempC   =  arrws$TempC[i],
                species =  arrws$species[i],
                type    =  arrws$type[i],
                plotset =  i))
      }

      maxnarrw<-max(tapply(plotarrows$species,plotarrows$species,length)/4)
      ncol<-dim(plotarrows)[2]
      plotarrows[plotarrows$type=="s(x)",]$plotset
      for(s in 1:nspp){
        sub                  <-  plotarrows[which(plotarrows$species==sppINFO[[s]]$plotSPP),]
        tmpm                 <-  data.frame(matrix(NA,maxnarrw*4,ncol))
        colnames(tmpm)       <-  colnames(plotarrows)

        tmpm[1:dim(sub)[1],] <-  sub
        tmpm$type            <-  as.factor(plotarrows$type[which(plotarrows$species==sppINFO[[s]]$plotSPP)])
        tmpm$species         <-  as.factor((plotarrows$species[which(plotarrows$species==sppINFO[[s]]$plotSPP)]))

        tmpm$plotset2        <-  0
        nsets                <-  dim(sub)[1]/4
        ncol2                <-  dim(tmpm)[2]
        for(i in 1:maxnarrw){
          srt  <- (i*2)-1
          nd   <- i*2
          tmpm$plotset2[c((srt:nd),(srt:nd)+maxnarrw*2)]<-i
          if(i>nsets){
            tmpm2<-sub[c((1:2)+nsets*2,(1:2)+nsets*2),]
            tmpm2$type<-factor(c("s(x)","s(x)",diftxt,diftxt),levels=c("s(x)",diftxt))
            tmpm[c((srt:nd),(srt:nd)+maxnarrw*2),-ncol2]<-tmpm2
            tmpm[(dim(sub)[1]+1):dim(tmpm)[1],-ncol2]<-tmpm2
            tmpm$type[(maxnarrw*2+1):(maxnarrw*4)]<-factor(diftxt,levels=c("s(x)",diftxt))
            
            i<-maxnarrw
          }
        }
        if(s==1) plotarrows2<-tmpm
        if(s>1) plotarrows2<-rbind(plotarrows2,tmpm)

      }
      plotALL$type<-factor(plotALL$type,levels=c("s(x)",diftxt))

      p <-     ggplot(data=plotALL, aes(x = TempC, y = deltaC),colour=TempC)
      p <- p + geom_hline(yintercept=0,colour="gray",size=1)                 
      p <- p + geom_hex(binwidth = c(0.2, 0.1)) + colorscale
      p <- p + geom_ribbon(data=ALLDAT[ALLDAT$type=="s(x)",],aes(ymin=dwn, ymax=up,group=species),fill=trndfill,col=trndln, linetype=1,size=sizeIN[1], alpha=alpha1)  
      p <- p + geom_line(data=ALLDAT[ALLDAT$type=="s(x)",],aes(x = TempC, y = deltaC,group=species),alpha=1,colour = trndln,inherit.aes=FALSE,size=sizeIN[2]) 
     
      p <- p + geom_line(data=threshALL,aes(x = TempC, y = sigdf1,group=species),alpha=1,colour = trndln,inherit.aes=FALSE,size=sizeIN[3]) 
      p <- p + geom_line(data=threshALL,aes(x = TempC, y = sigdf2,group=species),alpha=1,colour = trndln2,inherit.aes=FALSE,size=sizeIN[4]) # Ornjazz[3]
      if(any(p_thresh)) 
      p <- p + geom_point(data=arrws[arrws$type=="s(x)",],aes(x = thrsh, y = hatC,group=species),alpha=1,shape=shp,colour = tipping,inherit.aes=FALSE,size=ptsize) # Ornjazz[3]
      eval(parse(text=paste0(
        "p <- p + facet_grid(",byrow," ~ ",bycol,",scales = 'free_y') + geom_blank(data = blank_data, aes(x = x, y = y))")))
      
      p <- p + expand_limits(y = 0) + scale_y_continuous(expand = c(0, 0))   
      p <- p + geom_ribbon(data=ALLDAT[ALLDAT$type==diftxt,],aes(ymin=dwn, ymax=up,group=species),fill=night(10)[6],col=NA, linetype=1,size=.5, alpha=alpha1)  
      p <- p + geom_line(data=ALLDAT[ALLDAT$type==diftxt,],aes(x = TempC, y = deltaC,group=species),alpha=1,colour = night(10)[6],inherit.aes=FALSE,size=.75) 

      # set the custom color scale
      p <- p + scale_colour_gradientn(name = "TempC",colours = collin(ntemps)) 
      if(ylimmIN[1]!=FALSE) 
      p <- p + ylim(ylimmIN[1],ylimmIN[2]) 
      if(xlimmIN[1]!=FALSE) 
      p <- p + xlim(xlimmIN[1],xlimmIN[2]) 
      p <- p + theme_light() +
          labs(x=NULL, y=NULL,
           title="",
           subtitle="",
           caption="") +
          theme(plot.subtitle=element_text(margin=margin(b=20))) +
          theme(legend.title=element_blank()) +
          theme(legend.position=lgnpos) +
          theme(legend.key.width = unit(.5, "cm")) +
          theme(legend.text=element_text(size=5)) +
          theme(legend.key.size=unit(.01, "cm")) +
          labs(x= expression(paste("Temperature ",'( '^{o},"C)")),y=ylabbIN)+
          #labs(tag=letters(1:6)) +
          theme(plot.margin=margin(t = 3, r = 3, b = 3, l =3)) 




      if(altTheme) p<- p+ theme_kir_EBM(sub_title_size=12,
                                   sub_title_just="l",
                                   plot_margin = margin(plot_marginIN),
                                   plot_title_margin = 1,
                                   subtitle_margin = 0,
                                   caption_margin = 0,
                                   axis_title_just = "cm") 
      
      if(!is.null(sublab)){
        ann_text <-as_tibble(data.frame(x=xlimmIN[1],y=ylimmIN[2],
                                        species=rep(levels(plotALL$species),length(levels(plotALL$cap))),
                                        cap=rep(levels(plotALL$cap),each=length(levels(plotALL$sp)))))
        ann_text$species<-factor(ann_text$species,levels=levels(plotALL$species))
        ann_text$cap<-factor(ann_text$cap,levels=levels(plotALL$cap))
        ann_text<-ann_text%>%group_by(species,cap,x,y)%>%summarize(mnx=mean(x))
        ann_text$lab2<-letters[1:dim(ann_text)[1]]
        p <-  p + geom_text(data = ann_text,aes(x = x, y = y*sublab_adj,label = lab2,fontface=2),inherit.aes = FALSE)
      }
      p
      # expression (paste("Figure 3: Difference in catch (",Delta,"Catch) relative to the stable-climate persistence scenario as a function of
      #                             \n bottom temperature ",'( '^{o},"C,). Mean smoothed function and 95% CI for walleye pollock, Pacific cod, arrowtooth
      #                             \n flounder (a-c, top row). Thick white lines represent areas where the slope is significantly greater or less than 0,
      #                             \n based on the first derivative and 95% CI of the smoothing function and (bottom row, d-f) thick orange areas 
      #                             \n indicate significant threshold changes in the slope. ")),

      #expression(paste("", Delta,"Catch ~ f(Temperature) "))
      # p<- p + theme_light() +
      #   labs(x=NULL, y=NULL,
      #        title=  expression(paste("", Delta,",", R^{2},'=0.6')),
      #        subtitle="Timeseries of diet-based bioenergetics indices for juvenile and adult fish based on stomach \nsamples collected during the annual AFSC bottom trawl survey.",
      #        caption=paste0("Credit: Kirstin Holsman ",currentYR,"\nData source: www.afsc.noaa.gov/REFM/REEM/data")) +
      #   
     
} 


  #________________________________________________
  # TPlot F and effective F
  #________________________________________________   
 # print(paste0("plotting (3)",spnm))
 #      ggsave(filename=fnm,device="png",dpi=500,width=W,height=H)

# library(ggpubr)
# # Grouped Scatter plot with marginal density plots
# ggscatterhist(
#   iris, x = "Sepal.Length", y = "Sepal.Width",
#   color = "Species", size = 3, alpha = 0.6,
#   palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#   margin.params = list(fill = "Species", color = "black", size = 0.2)
#   )

plot_Feffective<-function(
  sppINFOIN=sppINFO,
  FDATA= list(
    "no cap"= F_219_CENaivecf_2_5_12_mc,
    "2 MT cap"=F_2MT_219_CENaivecf1_2_5_13_mc),
  F40DATA= list(
    "no cap"= F40_219_CENaivecf_2_5_12_mc,
    "2 MT cap"=F40_2MT_219_CENaivecf1_2_5_13_mc),
  BDATA= list(
    "no cap"= B_219_CENaivecf_2_5_12_mc,
    "2 MT cap"=B_2MT_219_CENaivecf1_2_5_13_mc),
  Btarget= list(
    "no cap"= target_B_2["no cap",],
    "2 MT cap"=target_B_2["2MT cap",]),
  plotSet = list("persistence" = 1,"RCP 4.5" = (rcp45_n_sim),"RCP 8.5" = (rcp85NoBio_n_sim)),
  collset=c("no cap"= "YlOrRd" ,"2 MT cap"="YlGnBu"),
  ntemps=5,
  plot_marginIN= c(5,5,5,5),
  lgnpos="none",
  subyr=fut_yrs,
  trndfill="white",
  trndln="white",
  trndln2="white",
  tipping =Ornjazz[3],
  shp=16,
  ptsize=2,
  sizeIN=c(0.5,.75,1.2,2),
  ylimmIN=c(-2,2),
  xlimmIN=c(-2,8),
  nspp=3,
  alphacore=1,
  altTheme=T){
   
   colorscale<-scale_fill_gradientn(
    colors = rev(RColorBrewer::brewer.pal(9, collset[2])),
    values = c(0, exp(seq(-5, 0, length.out = 100))))
    
    ndat<-length(F40DATA)
    coll<-rep(0,ndat)
    for(d in 1:ndat)
      coll[d]<-rev(RColorBrewer::brewer.pal(9, collset[d]))[3]

            
    colorscale2<-scale_colour_manual(
       values = coll,
       labels = (names(F40DATA))
    )

  
  nyr<-length(subyr)
  nset<-length(plotSet)
  for(d in 1:ndat)
    for (s in 1:nspp){
      for(p in 1:nset){
        
        yr<-as.numeric(colnames(FDATA[[d]][s,,subyr, plotSet[[p]]]))
        yr<-matrix(yr,dim(FDATA[[d]][s,,subyr, plotSet[[p]]])[1],dim(FDATA[[d]][s,,subyr, plotSet[[p]]])[2],byrow=T)
        tmpD<-data.frame(
          type =names(F40DATA)[d],
          rcp = names(plotSet)[p],
          col= collset[[d]],
          year = as.vector(yr),
          F= as.vector(F40DATA[[d]][s,,subyr, plotSet[[p]]]),
          B= as.vector(BDATA[[d]][s,,subyr, plotSet[[p]]]),
          F2Ftarget= as.vector(FDATA[[d]][s,,subyr, plotSet[[p]]]/F40DATA[[d]][s,,subyr, plotSet[[p]]]),
          B2Btarget= as.vector(BDATA[[d]][s,,subyr, plotSet[[p]]]/Btarget[[d]][s]),
          Species = sppINFO[[s]]$plotSPP)

          GAM<-gam(log(F2Ftarget+.001)~s(log(B2Btarget+.001),k=8,bs="tp"),data=tmpD[tmpD$B2Btarget>0.4,])
          # LM<-lm(log(F2Ftarget+.001)~log(B2Btarget+.001),data=tmpD[tmpD$B2Btarget>0.4,])
          hat<-predict.gam(GAM,se.fit=T)
          # hat.lm<-predict(LM,se.fit=T)
          
          # plot((F2Ftarget)~(B2Btarget),data=tmpD,ylim=c(0,1.1))
         # points(tmpD[tmpD$B2Btarget>0.4,]$B2Btarget,exp(hat.lm$fit-.001),col="red",pch=".")
          tmpD$F2Ftarget_hat<-tmpD$F2Ftarget_hatUP<-tmpD$F2Ftarget_hatDWN<-tmpD$F2Ftarget*NA
          tmpD[tmpD$B2Btarget>0.4,]$F2Ftarget_hat<-exp(hat$fit-.001)
          tmpD[tmpD$B2Btarget>0.4,]$F2Ftarget_hatUP<-exp(hat$fit+10*1.95*hat$se-.001)
          tmpD[tmpD$B2Btarget>0.4,]$F2Ftarget_hatDWN<-exp(hat$fit-10*1.95*hat$se-.001)
          # plot(tmpD$B2Btarget,tmpD$F2Ftarget)
          #  points(tmpD$B2Btarget,tmpD$F2Ftarget_hat,col="red")
          #  points(tmpD$B2Btarget,tmpD$F2Ftarget_hatUP,col="red")
          #  points(tmpD$B2Btarget,tmpD$F2Ftarget_hatDWN,col="red")
          
         if(s==1&d==1&p==1){
            plotALL<-tmpD
         }else{
            plotALL<-rbind(plotALL,tmpD)
         }

         #quantile()
         #probs
      }
    }
    binW<-c(0.16, 0.02)
                                                           
   
   p <-     ggplot(data=plotALL, aes(x = B2Btarget, y = F2Ftarget))
   #p <- p + geom_hex(alpha=0.6, aes(fill=..density..))
   p <- p + geom_hline(yintercept=0,colour="gray",size=1)        
   p <- p + geom_point(data=plotALL,aes(x = B2Btarget, y = F2Ftarget,colour=type),size= 1.5,alpha = 0.02)  #+ colorscale2
   p <- p + geom_hex(bins = 100) + colorscale
   p <- p + facet_grid(~ Species,scales = "free_y") 
   p<- p + xlim(-.1,4) + ylim(-0.1,1.2)
   # p <- p + geom_ribbon(data=plotALL,aes(ymin=F2Ftarget_hatDWN, ymax=F2Ftarget_hatUP,group=Species),fill=trndfill,col=trndln, linetype=1,size=sizeIN[1], alpha=alpha1)  
    #p <- p + geom_point(data=plotALL,aes(ymin=F2Ftarget_hatDWN, ymax=F2Ftarget_hatUP,group=type),alpha=.4,colour = trndln,size=.2) 
  # p <- p + geom_hex(binwidth = binW) + scale_fill_viridis() 
   # p <- p + geom_hex(aes(fill = stat(cut((count/max(count)),breaks = c(-Inf,0,.25,.5,.75,Inf),labels = F, right = T, include.lowest = T))),
   #  binwidth = c(0.16, 0.02)) + colorscale
   # # p <- p + scale_colour_gradientn(name = count) + colorscale
   # p <- p + geom_hex(aes(fill = stat(cut(log(count), breaks = log(c(0, 100, 200, 400, Inf)), labels = F, right = T, include.lowest = T))),binwidth = c(0.16, 0.02))
    
    p<- p + theme_light() +
      labs(x=NULL, y=NULL,
       title="",
       subtitle="",
       caption="") +
      theme(plot.subtitle=element_text(margin=margin(b=20))) +
      theme(legend.title=element_blank()) +
      theme(legend.position=lgnpos) +
      theme(legend.key.width = unit(.5, "cm")) +
      theme(legend.text=element_text(size=5)) +
      theme(legend.key.size=unit(.01, "cm")) +
      labs(x=expression(B[y]/B[0]), y=expression(~F[y]/F[target])) +
      #labs(tag=letters(1:6)) +
      theme(plot.margin=margin(t = 3, r = 3, b = 3, l =3)) 




      if(altTheme) p<- p+ theme_kir_EBM(sub_title_size=12,
                                   sub_title_just="l",
                                   plot_margin = margin(plot_marginIN),
                                   plot_title_margin = 1,
                                   subtitle_margin = 0,
                                   caption_margin = 0,
                                   axis_title_just = "cm") 

      p

}



# library(ggpubr)
# # Grouped Scatter plot with marginal density plots
# ggscatterhist(
#   iris, x = "Sepal.Length", y = "Sepal.Width",
#   color = "Species", size = 3, alpha = 0.6,
#   palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#   margin.params = list(fill = "Species", color = "black", size = 0.2)
#   )



