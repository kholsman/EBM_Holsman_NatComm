#'________________________________________________
#'Threshold plots
#'________________________________________________    
#'
PLOT_THRESHOLD<-function(
  sppIN="plk",
  sppINFOIN=sppINFO,
  multIN=10,
  collin=colorRampPalette(rev(RColorBrewer::brewer.pal(9, "YlGnBu"))),
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