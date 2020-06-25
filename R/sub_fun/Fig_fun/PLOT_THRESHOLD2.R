#'________________________________________________
#' Threshold plots
#'________________________________________________    
#'
PLOT_THRESHOLD2<-function( 
  ntemps      = 3,
  byrow       = "species",
  bycol       = "cap",
  ylabbIN     = expression(paste("", Delta,"Catch")) ,
  sppINFOIN   = sppINFO,
  multIN      = 5,
  collin      = colorRampPalette(rev(RColorBrewer::brewer.pal(9, "YlGnBu"))),
  firstdiff   = TRUE,
  plot_marginIN = c(5,5,5,5),
  lgnpos      = c(.95,1.12),
  trndfill    = "white",
  shp         = 16,
  ptsize      = 2,
  binW        = c(0.2,0.1),
  ylimmIN     = c(-1,1.5),
  xlimmIN     = c(1,7),
  trndln      = "white",
  trndln2     = Ornjazz[3],
  tipping     = Ornjazz[5],
  sizeIN      = c(0.1,.3,.75,2),
  binwidthIN =  c(0.2, 5),
  nspp        = 3,
  alphacore   = 1,
  altTheme    = T,
  sublab      = TRUE,
  sublab_adj  =.95,
  dataIN    = list(
    "no cap"   = list(C_thresh_12_1,C_thresh_12_2,C_thresh_12_3),
    "2 MT cap" = list(C_thresh_13_1,C_thresh_13_2,C_thresh_13_3))){
  
  require(RColorBrewer)
  
  makeNA     <- function(x){ if(length(x)==0){ return(NA)}else{return(x)} }
  diftxt     <- "s'(x)"
  if(!firstdiff)  
    diftxt   <- "s''(x)"
  
  ndat <- length(dataIN)
  # create plotting matrix for facet_wrap
  p_thresh   <- matrix(FALSE,2,nspp)
  
  for(d in 1:ndat){
    for(s in 1:nspp){
      plotD  <- dataIN[[d]][[s]]
      
      if(!is.na(unlist(plotD$thrsh_max1)[1]))
        p_thresh[d,s]  <-  TRUE   
      if(firstdiff)  diffn <- plotD$fdif1
      if(!firstdiff) diffn <- plotD$fdif2  # use second differential
      
      if(s==1&d==1){
        plotALL       <- data.frame(
                          deltaC  = plotD$datIN$delta_var_prcnt,
                          TempC   = plotD$datIN$TempC,
                          species = sppINFO[[s]]$plotSPP,
                          cap     = names(dataIN)[d])
        hat_se         <- (plotD$hat$up-plotD$hat$mn)
        plotD$hat$up   <- plotD$hat$mn+multIN*(hat_se)
        plotD$hat$dwn  <- plotD$hat$mn-multIN*(hat_se)
        
        hatALL      <- data.frame(plotD$hat,species=sppINFO[[s]]$plotSPP,cap=names(dataIN)[d])
        fdif1ALL    <- data.frame(diffn,species=sppINFO[[s]]$plotSPP)
        arrws       <- data.frame(
                          cap=names(dataIN)[d],
                          phase=NA, thrsh=NA, ix=NA,absval=NA,dwn=NA,
                                  TempC=NA,hatC=NA,sigdf1=NA,sigdf2=NA,
                                  species=sppINFO[[s]]$plotSPP)  # tresholds
        
        if(p_thresh[d,s]) 
          arrws       <- data.frame(
                          cap     = names(dataIN)[d],
                          phase   = NA,
                          thrsh   = plotD$hat$tmp[plotD$thrsh_max1], 
                          ix      = plotD$thrsh_max1,
                          absval  = NA,
                          dwn     = plotD$hat$up[plotD$thrsh_max1],
                          TempC   = plotD$hat$tmp[plotD$thrsh_max1],
                          hatC    = plotD$hat$mn[plotD$thrsh_max1],
                          sigdf1  = plotD$signif1[plotD$thrsh_max1],
                          sigdf2  = plotD$signif2[plotD$thrsh_max1],
                          species = sppINFO[[s]]$plotSPP)  # tresholds
        
        tmpth                 <- plotD$hat$mn*NA
        tmpth[plotD$signif1]  <- as.numeric(plotD$hat$mn[plotD$signif1])
        
        tmpth2                <- plotD$hat$mn*NA
        tmpth2[plotD$signif2] <- as.numeric(plotD$hat$mn[plotD$signif2])
        
        threshALL   <- data.frame(
                        TempC   = plotD$hat$tmp,
                        cap     = names(dataIN)[d],
                        sigdf1  = tmpth,
                        sigdf2  = tmpth2, 
                        thrsh   = makeNA(plotD$thrsh_max1),
                        species = sppINFO[[s]]$plotSPP)
        
      }else{
        plotALL    <- rbind(plotALL,
                            data.frame(
                              deltaC  = plotD$datIN$delta_var_prcnt,
                              TempC   = plotD$datIN$TempC,
                              species = sppINFO[[s]]$plotSPP,
                              cap     = names(dataIN)[d]))
        
        hat_se         <- (plotD$hat$up-plotD$hat$mn)
        plotD$hat$up   <- plotD$hat$mn+multIN*(hat_se)
        plotD$hat$dwn  <- plotD$hat$mn-multIN*(hat_se)
        
        hatALL     <- rbind(hatALL,data.frame(plotD$hat,species=sppINFO[[s]]$plotSPP,cap=names(dataIN)[d]))
        fdif1ALL   <- rbind(fdif1ALL,data.frame(diffn,species=sppINFO[[s]]$plotSPP))
        tmparrws   <- data.frame(cap=names(dataIN)[d],phase=NA, thrsh=NA, ix=NA,    absval=NA,dwn=NA,
                                 TempC=NA, hatC=NA,sigdf1 =NA,sigdf2 =NA,
                                 species=sppINFO[[s]]$plotSPP)  # tresholds
        
        if(p_thresh[d,s]) 
          tmparrws <- data.frame(
                            cap    =  names(dataIN)[d],
                            phase  = NA,
                            thrsh  = plotD$hat$tmp[plotD$thrsh_max1], 
                            ix     = plotD$thrsh_max1,
                            absval = NA,
                            dwn    = plotD$hat$up[plotD$thrsh_max1],
                            TempC  = plotD$hat$tmp[plotD$thrsh_max1],
                            hatC   = plotD$hat$mn[plotD$thrsh_max1],
                            sigdf1 = plotD$signif1[plotD$thrsh_max1],
                            sigdf2 = plotD$signif2[plotD$thrsh_max1],
                            species= sppINFO[[s]]$plotSPP)
        arrws                 <- rbind(arrws,tmparrws)
        tmpth                 <- plotD$hat$mn*NA
        tmpth[plotD$signif1]  <- as.numeric(plotD$hat$mn[plotD$signif1])
        
        tmpth2                <- plotD$hat$mn*NA
        tmpth2[plotD$signif2] <- as.numeric(plotD$hat$mn[plotD$signif2])
        for(ii in 1:length(makeNA(plotD$thrsh_max1)))
          threshALL   <- rbind(threshALL,
                               data.frame(
                                 TempC   = plotD$hat$tmp,
                                 cap     = names(dataIN)[d],
                                 sigdf1  = tmpth,
                                 sigdf2  = tmpth2,
                                 thrsh   = makeNA(plotD$thrsh_max1)[ii],
                                 species = sppINFO[[s]]$plotSPP))
      }
    }
  }
  
  arrws$TempC        <- plotD$hat$tmp[arrws$ix]
  colnames(fdif1ALL)[1:4] <- colnames(hatALL)[1:4]<-c("TempC","up","deltaC","dwn")
  hatALL$type        <- as.factor("s(x)")
  fdif1ALL$type      <- as.factor(diftxt)
  hatALL$species     <- as.factor(hatALL$species);fdif1ALL$species<-as.factor(fdif1ALL$species)
  fdif1ALL$group     <- (paste0(fdif1ALL$type,"_",fdif1ALL$species))
  hatALL$group       <- (paste0(hatALL$type,"_",hatALL$species))
  #ALLDAT             <- rbind(hatALL,fdif1ALL)
  ALLDAT             <- hatALL
  nc                 <- length(plotALL$TempC)
  alpha1             <- .4
  
  colorscale = scale_fill_gradientn(
    colors = collin(9),
    values = c(0, exp(seq(-5, 0, length.out = 100))))
   
  plotALL$type       <- "s(x)"
  plotALL$deltaCAKA  <- plotALL$deltaC
  plotALL$deltaC[plotALL$deltaC>ylimmIN[2]+.5]  <- NA
  plotALL$deltaC[plotALL$deltaC<ylimmIN[1]-.5]  <- NA
  
  threshALL$type     <- "s(x)"
  arrws$type         <- "s(x)"
  arrws$end          <- ylimmIN[1]-.5
  ulist              <- data.frame(group = unique(ALLDAT$group),species = NA,type = NA)
  for (uu in 1:length(ulist$group)){
    ulist$species[uu]   <- as.character(ALLDAT$species[ALLDAT$group==ulist$group[uu]][1])
    ulist$type[uu]      <- as.character(ALLDAT$type[ALLDAT$group==ulist$group[uu]][1])
  }
  blank_data            <- data.frame(
                              species = rep(ulist[,2],2),
                              type    = rep(ulist[,3],2),
                              x       = 0,
                              y       = c(rep(ylimmIN[1],3),rep(-.5,3),rep(ylimmIN[2],3),rep(.5,3)))
  
  species_labeller <- function(variable,value){
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
  
  maxnarrw     <-  max(tapply(plotarrows$species,plotarrows$species,length)/4)
  ncol         <-  dim(plotarrows)[2]
  #plotarrows[plotarrows$type=="s(x)",]$plotset
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
  
  plotALL$type <- factor(plotALL$type,levels=c("s(x)",diftxt))
  
  p <-     ggplot(data=plotALL, aes(x = TempC, y = deltaC),colour=TempC)
  p <- p + geom_hline(yintercept=0,colour="gray",size=1)                 
  p <- p + geom_hex(binwidth = binwidthIN) + colorscale
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
