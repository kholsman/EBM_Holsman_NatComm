
GGplot_aclimCEATTLE_risk_droppt2<-function(
  esm_namesIN=simnames,
  RISK    = list("no cap" = risk12,"2 MT cap" = risk13),
  RISKTYPES = riskTypes,
  modeIN  = c("MSM"),
  rcpIN   = c("RCP 8.5"="rcp85"),
  deltaIN = FALSE,
  plotSet = list("RCP 8.5" = c(1,rcp85NoBio_n)),
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
  
  tmp1      <-  (RISK[[1]])%>%filter(sp%in%spIN,mode%in%modeIN, type%in%RISKTYPES,rcp%in%rcpIN)
  tmp2      <-  (RISK[[2]])%>%filter(sp%in%spIN,mode%in%modeIN, type%in%RISKTYPES,rcp%in%rcpIN)
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
  bins      <-  unique(tmpall$YrBin)
  
  tmpall$timeframe  <-  factor(tmpall$YrBin,levels=bins)
  tmpall$rcp        <-  factor(tmpall$rcp,levels=rcpIN)
  tmpall$hcr        <-  factor(tmpall$hcr,levels=hcrs)
  tmpall$sp         <-  factor(tmpall$sp,levels=spIN)
  tmpall$mode       <-  factor(tmpall$mode,levels=modeIN)
  tmpall$type       <-  factor(tmpall$type,levels=RISKTYPES)
  
  if(rev_lines) tmpall[,colvar]        <-  factor(tmpall[,colvar],levels =rev(levels(tmpall[,colvar])))
  if(rev_group) tmpall[,between_group] <-  factor(tmpall[,between_group],levels =rev(levels(tmpall[,between_group])))
  if(rev_barsplit) tmpall[,barsplit]   <-  factor(tmpall[,barsplit],levels =rev(levels(tmpall[,barsplit])))
  
  dev.new(height=h,width=w)
  maxx      <-  0
  for(i in 1:length(plotSet))
    maxx    <-  max(length(plotSet[[i]]),maxx)
  
  splevels        <-  paste0(letters[1:3],") ",c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
  splevels        <-  paste0(c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
  # if(length(plotSet)>1){
  #   correctOrder    <-  c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])  # system constant in correct sort order.
  #   correctOrder    <-  factor(paste(1:length(correctOrder),correctOrder))
  #   correctOrderLab <-  c(esm_namesIN[plotSet[[1]]],esm_namesIN[plotSet[[2]]][-1])
  # }
  
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
                          sd   = tmpall[,riskINsd][i]),
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