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
  flip    = TRUE,
  revy    = TRUE,
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
  
  if(revy) tmpall$timeframe<-factor(tmpall$timeframe,levels =rev(levels(tmpall$timeframe)))
  d    <-  data.frame(x = as.numeric(tmpall$timeframe), y = tmpall[,riskIN])
  # interpolate values from zero to y and create corresponding number of x values
  vals <-  lapply(d$y, function(y) seq(0, y, by = 0.1))
  y    <-  unlist(vals)
  mid  <-  rep(d$x, lengths(vals))
  d2   <-  data.frame(
    x    = mid - 0.4,
    xend = mid + 0.4,
    xmid = mid,
    Risk    = y,
    Riskend = y,
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
  d2$y2     <-  alphas[as.numeric(d2[,barsplit])]*d2$Risk
  d2$y1     <-  abs(alphas-1)[as.numeric(d2[,barsplit])]*d2$Risk
  lvl<- levels(d2[,barsplit])
  if(revbar) lvl<-rev(lvl)
  
  d3        <- d2
  d3$Risk<-d3$Riskend <- d2$Risk*NA
  cc  <-  which(d3$hcr[cumsum(lengths(vals))]==lvl[1])
  d3$Risk[cumsum(lengths(vals))][cc]<-d3$Riskend[cumsum(lengths(vals))][cc]<-1
  d3$Risk<-d3$Riskend<-d3$Risk*d2$Risk
  
  #d2$col2  <-  factor(d2$alpha1)
  
  p <-     ggplot(data = d2, aes(x = x, xend = xend, y = Risk, yend = Riskend, color = Risk))
  p <- p + facet_grid(d2[,rowvar]~d2[,colvar],scales=scalesIN)
  
  p <- p + geom_segment(data = d2, aes(x = x, xend = xend, y = Risk, yend = Riskend),color=backcol,size = 2,alpha=alphaIN)
  p <- p + geom_segment(data = d3, aes(x = x, xend = xend, y = Risk, yend = Riskend),color=backcol,size = 1,alpha=.5,linetype="solid")
  
  p <- p + geom_segment(data = d2, aes(x = x+.1, xend = xend+.1, y = y1, yend = y1,color=y1),linetype="solid",size = 1)
  # p <- p + geom_segment(data = d2, aes(x = x, xend = xend, y = y1, yend = y1, color = y1,alpha=alpha1),alpha=.05,size = 2)
  p <- p + scale_color_gradient2(low = coll[1], mid = coll[round(length(coll)/2)], high = coll[length(coll)],midpoint = max(d2$Risk)/2) 
  
  p <- p + geom_segment(data = d2, aes(x = .5, xend = max(d2$xend)+.5, y = 0, yend = 0),color="gray",linetype="solid",size = .5)
  
  p <- p + theme_light() +
    labs(x=NULL, y=NULL,
         title=titleIN,
         subtitle=subtitleIN,
         caption=captionIN) +
    theme(plot.subtitle=element_text(margin=margin(t = 0, r = 0, b = 20, l = 0))) +
    #theme(legend.title=element_blank()) +
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
  #p <- p + guides(color = guide_legend(override.aes=list(fill=NA)))
  p <- p + guides(size = "none", colour = "legend",linetype="none")
  p <- p + guides(shape = guide_legend(nrow = nrowlg[1]))
  p <- p + guides(color = guide_legend(nrow = nrowlg[1]))
  # if(!is.null(xlimmIN[1])) 
  p <- p + xlim(0.5,max(d2$xend)+.25) 
  p <- p + ylim(-10,110) 
  p <- p + theme(axis.text.x = element_text(angle=angleIN,vjust=xtick_vjust, hjust=xtick_hjust))
  
  xticks <- tapply(d2$xmid,d2$timeframe,mean)                        
 
  if(revy){
    p <- p +  scale_x_discrete(name=xlabb,
                               limits = rev(xticks), 
                               labels=rev(names(xticks) ))
  }else{
    p <- p +  scale_x_discrete(name=xlabb,
                               limits = xticks, 
                               labels=names(xticks) )
  }
  p <- p +  scale_y_discrete(name=ylabb,
                             limits = seq(0,100,25)[2:4], 
                             labels=c("low"=25,"medium"=50,"high"=75) )
  p <- p + guides(fill = TRUE)
  if(flip) p <- p + coord_flip() 
  p
}

GGplot_aclimCEATTLE_riskBAR()


GGplot_aclimCEATTLE_risk_droppt<-function(
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
  rev_lines    = TRUE,
  rev_group    = FALSE,
  rev_barsplit = FALSE,
  itr = 100,
  spacer  = 1.1,
  spacer_groups  = .4,
  spacer_lines  = 1.04,
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
    
    tmpall$timeframe  <-  factor(tmpall$timeframe,levels=levels(tmp1$timeframe))
    tmpall$rcp        <-  factor(tmpall$rcp,levels=levels(tmpall$rcp))
    tmpall$hcr        <-  factor(tmpall$hcr,levels=unique(tmpall$hcr))
    tmpall$sp         <-  factor(tmpall$sp,levels=levels(tmpall$sp))
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
    tmpall$shape    <-  tmpall[,barsplit]
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
    jitterTMP$shape      <-  jitterTMP[,barsplit]
    jitterTMP$linesize   <-  jitterTMP[,barsplit]
    
    alphas    <-  c(1,0)
    if(revbar) 
      alphas  <-  rev(alphas)
    lvl<- levels(d2[,barsplit])
    if(revbar) lvl<-rev(lvl)
  
    dataUSE <- dataUSE2 <- tmpall
    dataUSE$Risk <- dataUSE2$Risk <- dataUSE2$Risk*NA
    cc  <-  which(dataUSE2$hcr==lvl[1])
    dataUSE2$Risk[cc]  <-  1
    dataUSE$Risk[-cc]  <-  1
    dataUSE2$Risk <- dataUSE2$Risk*tmpall$Risk
    dataUSE$Risk  <- dataUSE$Risk*tmpall$Risk
    
    jitterUSE <- jitterUSE2 <- jitterTMP
    jitterUSE$Risk <- jitterUSE2$Risk <- jitterUSE$Risk*NA
    cc  <-  which(jitterUSE$hcr==lvl[1])
    jitterUSE2$Risk[cc]  <-  1
    jitterUSE$Risk[-cc]  <-  1
    jitterUSE2$Risk <- jitterUSE2$Risk*jitterTMP$Risk
    jitterUSE$Risk  <- jitterUSE$Risk*jitterTMP$Risk
    
    
  p <- ggplot(data = dataUSE, aes(x =mid-.4 , xend = mid+.4, y = Risk, yend = Risk, 
                                 color = Risk,
                                 linetype=linetype,
                                 shape=shape,
                                 linesize=linesize))
    if(flip) 
      p <-   p+ coord_flip() 
  p <-  p +geom_jitter(data=jitterUSE,aes(x =mid ,y = Risk),size = 2, alpha = alphaIN, width = 0.03,col="gray")+
    scale_y_continuous(limits = c(0, 110), expand = c(0.005, 0.005)) +
    geom_hline(aes(yintercept = 25), color = "gray70", size = 0.6)
  p <-  p + geom_segment(data = dataUSE, aes(x =mid , xend = mid, y = 0, yend = Risk),size = 0.8,col="gray") 
  p <-  p + geom_point(data = dataUSE, aes(x =mid ,y = Risk),size = 5,color="gray")
  
  p <- p +geom_jitter(data=jitterUSE2,aes(x =mid ,y = Risk,color=Risk),size = 2, alpha = alphaIN, width = 0.03,inherit.aes = FALSE)
  p <-   p + geom_segment(data = dataUSE2, aes(x =mid , xend = mid, y = 0, yend = Risk),size = 0.8) 
  p <-  p + geom_point(data = dataUSE2, aes(x =mid ,y = Risk),size = 5) +
    scale_y_continuous(limits = c(0, 100), expand = c(0.005, 0.005)) +
    geom_hline(aes(yintercept = 25), color = "gray70", size = 0.6)
  p <- p + facet_grid(tmpall[,rowvar]~1,scales=scalesIN)
  #p <- p + scale_color_gradient2(low = coll[1], mid = coll[round(length(coll)/2)], high = coll[length(coll)],midpoint = max(d2$Risk)/2) 
  p<- p + scale_color_gradientn(colors=coll)
  
  p
  
  
  p <- p + theme_light() +
    labs(x=NULL, y=NULL,
         title=titleIN,
         subtitle=subtitleIN,
         caption=captionIN) +
    theme(plot.subtitle=element_text(margin=margin(t = 0, r = 0, b = 20, l = 0))) +
    #theme(legend.title=element_blank()) +
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
  #p <- p + guides(color = guide_legend(override.aes=list(fill=NA)))
  p <- p + guides(size = "none", colour = "legend")
  p <- p + guides(shape = guide_legend(nrow = nrowlg[1]))
  p <- p + guides(linetype = guide_legend(nrow = nrowlg[1]))
  p <- p + guides(color = guide_legend(nrow = nrowlg[1]))
  # if(!is.null(xlimmIN[1])) 
  p <- p + xlim(0.5,max(d2$xend)+.25) 
  p <- p + ylim(0,105) 
  p <- p + theme(axis.text.x = element_text(angle=angleIN,vjust=xtick_vjust, hjust=xtick_hjust))
  
  xticks <- tapply(tmpall$mid,tmpall[,between_group],mean)   
  
  
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
}


GGplot_aclimCEATTLE_risk_droppt(lgnpos= "bottom",angleIN=0,spacer=1.1,spacer_groups=1.6,spacer_lines=.4,itr=100)


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
  
  tmpall$timeframe  <-  factor(tmpall$timeframe,levels=levels(tmp1$timeframe))
  tmpall$rcp        <-  factor(tmpall$rcp,levels=levels(tmpall$rcp))
  tmpall$hcr        <-  factor(tmpall$hcr,levels=unique(tmpall$hcr))
  tmpall$sp         <-  factor(tmpall$sp,levels=levels(tmpall$sp))
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
  lvl<- levels(d2[,barsplit])
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
  p <-  p +geom_jitter(data=jitterUSE,aes(x =mid ,y = Risk),size = 2, alpha = alphaIN, width = 0.03,col="gray")+
    scale_y_continuous(limits = c(0, 110), expand = c(0.005, 0.005)) 
  #p <-  p + geom_point(data = dataUSE, aes(x =mid ,y = Risk),size = 5,color="gray")
  
  p <- p +geom_jitter(data=jitterUSE2,aes(x =mid ,y = Risk,color=Risk),size = 2, alpha = alphaIN, width = 0.03,
                      inherit.aes = FALSE)
  p <-  p + geom_segment(data = dataUSE, aes(x =mid-spacer/5 , xend = mid+spacer/5+spacer_lines/10, y = Risk, yend = Risk),
                         linetype="solid",size = 0.8,col="gray70") 
  p <-   p + geom_segment(data = dataUSE2, aes(x =mid , xend = mid, y =Riskend , yend = Risk),size=1.4) 
  p <-  p + geom_point(data = dataUSE2, aes(x =mid ,y = Risk),size = 5,alpha=.9) +
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
    #theme(legend.title=element_blank()) +
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
  #p <- p + guides(color = guide_legend(override.aes=list(fill=NA)))
  p <- p + guides(size = "none", colour = "legend")
  p <- p + guides(shape = guide_legend(nrow = nrowlg[1]))
  p <- p + guides(linetype = guide_legend(nrow = nrowlg[1]))
  p <- p + guides(color = guide_legend(nrow = nrowlg[1]))
  # if(!is.null(xlimmIN[1])) 
  p <- p + xlim(0.5,max(d2$xend)+.25) 
  p <- p + ylim(0,105) 
  p <- p + theme(axis.text.x = element_text(angle=angleIN,vjust=xtick_vjust, hjust=xtick_hjust))
  
  xticks <- tapply(tmpall$mid,tmpall[,between_group],mean)   
  
  
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
}


GGplot_aclimCEATTLE_risk_droppt2(lgnpos= "bottom",angleIN=0,spacer=1.1,spacer_groups=1.6,spacer_lines=.4,itr=100,
                                 colvar  ="timeframe",
                                 between_group = "type",
                                 barsplit ="hcr")

GGplot_aclimCEATTLE_risk_droppt2(lgnpos= "bottom",angleIN=0,spacer=1.1,spacer_groups=1.6,spacer_lines=.4,itr=100,
                                 colvar  ="type",
                                 between_group = "timeframe",
                                 barsplit ="hcr")


#p <- p + geom_segment(data = d2, aes(y = .5, yend = max(d2$xend)+.5, x = 0, xend = 0),color="gray",linetype="solid",size = .5)


#     annotate("text", x = 6.3, y = 35, family = "Poppins", size = 2.7, color = "gray20",
#              label = glue::glue("Worldwide average:\n{round(world_avg, 1)} students per teacher")) +
#     annotate("text", x = 3.5, y = 10, family = "Poppins", size = 2.7, color = "gray20",
#              label = "Continental average") +
#     annotate("text", x = 1.7, y = 11, family = "Poppins", size = 2.7, color = "gray20",
#              label = "Countries per continent") +
#     annotate("text", x = 1.9, y = 64, family = "Poppins", size = 2.7, color = "gray20",
#              label = "The Central African Republic has by far\nthe most students per teacher"))
# 
# 
# 
# jitterTMP$x<-jitterTMP$mid
# jitterTMP$y<-jitterTMP$Risk
# p<- ggplot(data=jitterTMP,aes(x = 'mid', y = `Risk`, fill = ..x..)) + 
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#   scale_fill_viridis(name = "lgndtxt", option = "C") +
#   labs(title = title)
# 
# 
# 
# 
# #p <- p + annotate("text", x = 6.3, y = 35, family = "Poppins", size = 2.7, color = "gray20",
# #         label = glue::glue("Worldwide average:\n{round(world_avg, 1)} students per teacher"))
# 
# #p <- p + ggplot(data = d2, aes(y = x, yend = xend, x = Risk, xend = Riskend, color = Risk),size = 0.8) +
# p
# 
# geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
#   stat_summary(fun.y = mean, geom = "point", size = 5) +
#   geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
#   annotate("text", x = 6.3, y = 35, family = "Poppins", size = 2.7, color = "gray20",
#            label = glue::glue("Worldwide average:\n{round(world_avg, 1)} students per teacher"))

