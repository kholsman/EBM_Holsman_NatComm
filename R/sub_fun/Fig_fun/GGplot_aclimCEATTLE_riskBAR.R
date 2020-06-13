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