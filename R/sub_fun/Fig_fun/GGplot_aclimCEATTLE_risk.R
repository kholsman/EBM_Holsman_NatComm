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