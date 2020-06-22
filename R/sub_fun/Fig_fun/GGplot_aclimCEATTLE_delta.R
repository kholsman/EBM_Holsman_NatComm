#'-------------------------------------
#' GGplot_aclimTS
#'-------------------------------------
#' PLot ACLIM covariates using GGplot
#'
#' @param dat is a data.frame with t=date and dat=data to plot
GGplot_aclimCEATTLE_delta<-function(
  esm_namesIN=simnames,
  ageLIST = list(6,6),
  nmLIST  = list("2 MT cap"="dat_2_5_13","no cap"="dat_2_5_12"),
  nmLIST_mc = list(dat1=dat_2_5_13_mc,dat2=dat_2_5_12_mc),
  valLIST = list(valIn1="SSB0_total_biom", valIn2="SSB0_total_biom"),
  hLIST   = list("H13_2MT_219_CENaivecf1","H12_219_CENaivecf"),
  plotSet = list("RCP 4.5" = c(1,rcp45_n),"RCP 8.5" = c(1,rcp85NoBio_n)),
  deltaIN = FALSE,
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
  coll    = list(c(colors()[320],col2(6)[c(2,3,4)]),
                 c(colors()[320],col3(6)[c(3,4,6)])),
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
  plot_marginIN       = c(1, 1, 1, 1),
  plot_title_marginIN = 0,
  subtitle_marginIN   = 0,
  caption_marginIN    = 0){
  
  
  dev.new(height=h,width=w)
  DAT    <- deltaDAT    <- list()
  ndat   <- length(nmLIST)
  nset   <-  length(plotSet)  # number of contrasts
  maxx   <-  0
  for(i in 1:length(plotSet))
    maxx <-  max(length(plotSet[[i]]),maxx)
  splevels         <- paste0(c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
  splabels         <- paste0(c(sppINFO[[1]]$plotSPP,sppINFO[[2]]$plotSPP,sppINFO[[3]]$plotSPP))
  rcplevels        <- names(plotSet)
  rcplabels        <- names(plotSet)
  
  sp.labs          <- splevels
  names(sp.labs)   <- splabels
  rcp.labs         <- rcplevels
  names(rcp.labs)  <- rcplabels
  nHCR             <- length(names(nmLIST))
  hcr              <- names(nmLIST)        
  mnNAME           <- paste0('prob',prob[((length(prob)-1)/2)+1]*100,collapse=",")
  col_df           <- data.frame(coll =  as.character(unlist(coll)), Scenario = unlist(plotSet))
  probNames        <- paste0('prob',prob*100)
  nprob            <- length(probNames)
  mnprobNames      <- paste0('mnhind_prob',prob*100)
  p_names          <- map_chr(prob, ~paste0(.x*100, "%"))
  p_funs           <- purrr::map(prob, ~purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>% purrr::set_names(nm = probNames)
  # p_funs
  

  # get and reshape data
  for(ll in 1:ndat){
    
    eval(parse(text = paste("tmpd<- ",nmLIST[[ll]]) ))
    
    tmpd <- tmpd%>% 
      filter(age==ageLIST[[ll]],
             Scenario%in%plotSet[[ll]], 
             hModev2==hLIST[[ll]])         %>%
      select(names(tmpd),sp = species)     %>%
      mutate(Year = start_yr + future_year-1,
             scenario = Scenarios[Scenario],
             rcp  = factor(names(plotSet)[ll],levels=names(plotSet)),
             HCR  = factor(names(nmLIST)[ll],levels=names(nmLIST)), 
             coll = col_df$coll[match(Scenario,col_df$Scenario)],
             var_nm  = valLIST[[ll]] )
    
    tmp_delta <-  calcDelta(datIN   = tmpd,
                       limm         = -10,
                       delta_var_nm = valLIST[[ll]] ) 
    tmpd      <- tmpd %>%
      select(Year,rcp,HCR,sp,bottomT_C,coll,scenario,Scenario,var_nm,value = valLIST[[ll]])
    tmpd <- merge(tmpd,tmp_delta,by=c("sp", "Year", "Scenario","rcp"),all.x=T)
    
    rm(tmp_delta)
    
  # now for MC runs:
    
    tmpdmc <- nmLIST_mc[[ll]]
    
    tmpdmc <- tmpdmc%>% 
      filter(age==ageLIST[[ll]],
             Scenario%in%plotSet[[ll]], 
             hModev2==hLIST[[ll]])         %>%
      select(names(tmpdmc),sp = species)     %>%
      mutate(Year = start_yr + future_year-1,
             scenario = Scenarios[Scenario],
             rcp  = factor(names(plotSet)[ll],levels=names(plotSet)),
             HCR  = factor(names(nmLIST)[ll],levels=names(nmLIST)), 
             coll = col_df$coll[match(Scenario,col_df$Scenario)],
             var_nm  = valLIST[[ll]] )
    
    tmp_delta <-  calcDelta(datIN   = tmpdmc,
                            limm         = -10,
                            delta_var_nm = valLIST[[ll]] ) 
    tmpdmc      <- tmpdmc %>%
      select(Year,MC_n,rcp,HCR,sp,bottomT_C,coll,scenario,Scenario,var_nm,value = valLIST[[ll]])
    
    tmpd2 <- merge(tmpdmc,tmp_delta,by=c("sp","MC_n", "Year", "Scenario","rcp"),all.x=T)
    
    tmpd2      <- tmpd2 %>%
      group_by(Year,rcp,HCR,sp,bottomT_C,coll,scenario,Scenario,var_nm)%>%
      summarize_at(vars(value), funs(!!!p_funs))
    
    tmpd3 <- merge(tmpd2,tmpd,by=c("sp", "Year", "Scenario","rcp","HCR",
                                   "bottomT_C","coll","scenario","var_nm"),all.x=T)
    
    # now get quantiles:
    rm(list=c("tmpd","tmpd2","tmp_delta"))
    
    if(ll==1){
      DAT <- tmpd3
    }else{
      DAT <- rbind(DAT,tmpd3)
    }
  }
  DAT$pch <- pchh[as.numeric(factor(DAT$HCR))]
  nspp    <-  length(unique(deltaDAT $sp))
  yr      <-  sort(unique(DAT$Year))
  
  # Now plot it:
  
  p <-     ggplot(DAT%>%filter(HCR==hcr[1]), aes(x = Year, y = value/ydiv),colour=scenario)
  p <- p + facet_grid(sp~rcp,scales=scalesIN, labeller = labeller(sp = sp.labs, rcp = rcp.labs))
  p <- p + geom_vline(aes(xintercept=projLine),col="gray",size=1,linetype="dashed") 
  pchIN <-  DAT%>%filter(HCR==hcr[1])%>%select(pch)
  # add moving average:
  if(plotpersist)
    p <-  p+ geom_line( aes(x = Year, y = value/ydiv,colour=scenario,linetype=HCR))
  
  for(nn in 1:((nprob-1)/2))
    eval(parse(text=paste0("p <-  p+ geom_ribbon(aes(x = Year, ymin = ",probNames[nn],"/ydiv, ymax =",probNames[1+(nprob-nn)],"/ydiv,fill=scenario),alpha=alpha[2]/100)")))
  p <-  p + geom_line(aes(x = Year, y = prob50/ydiv,colour=scenario,linetype=HCR),size=.7)
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
