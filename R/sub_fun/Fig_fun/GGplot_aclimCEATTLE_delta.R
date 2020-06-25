#'-------------------------------------
#' GGplot_aclimTS
#'-------------------------------------
#' PLot ACLIM covariates using GGplot
#'
#' @param dat is a data.frame with t=date and dat=data to plot
GGplot_aclimCEATTLE_delta<-function(
  esm_namesIN = simnames,
  ageLIST     = list(6,6),
  nmLIST      = list("2 MT cap"="dat_2_5_13","no cap"="dat_2_5_12"),
  nmLIST_mc   = list(dat1=dat_2_5_13_mc,dat2=dat_2_5_12_mc),
  valLIST     = list(valIn1="SSB0_total_biom", valIn2="SSB0_total_biom"),
  hLIST       = list("H13_2MT_219_CENaivecf1","H12_219_CENaivecf"),
  hind        = 1,
  plotSet     = list("hind"=1, "RCP 4.5" = c(rcp45_n),"RCP 8.5" = c(rcp85NoBio_n)),
  coll        = list(colors()[320],coll_use[2:4],coll_use[5:7]),
  deltaIN     = FALSE,
  h           = 3,
  w           = 4.75,
  plotpersist = TRUE,
  showlinetype= FALSE,
  plot_levels  = simnames,
  ylimm_up    = c(20,2,1.5)*1e6,
  ylimm_dwn   = c(0,0,0),
  xlimmIN     = NULL,
  scalesIN    = "free_y",
  sublab      = TRUE,
  sublab_adj  = 0.95,
  lgnpos  = "bottom",
  fn      = "BT",
  ltyy    = c("solid","solid"),
  pchh    = c(1,16),
  lwdd    = c(.7,.4),
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
  alpha      = c(20,20,40), 
  prob       = c(.1,.50,.9),
  plot_marginIN       = c(1, 1, 1, 1),
  plot_title_marginIN = 0,
  subtitle_marginIN   = 0,
  caption_marginIN    = 0){
  
  
  dev.new(height=h,width=w)
  ndat             <- length(nmLIST)
  nset             <- length(plotSet)  # number of contrasts
  maxx             <- 0
  for(i in 1:length(plotSet))
    maxx <-  max(length(plotSet[[i]]),maxx)
  sp.labs           <-  rep("",nspp)
  for(s in 1:nspp)
    sp.labs[s] <- sppINFO[[s]]$plotSPP
  rcp.labs         <- names(plotSet)
  nHCR             <- length(names(nmLIST))
  hcr              <- names(nmLIST)        
  mnNAME           <- paste0('prob',prob[((length(prob)-1)/2)+1]*100,collapse=",")
  deltaRCP         <-  list(rcp45=grep("rcp45",Scenarios),rcp85=grep("rcp85",Scenarios))
  deltaRCP_df      <- data.frame(rcp =  c("hind",rep.int(names(deltaRCP),times=as.numeric(lengths(deltaRCP)))), 
                                 scen=c(1,as.numeric(unlist(deltaRCP))))
  rcp_col_df       <- data.frame(coll     =  as.character(unlist(coll)), 
                                 Scen_n   = c(hind,unlist(plotSet)),
                                 Scenario = factor(Scenarios[c(hind,unlist(plotSet))],levels=plot_levels),
                                 rcp2      =  c("hind",rep.int(names(plotSet),times=as.numeric(lengths(plotSet)))))
  rcp_col_df$rcp   <- deltaRCP_df$rcp[match(rcp_col_df$Scen_n,deltaRCP_df$scen)]    
  rcp_col_df$coll  <- as.character(rcp_col_df$coll)
  probNames        <- paste0('prob',prob*100)
  probNamesDelta   <- paste0('prob_delta',prob*100)
  nprob            <- length(probNames)
 
  mnprobNames      <- paste0('mnhind_prob',prob*100)
  p_names          <- map_chr(prob, ~paste0(.x*100, "%"))
  p_funs           <- purrr::map(prob, ~purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    purrr::set_names(nm = probNames)
  p_fun_delta      <- purrr::map(prob, ~purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    purrr::set_names(nm = probNamesDelta)
  # p_funs
  
  # get and reshape data
  for(ll in 1:ndat){
    
    eval(parse(text = paste("tmpd<- ",nmLIST[[ll]]) ))
    
    tmpd <- tmpd%>% 
      filter(age==ageLIST[[ll]],
             hModev2==hLIST[[ll]])         %>%
      select(names(tmpd),sp = species)     %>%
      mutate(Year = start_yr + future_year-1,
             scenario  = rcp_col_df$Scenario[ match(Scenario,rcp_col_df$Scen_n)],
             rcp  = rcp_col_df$rcp[ match(Scenario,rcp_col_df$Scen_n)],
             rcp2 = rcp_col_df$rcp2[ match(Scenario,rcp_col_df$Scen_n)],
             coll = rcp_col_df$coll[match(Scenario,rcp_col_df$Scen_n)],
             HCR  = factor(names(nmLIST)[ll],levels=names(nmLIST)), 
             var_nm  = valLIST[[ll]] )
    
    tmp_delta <-  calcDelta(datIN   =  tmpd,
                       YrbinIN      =  c(2017,2105),
                       limm         =  -10,
                       delta_var_nm =  valLIST[[ll]] ) 
    
   
     tmpd      <- tmpd %>%
      select(Year,rcp,rcp2,HCR,sp,bottomT_C,coll,scenario,Scenario,var_nm,value = valLIST[[ll]])
    tmpd <- merge(tmpd,tmp_delta,by=c("sp", "Year", "Scenario","rcp"),all.x=T)
    
    rm(tmp_delta)
    
  # now for MC runs:
    tmpdmc <- nmLIST_mc[[ll]]
    
    tmpdmc <- tmpdmc%>% 
      filter(age==ageLIST[[ll]],
             hModev2==hLIST[[ll]])         %>%
      select(names(tmpdmc),sp = species)     %>%
      mutate(Year = start_yr + future_year-1,
             scenario  = rcp_col_df$Scenario[ match(Scenario,rcp_col_df$Scen_n)],
             rcp  = rcp_col_df$rcp[ match(Scenario,rcp_col_df$Scen_n)],
             rcp2 = rcp_col_df$rcp2[ match(Scenario,rcp_col_df$Scen_n)],
             coll = rcp_col_df$coll[match(Scenario,rcp_col_df$Scen_n)],
             HCR  = factor(names(nmLIST)[ll],levels=names(nmLIST)), 
             var_nm  = valLIST[[ll]] )
    
    tmp_delta <-  calcDelta(datIN   = tmpdmc,
                            YrbinIN      =  c(2017,2105),
                            limm         = -10,
                            delta_var_nm = valLIST[[ll]] ) 
    
    tmpdmc      <- tmpdmc %>%
      select(Year,MC_n,rcp,rcp2,HCR,sp,bottomT_C,coll,scenario,Scenario,var_nm,value = valLIST[[ll]])
    
    tmpd2 <- merge(tmpdmc,tmp_delta,by=c("sp","MC_n", "Year", "Scenario","rcp"),all.x=T)
    
    tmpd2_a      <- tmpd2 %>%
      group_by(Year,rcp,rcp2,HCR,sp,bottomT_C,coll,scenario,Scenario,var_nm)%>%
      summarize_at(vars(value), funs(!!!p_funs))
    
    tmpd2_d      <- tmpd2 %>%
      group_by(Year,rcp,rcp2,HCR,sp,bottomT_C,coll,scenario,Scenario,var_nm)%>%
      summarize_at(vars(delta_var_prcnt), funs(!!!p_fun_delta))
    
    
    tmpd3 <- merge(tmpd2_a,tmpd,by=c("sp", "Year", "Scenario","rcp","rcp2","HCR",
                                   "bottomT_C","coll","scenario","var_nm"),all.x=T)
    tmpd3 <- merge(tmpd2_d,tmpd3,by=c("sp", "Year", "Scenario","rcp","rcp2","HCR",
                                   "bottomT_C","coll","scenario","var_nm"),all.x=T)
    
    rm(list=c("tmpd","tmpd2","tmp_delta"))
    
    if(ll==1){
      DAT <- tmpd3
    }else{
      DAT <- rbind(DAT,tmpd3)
    }
  }
  
  sub              <- DAT%>%filter(rcp2=="hind")
  for(i in 1:nset){
    subtmp     <- sub
    subtmp$rcp2 <- factor(names(plotSet)[i],levels=names(plotSet))
    if(i==1){
      subOUT   <- subtmp
    }else{
      subOUT   <- rbind(subOUT,subtmp)
    }
    rm(subtmp)
  }
  
  DAT              <- DAT%>%filter(rcp2!="hind")
  DAT              <- rbind(DAT, subOUT)
  rm(list=c("sub","subOUT"))
  DAT$rcp2         <-  factor(DAT$rcp2,levels=names(plotSet))
  DAT$pch          <-  pchh[as.numeric(factor(DAT$HCR))]
  nrcps            <-  length(unique(DAT$rcp2))
  rcps             <-  factor(unique(DAT$rcp2),levels=levels(DAT$rcp2))
  yr               <-  sort(unique(DAT$Year))
  DAT$species      <-  factor(sp.labs[DAT$sp],levels = sp.labs)
  #DAT$coll         <-  col_df$col[match(DAT$Scenario, col_df$scen)
  spp              <-  factor(unique(DAT$species),levels=levels(DAT$species))
  
  # Now plot it:
  if(!deltaIN)       p <- ggplot(DAT) + geom_line(aes(x = Year, y = value/ydiv,colour=scenario,size=HCR),alpha=alpha[1]/100)
  if(deltaIN)      p <- ggplot(DAT) + geom_line(aes(x = Year, y = delta_var_prcnt/ydiv,colour=scenario,size=HCR),alpha=alpha[1]/100)
  p      <- p + facet_grid(species~rcp2,scales=scalesIN)
  p      <- p + geom_vline(aes(xintercept=projLine),col="gray",size=1,linetype="dashed") 
  pchIN  <-  DAT%>%filter(HCR==hcr[1])%>%select(pch)
  
  for(nn in 1:((nprob-1)/2)){
    for(h in 1:nHCR){
      sub <- DAT%>%filter(HCR==hcr[h])
      
      if(!deltaIN)       eval(parse(text=paste0("p <-  p+ geom_ribbon(data=sub,aes(x = Year, ymin = ",probNames[nn],"/ydiv, ymax =",rev(probNames)[nn],"/ydiv,fill=scenario),alpha=alpha[2]/100)")))
      if(deltaIN)        eval(parse(text=paste0("p <-  p+ geom_ribbon(data=sub,aes(x = Year, ymin = ",probNamesDelta[nn],"/ydiv, ymax =",rev(probNamesDelta)[nn],"/ydiv,fill=scenario),alpha=alpha[2]/100)")))
    }
  }
  if(!deltaIN)       p <-  p + geom_line(aes(x = Year, y = prob50/ydiv,colour=scenario,size=HCR),alpha=alpha[3]/100)
  if(deltaIN)        p <-  p + geom_line(aes(x = Year, y = prob_delta50/ydiv,colour=scenario,size=HCR),alpha=alpha[3]/100)
  
  p <-  p + scale_color_manual(values = unlist(coll))
  p <-  p + scale_fill_manual(values= unlist(coll), name=paste0(prob[1]*100," through ",prob[3]*100, "quanitles"))
  p <-  p + scale_linetype_manual(values=ltyy) 
  
  if(showlinetype)
    p <-  p + scale_size_manual(values=lwdd, guide = "legend")
  if(!showlinetype)
    p <-  p + scale_size_manual(values=lwdd, guide = "none")
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
  p <- p + guides(color = guide_legend(order = 1))
  p <- p + guides(color=guide_legend(override.aes=list(fill=NA)))
  p <- p + guides(colour = guide_legend(override.aes = list(alpha = 1)))
  p <- p + theme(plot.subtitle=element_text(face="bold"))
  if(!is.null(xlimmIN[1])) 
    p <- p + xlim(xlimmIN[1],xlimmIN[2]) 
  
  if(!is.null(sublab)){
    tmpd <- data.frame(
      lab =letters[1:(nrcps*nspp)],
      x =rep(xlabIN,each=nrcps), 
      y = rep(ylabIN,each=nrcps),
      rcp2 = rep(rcps,nspp),
      species = rep(spp,each=nrcps))
    p <- p + geom_text(data=tmpd,aes( x = x, y = y*sublab_adj, label=lab),fontface=2, inherit.aes = F)
    
  }
  if(deltaIN) p <- p + scale_y_continuous(limits=c(-100,100))
  p
  
  
}
