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