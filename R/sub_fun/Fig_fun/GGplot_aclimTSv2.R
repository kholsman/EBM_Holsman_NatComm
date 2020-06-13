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