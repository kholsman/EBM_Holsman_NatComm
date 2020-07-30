

GG_HCRplot<-function(
  datIN0 = dat_2_1_3,
  datIN1 = dat_2_5_12,
  sp     = 1, 
  BtargetIN = 2028356.0,
  B0IN      = 3871278.0,
  Bratio  = 0.52395,
  ylimm   = c(0,7e6),
  h       =  4,
  w       =  7,
  pts     = c(2026,2070),
  lgnpos  = "none",
  futScen = "persistence",
  Rin0    = "1",
  Rin     = as.character(rset),
  ylimm2  = c(0,1.2),fontSize=.8,
  Hin     = c("H3","H12_219_CENaivecf"), #H3_setB0; #H3_219_CENaivecf
  yfont   = c(2050,2060)){
  
  #dev.new(height=h,width=w)
  prob <- (.5)
  probNames        <- paste0('prob',prob*100)
  p_funs           <- purrr::map(prob, ~purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    purrr::set_names(nm = probNames)
  datIN0$F_eff <- datIN0$F
  datIN1$F_eff <- datIN1$F
  
  noF  <- datIN0%>%
    filter(recMode==Rin0,hMode=="3",hModev2==Hin[1],Scenario==1,species==sp)%>%
    mutate(Year = start_yr + future_year-1,F="Unfished")%>%
    group_by(Year,F_eff,recMode,hMode,Scenario,species,F,SSB0_total_biom)%>%
    summarize_at(vars(SSB0),sum)
  noF$B     <- noF$SSB0
  
  Fished  <- datIN0%>%
    filter(recMode==Rin0,hMode=="3", hModev2==Hin[1],
           Scenario==which(Scenarios=="persistence"),species==sp)%>%
    mutate(Year = start_yr + future_year-1,F="Fished")%>%
    group_by(Year,F)%>%
    group_by(Year,F_eff,recMode,hMode,Scenario,species,F,SSB_total_biom)%>%
    summarize_at(vars(SSB),sum)
  Fished$B <- Fished$SSB
    
  CE_proj  <- datIN1%>%
    filter(recMode==Rin,hModev2==Hin[2],
           Scenario==which(Scenarios==futScen),species==sp)%>%
    mutate(Year = start_yr + future_year-1,F="Fished")%>%
    group_by(Year,F)%>%
    group_by(Year,Fabc,F_eff,recMode,hMode,Scenario,species,F,SSB_total_biom)%>%
    summarize_at(vars(SSB),sum)
  CE_proj$B  <- CE_proj$SSB
  CE_proj$F  <- factor(CE_proj$F,levels=c("Unfished","Fished"))
  
  noF$B40     <- noF$B*.40 ; noF$B40[noF$Year<2017] <- NA
  Fished$B40  <- noF$B*.40 ; Fished$B40  <- NA
  
  noF$B35     <- noF$B*.35 ; noF$B35[noF$Year<2017] <- NA
  Fished$B35  <- noF$B*.35 ; Fished$B35  <- NA
  
  noF$BtargetIN <- BtargetIN ; noF$BtargetIN[noF$Year<2017] <- NA
  noF$B0IN      <- B0IN      ; noF$B0IN[noF$Year<2017] <- NA
  
  Bdat      <- as_tibble(rbind(noF,  Fished))
  Bdat      <- as_tibble(noF)
  Bdat$F    <- factor(Bdat$F,levels=c("Unfished","Fished"))
  point_dat <- CE_proj%>%filter(Year%in%pts)
  point_dat2 <- CE_proj%>%filter(Year%in%(pts+1))
  point_dat2F <- Fished%>%filter(Year%in%(pts+1))
  
  
  ylabb   <- "Spawning biomass (million tons)"
  xlabb   <- "Year" 
  plot_marginIN <- c(1,1, .5, .5)
  plot_title_marginIN <-
    subtitle_marginIN   <-
    caption_marginIN    <- 0
  mx2     <- max(Bdat$Year)+c(-9,0)
  coll    <- c(colors()[320],col2(6)[c(2,3,4)],col3(6)[c(3,4,6)])

  
  p <-     ggplot(data=Bdat, aes(x = Year, y = B),colour=Bdat$F)
  p <- p + geom_rect(aes(xmin=mx2[1], xmax=mx2[2], ymin=0, ymax=Inf),fill=colors()[350],alpha=.2)
  p <- p + geom_vline(data=Bdat, aes(xintercept=2017),col="gray",size=1,linetype="dashed") 
  p <- p + geom_hline(data=noF, aes(yintercept=mean(rev(noF$B)[1:5])),col=coll[1],size=.5,linetype="dashed") 
  p <- p + geom_hline(data=noF, aes(yintercept=mean(rev(noF$B)[1:5])*.40),col=coll[3],size=.5,linetype="dashed") 
  #p <- p + geom_hline(data=noF, aes(yintercept=mean(rev(CE_proj$B)[1:5])),col="red",size=.5,linetype="dashed") 
  #p <- p + geom_hline(data=noF, aes(yintercept=mean(rev(noF$BtargetIN)[1:5])),col=coll[4],size=.5,linetype="dashed") 
 # p <- p + geom_hline(data=noF, aes(yintercept=mean(rev(noF$B0IN)[1:5])),col="red",size=.5,linetype="dashed") 
  
  #p <-  p+ geom_line(data=noF, aes(x = Year, y = B40),col=coll[3],size=.5,linetype="solid") 
  p <-  p+ geom_line(data=Bdat, aes(x = Year, y = B,colour=Bdat$F),size=1)
  p <-  p+ geom_line(data=CE_proj, aes(x = Year, y = B),col=coll[4],size=1)
  p <-  p + geom_point(data=point_dat, aes(x = Year, y = B),col=col_in2(length(pts)),size=3,shape=c(16,17),alpha=.8)
  p <-  p + scale_color_manual(values=c(coll[c(1,4)]))
  
  p <- p + theme_light() +
    labs(x=NULL, y=NULL) +
    theme(plot.subtitle=element_text(margin=margin(b=20))) +
    theme(legend.title=element_blank()) +
    theme(legend.key.width = unit(.5, "cm")) +
    theme(legend.text=element_text(size=5)) +
    theme(legend.key.size=unit(.01, "cm")) +
    labs(x= xlabb,y=ylabb)+
    #labs(tag=letters(1:6)) +
    theme(plot.margin=margin(t = 10, r = 10, b = 10, l =10)) 
  
  p<- p+ theme_kir_EBM(sub_title_size=12,
                       sub_title_just="l",
                       
                       axis_title_just = "cm") +
    labs(x=xlabb, y= ylabb,subtitle="A) Biological reference points") +
    theme(legend.title=element_blank(),
          legend.background = element_rect(colour = NA),
          legend.key = element_rect(colour = "white", fill = NA)) 
  p <- p + theme(legend.position=lgnpos)
  p <- p + guides(fill = FALSE)
  p <- p + guides(color = guide_legend(order = 1))
  p <- p + guides(color=guide_legend(override.aes=list(fill=NA)))
  p <- p + ylim(ylimm[1],ylimm[2]) 
  p <- p + xlim(1975,2120) 
  p <-  p + annotate("text", x = 2115, y = tail(noF$B)[1]+1e6,  label = "paste(italic(B[0]))", parse = TRUE)
  p <-  p + annotate("text", x = 2116, y = tail(noF$B)[1]*.40-1e6,  label = "paste(italic(B[40]))", parse = TRUE)
  #p <-  p + annotate("text", x = 2116, y = tail(noF$BtargetIN)[1]+1e6,  label = "paste(italic(B[target]))", parse = TRUE)
  
  p <-  p + annotate("text", x = yfont[1], y = tail(noF$B)[1]+.4e6,  label = "paste('without climate; ',italic(F[y]),' = 0')",  color=coll[1],parse = T,size=fontSize)
  p <-  p + annotate("text", x = yfont[2], y = tail(noF$B)[1]-.6e6,  label = "paste('with climate; ',italic(F[y]),' = ' , italic(F[ABC[y]]))", color=coll[4],parse = T,size=fontSize)
  p <-  p + geom_segment(aes(x=2110, y=tail(noF$B)[1]+1e6-.3e6,xend=2105,yend=tail(noF$B)[1]+.05e6),arrow = arrow(length = unit(0.1,"cm"))) 
  p <-  p + geom_segment(aes(x=2110, y=tail(noF$B)[1]*.40-1e6+.3e6,xend=2105,yend=tail(noF$B)[1]*.40-.05e6),arrow = arrow(length = unit(0.1,"cm"))) 
  #p <-  p + geom_segment(aes(x=2110, y=tail(noF$BtargetIN)[1]+1e6-.3e6,xend=2105,yend=tail(noF$BtargetIN)[1]+.05e6),arrow = arrow(length = unit(0.1,"cm"))) 
  
  # First plot
  
  Ftarget  <- tail(Fished$B)[1]/tail(noF$B)[1]
  Bratio   <- seq(0,2,.001)
  dat2     <- data.frame(
    Bratio         = Bratio*tail(Fished$B)[1],
    Feffective     = slopingHCR(Ftarget=Ftarget,Bratio=Bratio,alpha=.05,Cbeta=.05),
    FeffectiveEBM  = slopingHCR(Ftarget=Ftarget,Bratio=Bratio,alpha=.05,Cbeta=.2))
  
  dat2     <- data.frame(
    Bratio         = Bratio,
    Feffective     = slopingHCR(Ftarget=Ftarget,Bratio=Bratio,alpha=.05,Cbeta=.05)/Ftarget,
    FeffectiveEBM  = slopingHCR(Ftarget=Ftarget,Bratio=Bratio,alpha=.05,Cbeta=.2)/Ftarget)
  
  ptdat <- data.frame(
    Bratio         = point_dat$B/(.4*B0IN),
    Feffective     = point_dat2F$F_eff/point_dat2$F_eff,
    Feffective2    = Ftarget/point_dat2$F_eff)
  
  ptdat <- data.frame(
    Bratio         = point_dat$B/(.4*B0IN),
    Feffective     = point_dat2F$F_eff/point_dat2$F_eff,
    Feffective2    = Ftarget/point_dat2$F_eff)
  for(i in 1:length(pts)){
    ptdat$Fratio[i] <-  dat2$FeffectiveEBM[ which(dat2$Bratio>ptdat$Bratio[i])[1]]
  }

 
  # Second plot
  p2 <- ggplot(dat2, aes(x=Bratio, y=Feffective)) 
  p2 <- p2 + geom_hline(data=dat2, aes(yintercept=1),col=coll[1],size=.5,linetype="dashed") 
  p2 <- p2 + geom_vline(data=Fished, aes(xintercept=1),col=coll[1],size=.5,linetype="dashed") 
  p2 <- p2 + geom_vline(data=Fished, aes(xintercept=.2),col=coll[1],size=.5,linetype="dashed") 
  
  p2 <-  p2+ geom_line(data=dat2, aes(x = Bratio, y=Feffective),col=coll[2],size=1,linetype="dashed") 
  p2 <-  p2+ geom_line(data=dat2, aes(x = Bratio, y=FeffectiveEBM),col=coll[4],size=1)
  p2 <-  p2+ geom_point(data=ptdat, aes(x = Bratio, y = Fratio),col=col_in2(length(pts)),size=3,shape=c(16,17),alpha=.8)
  
  p2 <- p2 + scale_color_manual(values=c(coll[c(1,4)]))
  p2 <- p2 + theme_light() +
    labs(x=NULL, y=NULL) +
    theme(legend.position=lgnpos)+
    theme(plot.subtitle=element_text(margin=margin(b=20))) +
    theme(legend.title=element_blank()) +
    theme(legend.key.width = unit(.5, "cm")) +
    theme(legend.text=element_text(size=5)) +
    theme(legend.key.size=unit(.01, "cm")) +
    #labs(tag=letters(1:6)) +
    theme(plot.margin=margin(t = 10, r = 10, b = 10, l =10)) 
  
  p2<- p2+ theme_kir_EBM(sub_title_size=12,
                         sub_title_just="l",
                         axis_title_just = "cm") +
    labs(x=expression(italic(B[y])/italic(B[40])), 
         y=expression(~italic(F[ABC[y]])/italic(F[target])),subtitle="B) Sloping harvest control rule") +
    theme(legend.title=element_blank(),
          legend.background = element_rect(colour = NA),
          legend.key = element_rect(colour = "white", fill = NA)) 
  p2 <- p2 + guides(fill = FALSE)
  p2 <- p2 + guides(color = guide_legend(order = 1))
  p2 <- p2 + guides(color=guide_legend(override.aes=list(fill=NA)))
  p2 <- p2 + ylim(ylimm2[1],ylimm2[2]) 
  
  p2
  
  multiplot(p, p2,cols=2) 
  
}
