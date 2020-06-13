GG_HCRplot<-function(h=4,w=7,lgnpos="none",futScen="persistence",ylimm2=c(0,1.2),fontSize=.8,yfont=c(2070,2080)){
  dev.new(height=h,width=w)
  noF <-data.frame(F="Unfished",Year=B0_0_5_3[[1]]$Year,B=B0_0_5_3[[1]]$persistence)
  Fished   <-data.frame(F="Fished",Year=B_0_5_3[[1]]$Year,B=B_0_5_3[[1]][,futScen])
  
  noF$B40=noF$B*.40;noF$B40[noF$Year<2017]<-NA
  Fished$B40=noF$B*.40;Fished$B40<-NA
  noF$B35=noF$B*.35;noF$B35[noF$Year<2017]<-NA
  Fished$B35=noF$B*.35;Fished$B35<-NA
  
  Bdat<-as_tibble(rbind(noF,  Fished))
  Bdat$F<-factor(Bdat$F,levels=c("Unfished","Fished"))
  
  
  ylabb   = "Spawning biomass (million tons)"
  xlabb   = "Year" 
  plot_marginIN         = c(1,1, .5, .5)
  plot_title_marginIN = 0
  subtitle_marginIN   = 0
  caption_marginIN    = 0
  coll    = c(colors()[320],col2(6)[c(2,3,4)],col3(6)[c(3,4,6)])
  
  p <-     ggplot(data=Bdat, aes(x = Year, y = B),colour=Bdat$F)
  p <- p + geom_vline(data=Bdat, aes(xintercept=2017),col="gray",size=1,linetype="dashed") 
  p <- p + geom_hline(data=noF, aes(yintercept=tail(noF$B)[1]),col=coll[1],size=.5,linetype="dashed") 
  p <- p + geom_hline(data=noF, aes(yintercept=tail(noF$B)[1]*.32),col=coll[2],size=.5,linetype="dashed") 
  p <- p + geom_hline(data=noF, aes(yintercept=tail(noF$B)[1]*.40),col=coll[3],size=.5,linetype="dashed") 
  #p <- p + geom_hline(data=Fished, aes(yintercept=tail(Fished$B)[1]),col=coll[4],size=.5,linetype="dashed") 
  
  p <-  p+ geom_line(data=Bdat, aes(x = Year, y = B,colour=Bdat$F),size=1)
  # p <-  p+ geom_line(data=Bdat, aes(x = Year, y = B40,colour=Bdat$F),size=1)
  # p <-  p+ geom_line(data=Bdat, aes(x = Year, y = B35,colour=Bdat$F),size=1) 
  
  p <- p + scale_color_manual(values=c(coll[c(1,4)]))
  
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
  p <- p + ylim(0,2e07) 
  p <- p + xlim(1975,2120) 
  
  p
  
  p <-  p + annotate("text", x = 2115, y = tail(noF$B)[1]+1e6,  label = "paste(italic(B[0]))", parse = TRUE)
  # p <-  p + annotate("text", x = 2116, y = tail(Fished$B)[1]+1e6,  label = "paste(B[target])", parse = TRUE)
  p <-  p + annotate("text", x = 2116, y = tail(noF$B)[1]*.32-1e6,  label = "paste(italic(B[35]))", parse = TRUE)
  p <-  p + annotate("text", x = 2116, y = tail(noF$B)[1]*.40+1e6,  label = "paste(italic(B[40]))", parse = TRUE)
  
  p <-  p + annotate("text", x = yfont[1], y = tail(noF$B)[1]+2e6,     label = "paste('without climate; ',italic(F),' = 0')",  color=coll[1],parse = T,size=fontSize)
  p <-  p + annotate("text", x = yfont[2], y = tail(Fished$B)[1]+3e6,  label = "paste('with climate; ',italic(F),' = ' , italic(F[target]))", color=coll[4],parse = T,size=fontSize)
  
  # p <-  p + annotate("segment", x=2105, y=tail(noF$B)[1]+1e6,xend=2100,yend=tail(noF$B)[1]+.1e6, size=.5, arrow=arrow(length=unit(.2, “cm”)))
  p <-  p + geom_segment(aes(x=2110, y=tail(noF$B)[1]+1e6,xend=2105,yend=tail(noF$B)[1]+.1e6),arrow = arrow(length = unit(0.1,"cm"))) 
  p <-  p + geom_segment(aes(x=2110, y=tail(noF$B)[1]*.32-1e6,xend=2105,yend=tail(noF$B)[1]*.32-.1e6),arrow = arrow(length = unit(0.1,"cm"))) 
  p <-  p + geom_segment(aes(x=2110, y=tail(noF$B)[1]*.40+1e6,xend=2105,yend=tail(noF$B)[1]*.40+.1e6),arrow = arrow(length = unit(0.1,"cm"))) 
  
  #  p <-  p + geom_segment(aes(x=2110, y=tail(Fished$B)[1]+1e6,xend=2105,yend=tail(Fished$B)[1]),arrow = arrow(length = unit(0.1,"cm"))) 
  #p <- p + ggtitle("Biological reference points")
  
  # This example uses the ChickWeight dataset, which comes with ggplot2
  # First plot
  slopingHCR<-function(Ftarget=tail(Fished$B)[1]/tail(noF$B)[1],Bratio=.2,alpha=.05,Cbeta=.2)
  {
    maxFabc<-0*Bratio
    
    if(any(Bratio<1)){
      
      if(any(Bratio>=alpha)) 
        maxFabc[Bratio>=alpha]<-Ftarget*((Bratio[Bratio>=alpha]-alpha)/(1-alpha))
      if(any(Bratio<alpha))
        maxFabc[Bratio<alpha]<-0
      if(any(Bratio<=Cbeta))
        maxFabc[Bratio<=Cbeta]<-0
    }
    if(any(Bratio>=1))
      maxFabc[Bratio>1]<-Ftarget
    
    return(maxFabc)
  }
  Ftarget<-tail(Fished$B)[1]/tail(noF$B)[1]
  
  Bratio<-seq(0,2,.001)
  dat2<-data.frame(
    Bratio=Bratio*tail(Fished$B)[1],
    Feffective=slopingHCR(Ftarget=Ftarget,Bratio=Bratio,alpha=.05,Cbeta=.05),
    FeffectiveEBM=slopingHCR(Ftarget=Ftarget,Bratio=Bratio,alpha=.05,Cbeta=.2))
  
  dat2<-data.frame(
    Bratio=Bratio,
    Feffective=slopingHCR(Ftarget=Ftarget,Bratio=Bratio,alpha=.05,Cbeta=.05)/Ftarget,
    FeffectiveEBM=slopingHCR(Ftarget=Ftarget,Bratio=Bratio,alpha=.05,Cbeta=.2)/Ftarget)
  
  
  # Second plot
  p2 <- ggplot(dat2, aes(x=Bratio, y=Feffective)) 
  
  
  p2 <- p2 + geom_hline(data=dat2, aes(yintercept=1),col=coll[1],size=.5,linetype="dashed") 
  p2 <- p2 + geom_vline(data=Fished, aes(xintercept=1),col=coll[1],size=.5,linetype="dashed") 
  p2 <- p2 + geom_vline(data=Fished, aes(xintercept=.2),col=coll[1],size=.5,linetype="dashed") 
  
  p2 <-  p2+ geom_line(data=dat2, aes(x = Bratio, y=Feffective),col=coll[2],size=1,linetype="dashed") 
  p2 <-  p2+ geom_line(data=dat2, aes(x = Bratio, y=FeffectiveEBM),col=coll[4],size=1)
  
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
    labs(x=expression(italic(B[y])/italic(B[40])), y=expression(~italic(F[y])/italic(F[target])),subtitle="B) Sloping harvest control rule") +
    theme(legend.title=element_blank(),
          legend.background = element_rect(colour = NA),
          legend.key = element_rect(colour = "white", fill = NA)) 
  p2 <- p2 + guides(fill = FALSE)
  p2 <- p2 + guides(color = guide_legend(order = 1))
  p2 <- p2 + guides(color=guide_legend(override.aes=list(fill=NA)))
  p2 <- p2 + ylim(ylimm2[1],ylimm2[2]) 
  
  #p2<- p2 + xlim(0,1.2) 
  p2
  
  
  multiplot(p, p2,cols=2) 
  
}