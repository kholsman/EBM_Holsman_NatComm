#'________________________________________________
#'TPlot F and effective F
#'________________________________________________   
#'print(paste0("plotting (3)",spnm))
#'     ggsave(filename=fnm,device="png",dpi=500,width=W,height=H)
#'     library(ggpubr)
#'     Grouped Scatter plot with marginal density plots
#'     ggscatterhist(
#'      iris, x = "Sepal.Length", y = "Sepal.Width",
#'      color = "Species", size = 3, alpha = 0.6,
#'      palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#'      margin.params = list(fill = "Species", color = "black", size = 0.2)
#'      )
#'      
plot_Feffective<-function(
  sppINFOIN=sppINFO,
  FDATA= list(
    "no cap"= F_219_CENaivecf_2_5_12_mc,
    "2 MT cap"=F_2MT_219_CENaivecf1_2_5_13_mc),
  F40DATA= list(
    "no cap"= F40_219_CENaivecf_2_5_12_mc,
    "2 MT cap"=F40_2MT_219_CENaivecf1_2_5_13_mc),
  BDATA= list(
    "no cap"= B_219_CENaivecf_2_5_12_mc,
    "2 MT cap"=B_2MT_219_CENaivecf1_2_5_13_mc),
  Btarget= list(
    "no cap"= target_B_2["no cap",],
    "2 MT cap"=target_B_2["2MT cap",]),
  plotSet = list("persistence" = 1,"RCP 4.5" = (rcp45_n_sim),"RCP 8.5" = (rcp85NoBio_n_sim)),
  collset=c("no cap"= "YlOrRd" ,"2 MT cap"="YlGnBu"),
  ntemps=5,
  plot_marginIN= c(5,5,5,5),
  lgnpos="none",
  subyr=fut_yrs,
  trndfill="white",
  trndln="white",
  trndln2="white",
  tipping =Ornjazz[3],
  shp=16,
  ptsize=2,
  sizeIN=c(0.5,.75,1.2,2),
  ylimmIN=c(-2,2),
  xlimmIN=c(-2,8),
  nspp=3,
  alphacore=1,
  altTheme=T){
  
  colorscale<-scale_fill_gradientn(
    colors = rev(RColorBrewer::brewer.pal(9, collset[2])),
    values = c(0, exp(seq(-5, 0, length.out = 100))))
  
  ndat<-length(F40DATA)
  coll<-rep(0,ndat)
  for(d in 1:ndat)
    coll[d]<-rev(RColorBrewer::brewer.pal(9, collset[d]))[3]
  
  
  colorscale2<-scale_colour_manual(
    values = coll,
    labels = (names(F40DATA))
  )
  
  
  nyr<-length(subyr)
  nset<-length(plotSet)
  for(d in 1:ndat)
    for (s in 1:nspp){
      for(p in 1:nset){
        
        yr<-as.numeric(colnames(FDATA[[d]][s,,subyr, plotSet[[p]]]))
        yr<-matrix(yr,dim(FDATA[[d]][s,,subyr, plotSet[[p]]])[1],dim(FDATA[[d]][s,,subyr, plotSet[[p]]])[2],byrow=T)
        tmpD<-data.frame(
          type =names(F40DATA)[d],
          rcp = names(plotSet)[p],
          col= collset[[d]],
          year = as.vector(yr),
          F= as.vector(F40DATA[[d]][s,,subyr, plotSet[[p]]]),
          B= as.vector(BDATA[[d]][s,,subyr, plotSet[[p]]]),
          F2Ftarget= as.vector(FDATA[[d]][s,,subyr, plotSet[[p]]]/F40DATA[[d]][s,,subyr, plotSet[[p]]]),
          B2Btarget= as.vector(BDATA[[d]][s,,subyr, plotSet[[p]]]/Btarget[[d]][s]),
          Species = sppINFO[[s]]$plotSPP)
        
        GAM<-gam(log(F2Ftarget+.001)~s(log(B2Btarget+.001),k=8,bs="tp"),data=tmpD[tmpD$B2Btarget>0.4,])
        # LM<-lm(log(F2Ftarget+.001)~log(B2Btarget+.001),data=tmpD[tmpD$B2Btarget>0.4,])
        hat<-predict.gam(GAM,se.fit=T)
        # hat.lm<-predict(LM,se.fit=T)
        
        # plot((F2Ftarget)~(B2Btarget),data=tmpD,ylim=c(0,1.1))
        # points(tmpD[tmpD$B2Btarget>0.4,]$B2Btarget,exp(hat.lm$fit-.001),col="red",pch=".")
        tmpD$F2Ftarget_hat<-tmpD$F2Ftarget_hatUP<-tmpD$F2Ftarget_hatDWN<-tmpD$F2Ftarget*NA
        tmpD[tmpD$B2Btarget>0.4,]$F2Ftarget_hat<-exp(hat$fit-.001)
        tmpD[tmpD$B2Btarget>0.4,]$F2Ftarget_hatUP<-exp(hat$fit+10*1.95*hat$se-.001)
        tmpD[tmpD$B2Btarget>0.4,]$F2Ftarget_hatDWN<-exp(hat$fit-10*1.95*hat$se-.001)
        # plot(tmpD$B2Btarget,tmpD$F2Ftarget)
        #  points(tmpD$B2Btarget,tmpD$F2Ftarget_hat,col="red")
        #  points(tmpD$B2Btarget,tmpD$F2Ftarget_hatUP,col="red")
        #  points(tmpD$B2Btarget,tmpD$F2Ftarget_hatDWN,col="red")
        
        if(s==1&d==1&p==1){
          plotALL<-tmpD
        }else{
          plotALL<-rbind(plotALL,tmpD)
        }
        
        #quantile()
        #probs
      }
    }
  binW<-c(0.16, 0.02)
  
  
  p <-     ggplot(data=plotALL, aes(x = B2Btarget, y = F2Ftarget))
  #p <- p + geom_hex(alpha=0.6, aes(fill=..density..))
  p <- p + geom_hline(yintercept=0,colour="gray",size=1)        
  p <- p + geom_point(data=plotALL,aes(x = B2Btarget, y = F2Ftarget,colour=type),size= 1.5,alpha = 0.02)  #+ colorscale2
  p <- p + geom_hex(bins = 100) + colorscale
  p <- p + facet_grid(~ Species,scales = "free_y") 
  p<- p + xlim(-.1,4) + ylim(-0.1,1.2)
  # p <- p + geom_ribbon(data=plotALL,aes(ymin=F2Ftarget_hatDWN, ymax=F2Ftarget_hatUP,group=Species),fill=trndfill,col=trndln, linetype=1,size=sizeIN[1], alpha=alpha1)  
  #p <- p + geom_point(data=plotALL,aes(ymin=F2Ftarget_hatDWN, ymax=F2Ftarget_hatUP,group=type),alpha=.4,colour = trndln,size=.2) 
  # p <- p + geom_hex(binwidth = binW) + scale_fill_viridis() 
  # p <- p + geom_hex(aes(fill = stat(cut((count/max(count)),breaks = c(-Inf,0,.25,.5,.75,Inf),labels = F, right = T, include.lowest = T))),
  #  binwidth = c(0.16, 0.02)) + colorscale
  # # p <- p + scale_colour_gradientn(name = count) + colorscale
  # p <- p + geom_hex(aes(fill = stat(cut(log(count), breaks = log(c(0, 100, 200, 400, Inf)), labels = F, right = T, include.lowest = T))),binwidth = c(0.16, 0.02))
  
  p<- p + theme_light() +
    labs(x=NULL, y=NULL,
         title="",
         subtitle="",
         caption="") +
    theme(plot.subtitle=element_text(margin=margin(b=20))) +
    theme(legend.title=element_blank()) +
    theme(legend.position=lgnpos) +
    theme(legend.key.width = unit(.5, "cm")) +
    theme(legend.text=element_text(size=5)) +
    theme(legend.key.size=unit(.01, "cm")) +
    labs(x=expression(B[y]/B[0]), y=expression(~F[y]/F[target])) +
    #labs(tag=letters(1:6)) +
    theme(plot.margin=margin(t = 3, r = 3, b = 3, l =3)) 
  
  
  
  
  if(altTheme) p<- p+ theme_kir_EBM(sub_title_size=12,
                                    sub_title_just="l",
                                    plot_margin = margin(plot_marginIN),
                                    plot_title_margin = 1,
                                    subtitle_margin = 0,
                                    caption_margin = 0,
                                    axis_title_just = "cm") 
  
  p
  
}
