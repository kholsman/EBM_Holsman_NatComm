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
  sppINFOIN = sppINFO,
  nspp    = 3,
  DataIN  = list("no cap"= dat_2_5_12_mc,"2 MT cap"=dat_2_5_13_mc),
  hLIST   = list("H12_219_CENaivecf","H13_2MT_219_CENaivecf1"),
  plotSet = list("persistence" = 1,"RCP 4.5" = (rcp45_n),"RCP 8.5" = (rcp85NoBio_n)),
  collset = c("no cap"= "YlOrRd" ,"2 MT cap"="YlGnBu"),
  ntemps  = 5,
  Btarget = target_B_2,
  ageIN   = 6, 
  plot_marginIN = c(5,5,5,5),
  subyr   = fut_yrs,
  trndfill= "white",
  trndln  = "white",
  trndln2 = "white",
  tipping = Ornjazz[3],
  shp     = 16,
  ptsize  = 2,
  sizeIN  = c(0.5,.75,1.2,2),
  ylimmIN = c(-2,2),
  xlimmIN = c(-2,8),
  alphacore = 1,
  lgnpos  = "none",
  altTheme = T){
  
  rcp_col_df       <- data.frame(Scen_n   = unlist(plotSet),
                                 Scenario = factor(Scenarios[unlist(plotSet)],levels=Scenarios),
                                 rcp     = rep.int(names(plotSet),times=as.numeric(lengths(plotSet))))
  DATA    <- list("no cap"="","2 MT cap"="")
  if(any(!(rownames(Btarget)%in%names(DataIN)))) stop("row names of Btarget do not match DataIN names")
  for(ll in 1:length(DataIN)){
    DATA[[ll]] <-  DataIN[[ll]]                          %>% 
      filter(age==ageIN,hModev2==hLIST[[ll]], MC_n >0)   %>%
      mutate(sp        = species,
             Year      = start_yr + future_year-1,
             scenario  = rcp_col_df$Scenario[ match(Scenario,rcp_col_df$Scen_n)],
             rcp       = rcp_col_df$rcp[ match(Scenario,rcp_col_df$Scen_n)],
             HCR       = factor(names(DataIN)[ll],levels=names(DataIN)),
             type      = factor(names(DataIN)[ll],levels = names(DataIN)),
             F2Ftarget = F/F40,
             B2Btarget = SSB_total_biom/SSB0_total_biom)%>%
      filter(Year >= fut_start)
  }
  Years  <- sort(unique(DataIN[[1]]$future_year)) +start_yr-1
  subyr  <- Years[Years >= fut_start]
  nyr    <- length(subyr)
  nset   <- length(plotSet)
  
  
  colorscale<-scale_fill_gradientn(
    colors = rev(RColorBrewer::brewer.pal(9, collset[2])),
    values = c(0, exp(seq(-5, 0, length.out = 100))))
  
  ndat<-length(DataIN)
  coll<-rep(0,ndat)
  for(d in 1:ndat)
    coll[d]<-rev(RColorBrewer::brewer.pal(9, collset[d]))[3]
  
  
  colorscale2<-scale_colour_manual(
    values = coll,
    labels = (names(DataIN))
  )
  #rm(DataIN)
  
  spnames       <- rep("",nspp)
  for (s in 1:nspp)
    spnames[s]  <- sppINFO[[s]]$plotSPP
  
  
  for(d in 1:ndat)
    for (s in 1:nspp){
      for(p in 1:nset){
       
        tmpD <- DATA[[d]]%>%filter(sp == s,Scenario%in%plotSet[[p]])%>%
          mutate(col     = collset[[d]],
                 Species = factor(spnames[s],levels = spnames) ,
                 targetB = Btarget[rownames(Btarget)==names(DataIN)[d],s],
                 B2Btarget = SSB_total_biom/Btarget[rownames(Btarget)==names(DataIN)[d],s])
        
        GAM  <- gam(log(F2Ftarget+.001)~s(log(B2Btarget+.001),k=8,bs="tp"),data=tmpD)
        hat  <- predict.gam(GAM,se.fit=T)
        tmpD <- tmpD%>%mutate(
          F2Ftarget_hat    = exp(hat$fit-.001),
          F2Ftarget_hatUP  = exp(hat$fit+10*1.95*hat$se-.001),
          F2Ftarget_hatDWN = exp(hat$fit-10*1.95*hat$se-.001))
       
        if(s==1&d==1&p==1){
          plotALL<-tmpD
        }else{
          plotALL<-rbind(plotALL,tmpD)
        }
        
        #quantile()
        #probs
      }
    }
  
  binW <- c(0.16, 0.02)
  
  p <-     ggplot(data=plotALL, aes(x = B2Btarget, y = F2Ftarget))
  p <- p + geom_hline(yintercept=0,colour="gray",size=1)        
  p <- p + geom_point(data=plotALL,aes(x = B2Btarget, y = F2Ftarget,colour=type),size= 1.5,alpha = 0.05, show.legend = T)  #+ colorscale2
  p <- p + geom_hex(bins = 100,show.legend = FALSE) + colorscale
  p <- p + facet_grid(~ Species,scales = "free_y") 
  p <- p + xlim(-.1,4) + ylim(-0.1,1.2) #+ theme(legend.position = "none")
  p <- p + guides(colour = guide_legend(override.aes = list(alpha = 1,size=2)))
  
  p <- p + theme_light() +
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
