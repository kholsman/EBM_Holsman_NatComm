## ------------------------------------------------
## K. Holsman 
## June 2020
## Kirstin.holsman@noaa.gov
##
## make_plots.R
## This code creates the figures in Holsman et al. Nat Comm
## 
## ------------------------------------------------

#source("R/make.R")       # loads packages, data, setup, etc.

#-------------------------------------
# 3. Final figures:
#-------------------------------------
#fig 2: temperature
graphics.off()


if(!update.figs){
  cat("\n printing figures via make_plots.R (not overwriting figs in Figures folder)....")
  fig2()
  fig3()
  fig4()
  fig5()
  fig6()
  figS1()
  figS2()
  figS3()
  figS4()
  figS5()
  figS6()
}else{
  cat("\n updating figures via make_plots.R....")
  
  fig2()
  ggsave(file=paste0("Figures/Fig2.tiff"), device = "tiff",
                         scale = scaleIN, width = NA, height = NA, units = "in",
                         dpi = dpiIN)  
  fig3()
  ggsave(file=paste0("Figures/Fig3.tiff"), device = "tiff", 
                         scale = scaleIN, width = NA, height = NA, units = "in",
                         dpi = dpiIN)

  fig4()
  ggsave(file=paste0("Figures/Fig4.tiff"), device = "tiff", 
                         scale = scaleIN, width = NA, height = NA, units = "in",
                         dpi = dpiIN)
  
  fig5()
  ggsave(file=paste0("Figures/Fig5.tiff"), device = "tiff", 
                         scale = scaleIN, width = NA, height = NA, units = "in",
                         dpi = dpiIN)
  fig6()
  
  ggsave(file=paste0("Figures/Fig6.tiff"), device = "tiff", 
                         scale = scaleIN, width = NA, height = NA, units = "in",
                         dpi = dpiIN)
  
  figS2()
  ggsave(file=paste0("Figures/FigS2.tiff"), device = "tiff", 
                         scale = scaleIN, width = NA, height = NA, units = "in",
                         dpi = dpiIN)
  figS3()
  ggsave(file=paste0("Figures/FigS3.tiff"), device = "tiff", 
                         scale = scaleIN, width = NA, height = NA, units = "in",
                         dpi = dpiIN)
  
  figS4()
  ggsave(file=paste0("Figures/FigS4.tiff"), device = "tiff", 
                         scale = scaleIN, width = NA, height = NA, units = "in",
                         dpi = dpiIN);  dev.off()
  figS5()
  ggsave(file=paste0("Figures/FigS5.tiff"), device = "tiff", 
                         scale = scaleIN, width = NA, height = NA, units = "in",
                         dpi = dpiIN)
  dev.off()
  
  figS6()
  quartz.save(file=paste0("Figures/FigS6.jpg"), type = "jpeg", dpi = dpiIN)
  dev.off()
  
  figS7()
  ggsave(file=paste0("Figures/FigS7.tiff"), device = "tiff",
         scale = scaleIN, width = NA, height = NA, units = "in",
         dpi = dpiIN) 
  
  tiff(filename = paste0("Figures/FigS1.tiff"), 
         units = "in",height=3.75,width=8,res=dpiIN)
  figS1()
  dev.off()
  


}