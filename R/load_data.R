# ----------------------------------------
# load_data.R
# subset of Holsman et al. 2020 Nature Comm.
# kirstin.holsman@noaa.gov
# updated 2020
# ----------------------------------------
cat("loading data, may take a few mins...")
# load data for paper analyses:

# load Kir's ggplot theme:
source("R/sub_scripts/THEMES_GGPLOT.r")
theme_kir_EBM <- function(...) {
  theme_kir(base_family="Helvetica",
            plot_title_family="Helvetica-Bold",
            subtitle_family="Helvetica",
            panel_face="bold",
            caption_family="Helvetica",
            plot_title_just ="r",
            axis_title_size = 12,
            ...)
}
theme_kir_EBM <- function(...) {
  theme_kir(base_family="ArialNarrow",
            plot_title_family="ArialNarrow-Bold",
            subtitle_family="ArialNarrow",
            panel_face="bold",
            caption_family="ArialNarrow",
            plot_title_just ="r",
            axis_title_size = 12,
            ...)
}


#_______________________________________
# Read in simulation data and list of sims:
#_______________________________________
# multispp results:
load("data/sim/sim_msm.Rdata")
load("data/sim/mclist2.Rdata")
load("data/sim/simlist2.Rdata")
load("data/sim/empty2.Rdata")

# Single spp:
# load("data/sim/sim_ssm.Rdata")
# load("data/sim/mclist0.Rdata")
# load("data/sim/simlist0.Rdata")
# load("data/sim/empty0.Rdata")

cat("Load Data Complete")
if(1==10){
  if(readdat==FALSE){
    if(!any(dir("data/out")%in%"EBM_ceattlenew.Rdata"))
      stop("EBM_ceattlenew.Rdata file not found, please go to 
           https://figshare.com/s/6dea7722df39e07d79f0 
           and download file into EBM_Holsman_NatComm/EBM_ceattlenew.Rdata")
    #download.file("https://figshare.com/s/6dea7722df39e07d79f0",destfile="EBM_ceattlenew.Rdata")
    load("data/out/EBM_ceattlenew.Rdata")    
  }else{
    
    # download and unzip the latest ceattle runs:
    if(!length(dir(paste0("data/runs/",fldr,"_0")))>0){
      #download.file("https://figshare.com/s/d9c35dbe0880f4169041",paste0("data/runs/",fldr,"_0.zip"))
      #system (paste0("cd data/runs; unzip ",fldr,"_0.zip"))
      stop(paste0("data/runs/",fldr,"_0 file not found, please go to https://figshare.com/s/d9c35dbe0880f4169041 and download file into data/runs/",fldr,"_0.zip and unzip)") ) 
      
    }
    if(!length(dir(paste0("data/runs/",fldr,"_2")))>0){
      #download.file("https://figshare.com/s/3a1aaa86837b79d6aa07",paste0("data/runs/",fldr,"_2.zip"))
      #system (paste0("cd data/runs; unzip ",fldr,"_2.zip"))
      stop(paste0("data/runs/",fldr,"_2 file not found, please go to https://figshare.com/s/3a1aaa86837b79d6aa07 and download file into data/runs/",fldr,"_2.zip and unzip)") ) 
      
    }
    
    source("SUB_EBM_paper.R")
    save.image(file = "data/out/EBM_ceattlenew.Rdata")
  } 
}




