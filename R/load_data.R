# ----------------------------------------
# load_data.R
# subset of Holsman et al. 2020 Nature Comm.
# kirstin.holsman@noaa.gov
# updated 2020
# ----------------------------------------
  cat("loading data, this may take a few mins...")
  
  # load ggplot theme:
  source("R/sub_scripts/THEMES_GGPLOT.r")

  #_______________________________________
  # Read in simulation data and list of sims:
  #_______________________________________
  
  # multispp results:
  load(file.path(in_dir,"sim_msm.Rdata"))
  load(file.path(in_dir,"target_B_2.Rdata"))
  load(file.path(in_dir,"mclist2.Rdata"))
  load(file.path(in_dir,"simlist2.Rdata"))
  load(file.path(in_dir,"empty2.Rdata"))
  run_def <- readxl::read_xlsx(file.path(in_dir,"Run_Defintions.xlsx"),sheet="Sheet1")

  cat("simulation data loaded ('data/in')...")
  #_______________________________________
  # Load simulations, risk calcs, and threshold results (or run these if not in folder):
  #_______________________________________

  
    for(fn in outfn){
      if(!any(dir(out_dir)%in%fn))
        stop(paste0(fn," file not found in: \t \t",out_dir,
                    "\n\nplease go to: https://figshare.com/s/6dea7722df39e07d79f0","",
                    "\n\nand download file into: \t \t",out_dir,"/",fn))
      load(file.path(out_dir,fn))
    }
  cat("analysis data loaded ('data/out')...")
  #_______________________________________
  # Load ROMSNPZ covariates:
  #_______________________________________
    load(file.path(in_dir,"covariates.Rdata"))
    for(fn in "covariates.Rdata"){
      if(!any(dir(in_dir)%in%fn))
        stop(paste0(fn," file not found in: \t \t",in_dir,
                    "\n\nplease go to: https://figshare.com/s/6dea7722df39e07d79f0","",
                    "\n\nand download file into: \t \t",in_dir,"/",fn))
      load(file.path(in_dir,fn))
    }
    
    Scenarios     <-  unique(covariates$Scenario)
    A1B_n         <-  grep("A1B",Scenarios)
    bio_n         <-  grep("bio",Scenarios)
    rcp45_n       <-  grep("rcp45",Scenarios)
    rcp85_n       <-  grep("rcp85",Scenarios)
    rcp85NoBio_n  <-  setdiff(rcp85_n,bio_n)
    plotList      <-  Scenario_set  <- c(1,rcp45_n,rcp85NoBio_n)
    esnm          <-  list(c(rcp45_n,rcp85NoBio_n))
    esmlist       <-  list(rcp45_n,rcp85NoBio_n)
    
    sim_msm    <- sim_msm%>%filter(Scenario%in%Scenario_set)
    cat("covariate data loaded ('data/in')...")
# subset of downscaled projections used for the paper = Scenario_set
# bio runs are a sensitivity set of runs to evaluate nutrient forcing
# of boundary conditions, not used here bc they are highly similar to 
# non-bio runs (See Kearney et al. 2020 and Hermann et al. 2019 for more info
# A1B not used bc they were AR4 runs and only went to 2040
# print(as.character(Scenarios[Scenario_set]))
# ACLIM Projection simulations
# "###########################################################"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
# [1]  "#  | mn_Hind"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
# [2]  "#  | MIROC_A1B"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
# [3]  "#  | ECHOG_A1B"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
# [4]  "#  | CCCMA_A1B"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
# [5]  "#  | GFDL_rcp45"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
# [6]  "#  | GFDL_rcp85"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
# [7]  "#  | GFDL_rcp85_bio"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
# [8]  "#  | MIROC_rcp45"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
# [9]  "#  | MIROC_rcp85"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
# [10] "#  | CESM_rcp45"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
# [11] "#  | CESM_rcp85"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
# [12] "#  | CESM_rcp85_bio"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#  "###########################################################"  

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




