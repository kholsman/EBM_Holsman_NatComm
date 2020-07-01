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
  #load(file.path(in_dir,"sim_msm.Rdata"))
  load(file.path(in_dir,"target_B_2.Rdata"))
  #load(file.path(in_dir,"mclist2.Rdata"))
  #load(file.path(in_dir,"simlist2.Rdata"))
  #load(file.path(in_dir,"empty2.Rdata"))
  run_def <- readxl::read_xlsx(file.path(in_dir,"Run_Defintions.xlsx"),sheet="Sheet1")
  #_______________________________________
  # Load simulations, risk calcs, and threshold results (or run these if not in folder):
  #_______________________________________

  cat("\nLoading Intermediate data ('data/in')...\n")
    for(fn in infn){
      if(!any(dir(in_dir)%in%fn))
        stop(paste0(fn," file not found in: \t \t",in_dir,
                    "\n\nplease go to: https://figshare.com/s/6dea7722df39e07d79f0","",
                    "\n\nand download file into: \t \t",in_dir,"/",fn))
      load(file.path(in_dir,fn))
      cat(paste("\nloaded",fn))
    }
  cat("\nIntermediate data loaded ('data/in')...\n")
  
  cat("\n\nLoading final data ('data/out')...\n")
  for(fn in outfn){
    if(!any(dir(out_dir)%in%fn))
      stop(paste0(fn," file not found in: \t \t",out_dir,
                  "\n\nplease go to: https://figshare.com/s/6dea7722df39e07d79f0","",
                  "\n\nand download file into: \t \t",out_dir,"/",fn))
    load(file.path(out_dir,fn))
    cat(paste("\nloaded:",fn))
  }
  cat("\nfinal data loaded ('data/out')...\n\n")
   
  #_______________________________________
  # Load ROMSNPZ covariates:
  #_______________________________________

    Scenarios     <-  unique(covariates$Scenario)
    A1B_n         <-  grep("A1B",Scenarios)
    bio_n         <-  grep("bio",Scenarios)
    rcp45_n       <-  grep("rcp45",Scenarios)
    rcp85_n       <-  grep("rcp85",Scenarios)
    rcp85NoBio_n  <-  setdiff(rcp85_n,bio_n)
    plotList      <-  Scenario_set  <- c(1,rcp45_n,rcp85NoBio_n)
    esnm          <-  list(c(rcp45_n,rcp85NoBio_n))
    esmlist       <-  list(rcp45_n,rcp85NoBio_n)
    
    simnames  <- Scenarios
    Years     <- sort(unique(dat_2_5_12$future_year)+start_yr-1)
    nYrsTot   <- length(Years )
    riskTypes <- unique(risk12$type)

    #sim_msm    <- sim_msm%>%filter(Scenario%in%Scenario_set)
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





