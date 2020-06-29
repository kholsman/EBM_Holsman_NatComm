# ----------------------------------------
# load_functions.R
# subset of Holsman et al. 2020 Nature Comm.
# kirstin.holsman@noaa.gov
# updated 2020
# ----------------------------------------

  for(d in dir("R/sub_fun")) 
    source(paste0("R/sub_fun/",d))
  
  for(fn in dir("R/sub_fun/Fig_fun"))
    source(file.path(main,"R/sub_fun/Fig_fun",fn))

  # This script generates the figure functions for the paper:
  source("R/sub_scripts/load_paper_figs.R")
    

  
 
  
  
  

  
  
  
  