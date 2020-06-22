# ----------------------------------------
# packages.R
# load or install packages
# subset of Holsman et al. 2020 Nature Comm.
# kirstin.holsman@noaa.gov
# updated 2020
# ----------------------------------------

lib_list <- c(
  # these for reshaping and manipulating data:
    "dplyr", 
    "data.table",
    "svMisc",
    "quantmod",
    "rootSolve",
    "purrr",
    "readxl",   # "drake",
    
  # These for making plots:
    "ggplot2", 
    "mgcv",
    "reshape",
    "wesanderson",
    "scales",
    "ggforce",
    "grid",
    "processx",
    "plotly",
    "extrafont"
  )

# Install missing libraries:
if (length(setdiff(lib_list, installed.packages()[, 1])) > 0) install.packages(req)

# Load libraries:
for(lib in lib_list)
       eval(parse(text=paste("library(",lib,")")))


