# ----------------------------------------
# make.R
# subset of Holsman et al. 2020 Nature Comm.
# kirstin.holsman@noaa.gov
# updated 2020
# ----------------------------------------

source("R/packages.R")       # loads packages
source("R/setup.R")          # load other switches and controls
source("R/load_functions.R") # defines the create_plot() function
source("R/load_data.R")      # load other switches and controls

source("R/plan2.R")          # creates the drake plan

# options(clustermq.scheduler = "multicore") # optional parallel computing. Also needs parallelism = "clustermq"
make(
  plan2, # defined in R/plan.R
  verbose = 2
)