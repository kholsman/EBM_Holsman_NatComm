#________________________________________________
# GG plots for EBFM CEATTLE climate paper
# Kirstin.holsman@noaa.gov
#________________________________________________

for(fn in dir("R/sub_fun/Fig_fun"))
  source(file.path(main,"R/sub_fun/Fig_fun",fn))

# The width of figures, when printed, 
# will usually be 5.5 cm (2.25 inches or 1 column) 
# or 12.0 cm (4.75 inches or 2 columns). 

