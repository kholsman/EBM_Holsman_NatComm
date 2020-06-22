# ----------------------------------------
# setup.R
# subset of Holsman et al. 2020 Nature Comm.
# kirstin.holsman@noaa.gov
# updated 2020
# ----------------------------------------
  
  # set up directory paths:
  #-------------------------------------------
    main            <- getwd()
    fun_dir         <- "R/sub_Fun" # function subdirectory - is this used?
    out_dir         <- "data/out" # function subdirectory - is this used?
    
    # Specify URL where file is stored
    url <- "https://www.stats.govt.nz/assets/Uploads/Annual-enterprise-survey/Annual-enterprise-survey-2017-financial-year-provisional/Download-data/annual-enterprise-survey-2017-financial-year-provisional-csv.csv"
    
    # Specify destination where file should be saved
    destfile <- "C:/Users/ ... Your Path ... /my folder/output.csv"
    
    # Apply download.file function in R
    download.file(url, destfile)
    
  # switches and options:
  #-------------------------------------------
    update.figs     <-  FALSE  # set to true to re-save figs
    update.outputs  <-  TRUE   # overwrite the existing Rdatafiles in data/out
    status          <-  TRUE   # print progress
    
  # Some settings for which scenarios to evaluate:
  #-------------------------------------------
    start_yr        <-  1979        # first year of the hindcast simulation
    fut_start       <-  2018        # first year of the projections
    refrun          <-  19
    scn             <-  2
    h               <-  13
    rfset           <-  c(19,59)    # climate niave, climate-informed B0
    rset            <-  c(1,5)[2]   # use climate enhanced recruitment
    
   # Risk thresholds/limits (used for threhold/tipping point analysis)
   #-------------------------------------------
    limlist         <-  c(-10,-50,-80)  # % thresholds for decline, severe decline, collapse relative to persistence scenario
    modeLIST        <-  c("SSM","MSM")
    Yrbin           <-  c(2017,2025,2050,2075,2100)  # bins for the risk calculation
 
    #-------------------------------------------    
    # threhold settings:
    
    # s            <-  1
    # itr          <-  1
    # adj          <-  0
    # yrs          <-  sort(unique(start_yr + dat_2_5_12$future_year-1))
    # hind_yrs     <-  match(1979:2017,yrs)
    # fut_yrs      <-  match(2018:max(yrs),yrs)
    # ref_yrs      <-  match(yrs,2007:2017)
    # sim          <-  rcp45_n
    # sim          <-  rcp85NoBio_n  # don't include the bio runs
    rndNIN       <-  6 # sig digits for rounding the first and second derivatives
    boot_nobsIN  <-  1000  # this controls the sensitivity of the analysis
    nitrIN       <-  1000
    t_knots      <-  4   # numnber of knots for the threshold gams
    methodIN     <-  2   # method for getting the threshold significance
   
 # Species stuff: (used for plotting and manipulating data)
 #-------------------------------------------
    update.figs     <-  FALSE
    sppINFO<-list(
    plk=list(abv="plk",
               guildIN="Walleye pollock",
               plotSPP="walleye pollock",
               bin2=c(seq(0,300,10),1000),
               binJvAD=c(0,40,1000),
               splistIN="W. Pollock",doNEBS=T,plotIT=T),
      pcod=list(abv="pcod",
                guildIN="Pacific cod",
                plotSPP="Pacific cod",
                bin2=c(seq(0,300,10),1000),
                binJvAD=c(0,40,1000),
                splistIN="P. Cod",doNEBS=T,plotIT=T),
      atf=list(abv="atf",
               guildIN="Arrowtooth or Kamchatka",
               plotSPP="arrowtooth flounder",
               bin2=c(seq(0,300,10),1000),
               binJvAD=c(0,40,1000),
               splistIN=c("Arrowtooth","Arrow or Kam", "Kamchat fl"),doNEBS=F,plotIT=T)
    )
    
    # These switches for KHolsman during simulation updates:
    #-------------------------------------------
    retroFL         <-  "data/raw/retro_data2018_long_ext_bcs.dat"
    futFL           <-  "data/raw/proj_data2018_long_ext_bcs.dat"
    fldr_nm         <-  "aclim_00_JunV2_2019"  # folder with the CEATTLE assessment runs
    UpdateMCMC      <-  1      # update MCMC? 1 = TRUE, 0 = FALSE
    readdat         <-  FALSE  # re-read in new data?
    update.simlist  <-  FALSE  # only TRUE when re-running CEATTLE simulations 
    update.romsnpz  <-  FALSE  # only TRUE when re-running CEATTLE simulations
    
    
  # Plotting stuff:
  #-------------------------------------------
  # The width of figures, when printed, 
  # will usually be 5.5 cm (2.25 inches or 1 column) 
  # or 12.0 cm (4.75 inches or 2 columns). 
  
  dpiIN           <-  150    # dpi for figures
  
  # set up color palettes
  col1<-colorRampPalette(colors()[c(280,320)])
  col2<-colorRampPalette(colors()[c(70,491)])
  col2<-colorRampPalette(colors()[c(114,491)])
  col3<-colorRampPalette(c("yellow","red"))
  
  # set the color scheme
  coll_use         <-  c(colors()[320],col2(6)[c(2,3,4)],col3(6)[c(3,4,6)])
  
  plt     <- c("Zissou1","Darjeeling1","Darjeeling2","FantasticFox1")
  # set up colors
  blues   <- RColorBrewer::brewer.pal(5, "Blues")
  BG      <- RColorBrewer::brewer.pal(9, "GnBu")  #5
  Ornjazz <- RColorBrewer::brewer.pal(5, "Oranges")
  YGB     <- (RColorBrewer::brewer.pal(5, "YlGnBu"))
  bg      <- colorRampPalette(BG)
  YlGnBu  <- colorRampPalette(YGB[-1])
  blu     <- colorRampPalette(blues[-1])
  night   <- colorRampPalette(colors()[c(653,47,474,72,491,477)])
  dawn    <- colorRampPalette(c(colors()[c(477,491,72,474,47,653)],"orange","red"))
  orng    <- colorRampPalette(Ornjazz[1:5])
  plt     <- c("Zissou1","Darjeeling1","Darjeeling2","FantasticFox1")
  colIN1  <- colorRampPalette(c(wes_palette(n=5, name=plt[1])[1:5]))
  #col_in  <- colorRampPalette(colors()[c(408,44,73)])
  col_in  <- colorRampPalette(colors()[c(459,122,73)])
  col_in2 <- colorRampPalette(c("orange","red"))
  wes     <- colorRampPalette(c(wes_palette(n=5, name=plt[1])[1:5]))
  col2    <- colorRampPalette(c(wes(7)[c(3,1)],col2(3)[3]))
  col3    <- colorRampPalette(c(wes(7)[4:7]))
  
  
  
  # Set up plotting stuff:
  #------------------------------------     
 
  probbs        <-  c(.1,.25,.5,.75,.9)
  alphaAll      <-  ceiling(rep(100/(length(probbs)/2),length(probbs)/2))
  
  c1            <-  col2(5)
  c2            <-  col2(6)[seq(2,6,2)]
  c3            <-  col3(6)[seq(2,6,2)]
  collIn        <-  rep(NA,13)
  collIn[1:2]   <-  col2(2)[2]
  #collIn[c(A1B_n,bio_n)]  <-  c1
  #collIn[rcp45_n]         <-  c2
  #collIn[rcp85NoBio_n]    <-  c3
  ltyall                  <-  rep(1,13)  
  ltyall[1:2]             <-  1
 # ltyall[c(A1B_n,bio_n)]  <-  1
#  ltyall[rcp45_n]         <-  c(1,1,2)
  #ltyall[rcp85NoBio_n]    <-  c(1,1,2)
  lwdall                  <-  rep(1,13)  
  lwdall[1:2]             <-  2
 # lwdall[c(A1B_n,bio_n)]  <-  1
 # lwdall[rcp45_n]         <-  c(1,1,1)
 # lwdall[rcp85NoBio_n]    <-  c(1,1,1)
  
  
  # General workflow:
  
  


