*Data and code are under review and subject to change. Please do not use without permission from lead author: <kirstin.holsman@noaa.gov>*

Kirstin Holsman
Alaska Fisheries Science Center
NOAA Fisheries, Seattle WA
**<kirstin.holsman@noaa.gov>**
*Last updated: Jun 29, 2020*

------------------------------------------------------------------------

1. Overview
===========

This is an overview of the data, code, and workflow used to generate intermediate and final data for the Holsman et al. in review Nature Communications paper. Below are instructions for downloading the code and intermediate and final data and for re-generating the figures and analyses presented in the paper. Note, in all cases species 1 = walleye pollock, species 2 = Pacific cod, and species 3 = arrowtooth flounder.

1.1. Data and Figures
---------------------

Intermediate data from the ACLIM models and simulations can be found in the **data/in** sub-folder in the form of `.Rdata` files. Final data from the analyses of the paper can be found in the **data/out** sub-folder (also `.Rdata` files).

Final figures and tables (including illustrator files that were used to add fish icons) can be found in the **Figures** sub-folder. These figures can be called direct or re-generated (see section below ).

2. Getting started:
===================

2.1. Download the code from the github repository:
--------------------------------------------------

``` r
    # download the code:
    main_nm       <- "EBM_Holsman_NatComm-master"
    download_path <- path.expand("~/desktop")
    main          <- file.path(download_path,main_nm)
   
    # download the code:
    dest_file     <- file.path(download_path,paste0(main_nm,".zip"))
    url           <-"https://github.com/kholsman/EBM_Holsman_NatComm/archive/master.zip"
    download.file(url=url, destfile=dest_file)
    
    # unzip the .zip file
    setwd(download_path)
    unzip (dest_file, exdir = "./",overwrite = T)
    setwd(main)
```

3.1. Download data from figshare:
---------------------------------

To run the analyses or create the paper figures you will now need to download the large zipped data folder here: <https://figshare.com/s/81007e2dd5edee0a5a7a> (Data 10.6084/m9.figshare.12568625) and copy - paste the contents (folders "in" and "out"") it in the directory: '\[your local directory path\]/EBM\_Holsman\_NatComm/data' or simply run the following script to download and place the data in the correct sub-folders:

``` r
    cat("The download takes a few mins (large data files)...\n")

    url <-  "https://ndownloader.figshare.com/files/23442137?private_link=81007e2dd5edee0a5a7a"
    dest_path  <-  file.path(main,"data.zip")
    download.file(url=url, destfile=dest_path,method="libcurl")
    
    cat("\nDownload complete...\n")
    
    unzip (file.path(main,"data.zip"), exdir = "./",overwrite=T)
    cat("\nFiles unzipped successfully...\n")
```

If you plan to use the data within the folder for purposes beyond rerunning the paper analyses and figures please contact <kirstin.holsman@noaa.gov> and provide the following citation for the data:

10.6084/m9.figshare.12568625 along with Holsman et al. 2020.

3. Regenerating analyses and figures:
=====================================

Below are instructions for running the R scripts to recreate the figures, tables, results, and run the risk and threshold analyses for the paper. The scripts below require R version 3.5.3 (available at <https://cran.r-project.org/bin/macosx/el-capitan/base/>).

3.1. Re-generate plots
----------------------

### Set things up:

The first step is to run the make.R script to load the data, packages, and setup (where various options are specified). Note that the scripts depend on a number of packages that will be installed the first time through running make.R if they are not already included. *A list of those packages can be found in 'EBM\_Holsman\_NatComm/R/packages.R'.*

``` r
    # set your local path:
    # main        <-  file.path(download_path,"EBM_Holsman_NatComm/")
    # e.g., main  <-  "/Users/kholsman/GitHub_new/EBM_Holsman_NatComm/"
    setwd(main)
    
    # load data, packages, setup, etc.
    source("R/make.R")
```

### (optional) Regenerate plots:

To generate the figures in the paper without overwriting them run the 'make\_plots.R' code.

``` r
    cat(paste("\n\n update.figs  = ",update.figs,"\n"))
    cat(paste("\n\n update.outputs  = ",update.outputs,"\n"))

    # You can generate individual plots like this (some take a few mins and throw warnings):
    graphics.off()
    fig2()
    fig3() #  slow....
    fig4() #  slow....
    fig5()
    fig6()
    figS1()
    figS2() #  slow....
    figS3() #  slow....
    figS4()
    figS5()
    figS6()
```

Alternatively, by setting `update.figs` to `TRUE` the script will overwrite the existing figures in the `Figures` folder:

``` r
    update.figs  <- TRUE  
    stop("warning! this will overwrite existing figures in the Figures folder")
    source("R/sub_scripts/make_plots.R")
    update.figs  <- FALSE   # return this to it's default setting
```

### Re-running the intermediate data

To re-run the paper analyses, including risk calculations and threshold/tipping points, set `update.outputs = TRUE`(this is set to `FALSE` by default) and run the `SUB_EBM_paper.R` script:

``` r
    update.outputs  <- TRUE  
    stop("warning! Setting update.outputs to TRUE will overwrite Rdata files in the data folder")
    source("R/sub_scripts/SUB_EBM_paper.R")

    # once complete set to FALSE and reload new datafiles:
    update.outputs  <- FALSE  
    source("R/make.R")
```

Primary and Intermediate Data sources and models:
=================================================

Various simulation outputs were made available for use in this analysis through the interdisciplinary [Alaska Climate Integrated Modeling (ACLIM) project](%22https://www.fisheries.noaa.gov/alaska/ecosystems/alaska-climate-integrated-modeling-project%22). An overview of the project and simulation experiments can be found in [Hollowed et al. 2020](%22https://www.frontiersin.org/articles/10.3389/fmars.2019.00775/full%22).

### Bering10K ROMSNPZ

ACLIM indices used in this analysis can be viewed interactively online at: <https://kholsman.shinyapps.io/aclim>. The indices were produced for the ACLIM project and derived from the outputs of the Bering10K ROMSNPZ model. Downscaled hindcasts and CMIP5 projections of oceanographic and lower trophic conditions from the Bering10K model were developed as part of the ACLIM project. An overview of these projections and the Bering10K ROMSNPZ model can be found in [Hermann et al. 2019](%22https://academic.oup.com/icesjms/article/76/5/1280/5477847%22), [Kearney et al. 2020](%22https://gmd.copernicus.org/articles/13/597/2020/%22), and [Hollowed et al. 2020](%22https://www.frontiersin.org/articles/10.3389/fmars.2019.00775/full%22). An overview of the Bering10K ROMSNPZ model can be found [here](%22https://beringnpz.github.io/roms-bering-sea/intro/%22).

Kearney K, Hermann A, Cheng W, Ortiz I, Aydin K (2020) A coupled pelagic-benthic-sympagic biogeochemical model for the Bering Sea: documentation and validation of the BESTNPZ model (v2019.08.23) within a high-resolution regional ocean model. Geosci Model Dev 13:597-650. <DOI:10.5194/gmd-13-597-2020>.

### CEATTLE

CEATTLE is a climate-enhanced multispecies stock assessment model for walleye pollock, Pacific cod, and arrowtooth flounder ([Holsman et al. 2016](%22https://www.sciencedirect.com/science/article/pii/S0967064515002751%22), [2019](%22https://archive.afsc.noaa.gov/refm/docs/2019/EBSmultispp.pdf%22)) that has been updated annually and included as an appendix to the BSAI walleye pollock stock assessment ([Ianelli et al. 2019](%22https://archive.afsc.noaa.gov/refm/docs/2019/GOApollock.pdf%22)) since 2016 as part of the Bering Sea fishery stock assessment process. As part of ACLIM CEATTLE was coupled to the ROMSNPZ model and the ATTACH model (below) to generate projections of species biomass and catch under future climate conditions in the Bering Sea. Methods for this coupling and projection simulation can be found in Holsman et al. submitted and Hollowed et al. 2020. The simulation outputs, scripts, and input data files used to generate these simulations can be found on the ACLIM-CEATTLE gitrepo and figshare sites. Details about the CEATTLE model can be found in the [2018 Multispecies assessment](%22https://archive.fisheries.noaa.gov/afsc/REFM/Docs/2018/BSAI/2018EBSmultispp-508.pdf%22) and [2019 Assessments](%22https://archive.afsc.noaa.gov/refm/docs/2019/EBSmultispp.pdf%22).

Three harvest simulations are included in the available simulations: 1. Catch = ABC where multispecies assessment simulations (run in ADMB) using climate naive reference points for BO but climate specific B40 and projections (climate effects on growth, M2, and recruitment). 2. Catch = ABC + sloping harvest control rule below B40, and with F = 0 when B &lt; B20. 3. As in 2 but where catch~f(ABC,TAC) via the ATTACH package (below).

### ATTACH

The catchfunction package (which we refer to as the ABC To TAC and Commercial Harvest, aka ATTACH, model: R package rename forthcoming) was created for the Alaska Climate Integrated Modeling Project (ACLIM) by Amanda Faig (University of Washington; School of Aquatic Fisheries and Sciences) and Alan Haynie (NOAA; NMFS). This function, in a nutshell, takes Bering Sea (BS) acceptable biological catch (ABC) as input and uses a series of regression estimates to predict total allowable catch (TAC) and from that the commercial harvest in the Bering Sea, based on ABC, TAC, and catch data from 1992 to 2017. Documentation and code for ATTACH can be found on the [attach github](%22https://github.com/amandafaig/catchfunction%22).
