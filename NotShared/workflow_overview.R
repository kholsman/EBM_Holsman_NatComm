# Overall structure and workflow:

# Previous analyses:
#-----------------------
# CEATTLE Model assessment (Holsman et al. 2019)
#    * single - species mode
#    * multi  - species mode
# recruitment covariates (paper) (Holsman et al. in prep; 2019)
#   * with no environemental covariates (climate naive)
#   * with environmental covariates
# ROMSNPZ hindcasts (Kearney et al. 2020; Hermann et al. 2019) 1979-2018
# ROMSNPZ projections (Hermann et al. 2019) 2006-2100
#   * RCP 4.5 
#         * MIROC
#         * GFLD
#         * CESM
#   * RCP 8.5 
#         * MIROC
#         * GFLD
#         * CESM
#  --> produces bottom temp, sst, zooplankton, etc. annual covariates 
# Harvest strategies
#         B0, B40 via Holsman et al. 2016
#         ATTACH - simulated management system
#-----------------------
# This analysis:
#-----------------------
# Projections of CEATTLE:
# INPUTS =
#   recruitment: 
#        climate naive recruitment
#        rec projections with covariates from ROMSNPZ hindcast
#   catch in each year:
#        F = 0; no catch
#        catch = ABC, where ABC = harvest control rule with slope and b20 cutoff (fig X)
#        catch = simulated ABC-->TAC-->Catch via ATTACH()
#   selectivity, and other parms from ceattle estimation mode and projections
# 
# OUTPUTS:
#   timeseries of biomasss, recruitment, catch under each mode X RCP X management scenario combo for each species
#         Folders : multispecies mode
#   MCMC random draws from mean + SD of recruitment covariates for F= 0
#   Figures and mean values



# load_data
# load_functions.R
# make.R
# packages.R
# setup.R


# Notes on Assessment appraoch:

# aclim_00_JunV2_2019_setB0   these files were used to set the target B0 for B40 iterative calculations

#mnF = last 5 years of the estimation period of ceattle
# 0_1_9mnF and 0_5_9mnF are the same F40s based on meanF from the past 5 years (so not F40, but target F)

#F40In_0_1_9Naive.dat is based on scenario=1, ie persistence of hist climate for each R model:
#e.g., F40In_0_1_9Naive.dat is RS only (no cliamte)
#  F40In_0_5_9Naive.dat is using climate indices but held constant


# _059_mnFcf  uses F40In_0_5_9mnF.dat
# 
# _059_cf  uses F40In_0_5_9.dat  as the target F
# 
# _059_CENaivecf  uses F40In_0_5_9Naive.dat






