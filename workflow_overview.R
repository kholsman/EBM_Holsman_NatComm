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


