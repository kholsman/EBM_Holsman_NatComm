# MSMt: Multispecies stock assessment model with climate covariates
# Last updated 5/24/14
# K. Holsman ( kirstin.holsman@noaa.gov)

# _________________________________________________________
# To run the model for estimation:
# _________________________________________________________
    ./run.sh "$1" "$2"
    "$1" is the msm_mode (0 for single species (decoupled model), 2 for multispecies) 
    "$2" is optional but if included (use 0) then the code will refit the RS function but not re-run the estimation (requires the estimation to have been completed previously)

    For example

           ./run.sh 0
    Will run MSMt in single species mode using the tmsm_0.ctl file, and then run the RS fits for all the covariates specified in the msm_rs.ctl file.
    The model results and RS fitting results (including AIC selection) will be saved in the folder "tmsm_0" under results, including:
      AIC_results :folder with the AIC results (based on the selectRS_USE.R script in the MSM_RS folder in root directory)
      fits_4_MSM  :folder with the dat files needed for projections (based on the selectRS_USE.R script in the MSM_RS folder in root directory)
      MSM_RS_0    :folder with the RS fit results including a copy of the MSM_RS model
      RS_fits     :folder with the total set of RS fits used for AIC selection
        tmsm_est.std : file with the estimated parameter fits (including stdevs) for msmt
        tmsm_R2_est.rep : output report from the MSMt fit
        tmsm.rep 
      tmsm_0.bar
      tmsm_0.ctl : copy of the control file used for the estimation fitting

# _________________________________________________________
# To project the model - without MCMC
# _________________________________________________________

./run_futNew.sh "$1" "$2" "$3"
 "$1" is the msm_mode (0 for single species (decoupled model), 2 for multispecies) 
 "$2" is the recruitment mode (recMode):
    # 0 = project under mean  rec   
    # 1 = mean RS function
    # 2 = RS function plus SST (rs_data4MSM_0_0_0_SST)
    # 3 = RS function based on top AIC selected environm. parms for each spp (rs_data4MSM_TOP)
    # 4 = RS function based on model averaged AIC selected parms for each spp (rs_data4MSM_AICmn)
    # 5 = RS function based on Recfile_name (above)  
"$3" is the fishing mode (proj_mode)
    #1= project under no  fishing,  
    #2= project under mean  fishing rate  from  hindcast, 
    #3= project to  F rate  that  matches Btarget,  
    #4= fit to NewCatch.dat
    #5= project under radnom seeded fishing rate  from  hindcast   

For example

           ./run_futNew.sh 0 2 2

Will run MSMt in projection mode using the single species mode estimation results saved in tmsm_0 and the climate conditions specified in Zoop_fut.dat.
It will then create a folder called "tmsm_0_2_2" within a new folder "projections" in tmsm_0/results that contains various resulting report files including:
    "Future_report.rep" : which contains the SSB, Frate, Catch, and recruitment vectors for each   climate scenrio included in the model projections (and specified in the tmsm_0.ctl file)


# _________________________________________________________
# To project the model with MCMC based on random draws around recruitment
# _________________________________________________________

./run_fut_blended.sh "$1" "$2" "$3" "$4"
 "$1" is the msm_mode (0 for single species (decoupled model), 2 for multispecies) 
 "$2" is the recruitment mode (recMode):
    # 0 = project under mean  rec   
    # 1 = mean RS function
    # 2 = RS function plus SST (rs_data4MSM_0_0_0_SST)
    # 3 = RS function based on top AIC selected environm. parms for each spp (rs_data4MSM_TOP)
    # 4 = RS function based on model averaged AIC selected parms for each spp (rs_data4MSM_AICmn)
    # 5 = RS function based on Recfile_name (above)  
"$3" is the fishing mode (proj_mode)
    #1= project under no  fishing,  
    #2= project under mean  fishing rate  from  hindcast, 
    #3= project to  F rate  that  matches Btarget,  
    #4= fit to NewCatch.dat
    #5= project under radnom seeded fishing rate  from  hindcast   
"$4"  is the number of random "draws" you want to run (e.g., 1000)
This uses the -rand_rec 1 call in the command line to have MSMt estimate random recruitment based on error around the recruitment parameters.

For example

           ./run_fut_blended.sh 0 2 2 10

Will run 10 iterations of MSMt in projection mode using the single species mode estimation results saved in tmsm_0 and the climate conditions specified in Zoop_fut.dat.
It will then create a folder called "MCMC_tmsm_0_2_2" within the new folder "projections" in tmsm_0/results. This folder contains various resulting report files all of which will be preceeded with "i$N" where N is the iteration number.
   e.g.,  "i2Future_report.rep" :  contains the SSB, Frate, Catch, and recruitment vectors for the second MCMC iteration (2) and each climate scenrio included in the model projections (and specified in the tmsm_0.ctl file).






