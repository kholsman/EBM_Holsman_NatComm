
# CEATTLE
Data and code repository for CEATTLE multispecies model. 
See   <a href="http://www.sciencedirect.com/science/article/pii/S0967064515002751">Holsman et al. 2015 DeepSea Res II<a/> for more information..
<p>
For site info email:
<a href="mailto:kirstin.holsman@noaa.gov?Subject=Hello%20again" target="_top">Kirstin</a>
</p>

***
All results are saved in the ``CEATTLE/CEATTLE-outputs`` folder
</p>
##**Estimation Mode:** 
<p>
<p>
To run the model open terminal and navigate to the folder with the script  ``./CEATTLE_run.sh`` (e.g., *CEATTLE/src/ceattle-master*) and enter ``./CEATTLE_run.sh 0`` for single species or ``./CEATTLE_run.sh 2`` for multispecies.

It will produce files in the outputs folder (e.g., *CEATTLE/runs/*) called *ceattle_0* or *ceattle_2*.
 
 
### **Options _NEW_**   

``./CEATTLE_run.sh # -f <filename> -ctl <ctl filename> -o -skip`` <p>
* **_setting a custom filename_**        -f or --file  <filename>  : ``./CEATTLE_run.sh # -f <filename>``  <p>
this will create a model run with the name <filename>. For example ``./CEATTLE_run.sh 0 -f test`` will create a model folder under runs/ called *test_0* and ``./CEATTLE_run.sh 2 -f test`` will create a folder under *runs/* called *test_2.* The default is *ceattle_0* or *ceattle_2*.
 <p>
* **_Specifying the control file_**     -ctl or --control  <filename.ctl>  : ``./CEATTLE_run.sh # -f <filename> -ctl <ctl filename>``  <p>
this will run the model using the specified control file saved in the *CEATTLE/src/Control_files* folder. For example ``./CEATTLE_run.sh 0 -f test -ctl KirsControl.ctl`` will create a model folder under outputs called *test_0* and will run that model using the *KirsControl.ctl* file. The default control file is *ceattle.ctl*.
<p>
* **_Suppressing the overwrite? promt_** -o or --overwrite : ``./CEATTLE_run.sh # -f <filename> -ctl <ctl filename> -o`` <p>
this will force the model to overwrite any files that exist in the outputs folder with the same filename. Proceed with caution. An example is ``./CEATTLE_run.sh 0 -f test -ctl KirsControl.ctl -o``
<p>
* **_Skipping the estimation mode_**      -skip or --skip : <p> ``./CEATTLE_run.sh # -f <filename> -ctl <ctl filename> -o -skip`` 
this will run the recruitment models only by skipping the model estimation and .Rdata file compilation mode. Can only be used to update the recruitment of an existing model in the outputs folder. An example is ``./CEATTLE_run.sh 0 -f test -ctl KirsControl.ctl -skip``

<p>
***
##**Projection Mode:**

<p>
First run the model in estimation mode. Then to run the model open terminal navigate to CEATTLE/src/ceattle-master folder and enter 
``./CEATTLE_run_fut.sh 'msmMode' -r 'recMode #' -h 'harvestMode #'``:

Where ``msmMode`` = is ``0`` or ``2``, for single- or multi-species mode, respectively. 

Where ``recMode`` is recruitment mode:
* 0 = project under mean  rec   (no RS)
* 1 = mean RS function (mean RS no covariates, ricker; rs_data4CEATTLE_0_0_0)
* 2 = RS function plus SST (rs_data4CEATTLE_SST)
* 3 = RS function based on top AIC selected environm. parms for each spp (rs_data4CEATTLE_TOP)
* 4 = RS function based on model with top R2 value (rs_data4CEATTLE_TopR2)
* 5 = RS function based on Recfile_name (above)   
* 6 = RS function plus bottom temp 
* 7 = RS function plus Cold Pool
* 8 = RS function plus full model plus EAT (tot)

Where ``harvestMode``  is used  for the ``CALCMORT()``  function for harvest mortality  in  the projection  / simulation  models: 
* 0= hindcast
* 1= project under no  fishing,  
* 2= project under mean  F  rate  from  hindcast, 
* 3= project to  PF (F rate) that  matches Btarget,  
* 4= fit to NewCatch.dat
* 5= project under F+Fdev radnom seeded fishing rate  from  hindcast
* 6= project using max F from hindcast
* 7= project using mean catch from hindcast
* 8= project using max catch from hindcast
* 9= project using f profiles  

###**Options _NEW_**   
``./CEATTLE_run_fut.sh # -r 'recMode #' -h 'harvestMode #' -f <filename> -ctl <ctl filename> -o -skip -rand <itr>`` <p>
* **_setting a custom filename_**        -f or --file  <filename>  : ``./CEATTLE_run_fut.sh # -f <filename> -r 'recMode #' -h 'harvestMode #'``  <p>
this will create a model run with the name <filename>. For example ``./CEATTLE_run_fut.sh 0 -f test -r 0 -h 1`` will create a model folder under *runs/test_0* called *projections/test_0_0_1* and ``./CEATTLE_run_fut.sh 2 -f -r 0 -h 1 test`` will create a folder under *runs/test_2* called *projections/test_2_0_1.* The default is *ceattle_0_0_1* or *ceattle_2_0_1*.
 <p>
* **_Specifying the control file_**     -ctl or --control  <filename.ctl>  : ``./CEATTLE_run_fut.sh # -f <filename> -ctl <ctl filename> -r 'recMode #' -h 'harvestMode #' ``  <p>
this will run the model using the specified control file saved in the *CEATTLE/src/Control_files* folder. For example ``./CEATTLE_run_fut.sh 0 -f test -ctl KirsControl.ctl -r 0 -h 1`` will create a model folder under runs called *runs/test_0* called *projections/test_0_0_1* and will run that model using the *KirsControl.ctl* file. The default control file is *ceattle.ctl*.
<p>
* **_Suppressing the overwrite? promt_** -o or --overwrite : ``./CEATTLE_run_fut.sh # -f <filename> -ctl <ctl filename> -o -r 'recMode #' -h 'harvestMode #'`` <p>
this will force the model to overwrite any files that exist in the outputs folder with the same filename. Proceed with caution. An example is ``./CEATTLE_run_fut.sh 0 -f test -ctl KirsControl.ctl -o -r 0 -h 1``

<p>
* **_Running a random iteration_** -rand or --mcmc : ``./CEATTLE_run_fut.sh # -f <filename> -ctl <ctl filename> -o -r 'recMode #' -h 'harvestMode #' -rand 'itr #"`` <p>
this will seed the random starts with itr, can be used to generate random draws from recruitment estimates. Loop across itr to get random set. Will save each run with mcmc<itr> filename. An example is ``./CEATTLE_run_fut.sh 0 -f test -ctl KirsControl.ctl -o -r 0 -h 1 -rand 10``

<p>
***
##**Updating CEATTLE inputs:**  

Updating CEATTLE requires updating the following biomass and catch data:  

###Step 1: Run database queries for biomass:
Run the following script:``GitHub\R_SQL_Code\Get_DATA_Kirstin.R``. Note that the script must be run in 32 bit R. The script uses LW regressions that are NOT biomass weighted and are not annual specific but are region specific. It updates the regression with the latest data in the database. Outputs are ``[spp_name]_CPUE_[region].Rdata``. For example: plk_CPUE_ebs.Rdata. The code results in station specific CPUE and proportion based on counts and weight.

Kirstin's local path for these outputs is:

*//AFSC-S79/REFM_Users/Kirstin.Holsman/My Documents/Holsman_AFSC_science/AFSC_data/AFSC_data_all*

``\\AFSC-S79\REFM_Users\Kirstin.Holsman\My Documents\GitHub\R_SQL_Code\Get_DATA_Kirstin.R``
This code queries the data base for CPUE by number or by biomass and creates proportional multipliers for biomass weighting. It also creates the area weighted temperature data file for input into CEATTLE (saved as srvy_TempObs.Rdata).

* Step 2: Run database queries for diet data:
``\\AFSC-S79\REFM_Users\Kirstin.Holsman\My Documents\GitHub\R_SQL_Code\Create_AggPrey.R``

This code creates the aggregated datafiles of predator prey data by Weight or by count.

* Step 3: Run database queries for weight and length at age

* Step 4: Biomass weight the diet data to derive annual estimates of proportion by age in the diet of predators by age
* Step 5: Biomass weight the length at age data to derive mean weight at age by year and temperature
* Step 6: Use observed rations and bioenergetics model estimates of max C to derive relative foraging rates (annual)
* Step 7: [if applicable] Use ROMS/NPZ or survey data to get overlap and bottom temperature for each year

* Step 8: Update stomach.dat files
* Step 9: Update srvy and fishery based age comp data and biomass data


