# MSMt Model Script 
# Last updated: Oct 24 2014 (KH)
# Kirstin Holsman 


# This script will run MSMt from R
# 1. set up script
# 2. read in diet data
# 3. configure .ctl files and .dat files
# 4. run model
# 5. run RS model fits & create .dat files for projections
# 6. run projections
# 7. plot initial results

#######################################
# 0. set up script
#######################################
rm(list=ls())
graphics.off()

root<-"/Users/kkari/Dropbox/MSMt"
MSM_type<-2		# 0 = single species, 2= multispecies
sp<-c("Pollock","Cod","Arrowtooth")
compile.msm<-1		# load admb and create executable 
run.hindcast<-1		# run the hindcast script for MSM
run.RSfit<-1		# run the script to fit spawner recruitment functions to MSM
run.projections<-1	# run the projections script for MSM

# some other stuff to get going:
source(file.path(root,"msm-master/Scripts/MSMt_R/funKir.R"))
source(file.path(root,"msm-master/Scripts/MSMt_R/funMSMt.R"))
admb_cmd<-"export ADMB_HOME='/Applications/ADMBTerminal.app/admb'; export PATH='/Applications/ADMBTerminal.app/admb/bin':${PATH}"
msm_path<-file.path(root,"msm-master/main")
data_path<-file.path(root,"msm-master/raw_data")

#######################################
# 1. read in RACE data and make .dat files
#######################################
read.csv(file.path(data_path,"survey_biomass"))
read.csv(file.path(data_path,"fishery_biomass"))

- save to "Data/dat_input_files" folder

#######################################
# 2. read in NOAA diet data and make .dat files
#######################################
- save to "Data/dat_input_files" folder

#######################################
# 3. configure .ctl files and .dat files
#######################################
- save to "Control_files" folder

#######################################
# 4. compile and run model
#######################################
if(compile.msm) 	system(paste(admb_cmd,";cd ",msm_path,";admb tmsm"))
if(run.hindcast)
{

	system(paste("cd ",msm_path,"; pwd;cd ..;./run.sh",MSM_type))
	cat(" ################################################","\n","\n",
		"MSMt type = ",MSM_type," ESTIMATION PHASE COMPLETE, SAVING RESULTS.....","\n","\n",
		" ################################################")
	reppath<-file.path(root,paste("/msm-master/tmsm_",MSM_type,"/results/tmsm_R2_est.rep",sep=""))
	eval(parse(text=paste("tmsm_",MSM_type,"<-read_estrep(reppath)",sep="")))
	outpath<-file.path(root,paste("msm-master/tmsm_",MSM_type,"/tmsm_",MSM_type,".Rdata",sep=""))
	eval(parse(text=paste("save(tmsm_",MSM_type,",file=outpath)",sep="")))
}

if(plot.est)		#source("plot_Est.R")
#if(run.hindcast) system(paste(admb_cmd,";cd ",msm_path,";admb tmsm"))
#######################################
# 5. run RS model fits & create .dat files for projections
#######################################
if(run.RSfit)		#system(paste("cd ",msm_path,"; pwd;cd ..;./run.sh 0"))

#######################################
# 6. run projections
#######################################
if(run.projections)		#system(paste("cd ",msm_path,"; pwd;cd ..;./run.sh 0"))

#./run_fut_blended.sh 0 1 2  
#######################################
# 7. run_model validation simulation
#######################################

