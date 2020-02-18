# abc2c.R
## R code to read in abc and convert to catch based on the rfunction developed by amanda
## K Holsman

#install.packages("devtools")
#devtools::install_github("amandafaig/catchfunction")
# print(" Running R code to convert ABC to catch")
# print("_______________________________________")
# require("devtools",quietly=F)
require("catchfunction",quietly=T)

# make vanilla

# VGAM
# methods
# stats4
# splines
# load("/Users/kholsman/Documents/GitHub/CEATTLE/src/ceattle-master/Scripts/R_code/abc2c_workspace.Rdata")
# setwd("/Users/kholsman/GitHub/CEATTLE/src/ceattle-master/main")

	#print("running abc2c.R")
	#setwd("/Users/kholsman/GitHub/CEATTLE/src/ceattle-master/main/conn22")
	#print(getwd())
	setwd("../")
	#print(getwd())
	file.create("tmp.dat", overwrite=T)
	cat(33,file="tmp.dat",append=F,sep=" ")
	stoprun<-FALSE

	#/Users/kholsman/GitHub/CEATTLE/src/ceattle-master/main/abc_out.dat
	#/Users/kholsman/GitHub/CEATTLE/src/ceattle-master/Scripts/R_code/abc2c.R
	## read in dat file from ADMB
	dat        <-  data.frame(read.csv("abc_out.dat",sep=" ",comment="#", header=F))
	scn        <-  dat[1,]
	yr         <-  dat[2,]
	mode       <-  dat[3,]
	climScn    <-  dat[4,]
	minBlimMult<-  dat[5,]
	nspp       <-  dim(dat)[1]-5
	abc        <-  as.numeric(dat[(1:nspp)+5,])

	if (scn%in%(1:4)==FALSE) 
		e      <-  simpleError("ERROR with catch function: scn is not in the set 1:4")
	Logfl      <-  paste0("CFunLog_",Sys.Date(),paste("",scn,mode,climScn,sep="_"),".dat")

	makeLog    <-  function(abc, e, yr, mode, climScn){
		if(file.exists(Logfl)==FALSE){
			file.create(Logfl)
			cat("Time CatchScen Pollock_ABC PCod_ABC Arrowtooth_ABC, yr, mode, climScn error", sep="\n",append=F,file=Logfl)
		}
		cat(paste(Sys.time(),paste(c(scn,abc,yr,mode,climScn,e),collapse=" ")), sep="\n",append=T,file=Logfl)
	}
	catch      <-  rev(catch_function(scenario=scn, Pollock=abc[1],PCod=abc[2],Arrowtooth =abc[3]))

	if("catch"%in%ls()==FALSE)
	{  
		stoprun<-  TRUE
	    e      <-  simpleError("ERROR with catch function: catch not calculated")
	}

	if(stoprun){
	  # if there is no catch calculated for a given year
	  tryCatch(stop(e),error = makeLog(abc, e,yr=yr,mode=mode,climScn=climScn))
	  cat(1,file="killrun.dat",append=F)  # kill the ADMB run
	}

	# now write the catch out file
	file.create("catch_out.dat",overwrite=T)
	cat(as.numeric(catch),file="catch_out.dat",append=F,sep=" ")

	# script finished up so set the file to run 
	file.create("tmp.dat", overwrite=T)
	cat(10,file="tmp.dat",append=F,sep=" ")
