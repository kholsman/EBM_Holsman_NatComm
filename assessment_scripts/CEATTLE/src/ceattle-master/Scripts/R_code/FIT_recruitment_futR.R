
##_____________________________________________________
## Kirstin Holsman
## Feb 2020
## 
## Rcode for creating .dat files for recruitment model of CEATTLE
## TASK LIST
# fit RS function to scaled values
# recenterprojections on hindcast
# calculate out of sample fit
##_____________________________________________________

# ###########################################################
# First set things up
# ###########################################################

	rm(list=ls())
	# rm(list=ls()); setwd("/Users/kholsman/GitHub/CEATTLE/runs/assmnt_2019_2")

	parm.list  <-  c("fallZavg","springZavg","ColdPool","BottomTemp")
	eat_parms  <-  c("fallZtot","fallZavg","springZtot","springZavg" )
	hind_fl    <-  "ROMS_NPZ.Rdata"
	fut_fl     <-  "ROMS_NPZ_covars.Rdata"
	run_models <-  1
	rmYrs      <-  0    # which years to omit for "leave 1 out" of recruitment function
	
	#________________________________________
	# Read in the .config file
	#________________________________________

		tt    <-  read.csv("tmpfile.txt",header=FALSE,colClasses="character")[[1]]
		nt    <-  length(grep("#",tt))
		tt_nm <-  unlist(strsplit(tt[grep("#",tt)],split="#"))[seq(2,nt*2,2)]
		tt    <-  read.csv("tmpfile.txt",comment="#",header=FALSE,colClasses="character")[[1]]
		for(itt in 1:nt)
			eval(parse(text=paste(tt_nm[itt],"<-'",tt[itt],"'",sep="")))
		if(testread!=12345){message(paste("Error with reading tmpfile.txt  line 29 of FIT_recruitment.R in ",getwd()))}
		model_path  <-  file.path(DIR_main,model_path)
		rm(tt_nm)
		rm(nt)
		rm(tt)
	#________________________________________
	#________________________________________

	Type1    <-  mode
	path     <-  file.path(DIR_main,output_path)
	fn.use   <-  paste("results/",filename,"_rs.dat",sep="")
	plotit   <-  TRUE  #plot results?
	cat ("running analysis; please wait")
	cat ("\n")
	cat ("..")

	sp       <-  c("Pollock","Cod","Arrowtooth")
	np       <-  length(sp) # number of spp

	#________________________________________
	#________________________________________

	grep.list<-function(find=x,source=longlist){
		return(which(source%in%find))
	}

	mean.na<-function(x){
		mean(x,na.rm=TRUE)
	}
	sum.na<-function(x){
		sum(x,na.rm=TRUE)
	}
	sd.na<-function(x){
		sd(x,na.rm=TRUE)
	}
	max.na<-function(x){
		max(x,na.rm=TRUE)
	}
	min.na<-function(x){
		min(x,na.rm=TRUE)
	}
	read_rs<-function(fn=fn.use){
		allocate<-function(ii=3){
			tt3<-matrix(0,rs_data$nspp,rs_data$nyrs)
			for(i in 1:rs_data$nspp){
				ii<-ii+1
				tt3[i,]<-as.numeric(na.omit(as.numeric(strsplit(as.character(tt2[[1]][[ii]]),split=" ")[[1]])))
			}
			return(tt3)
		}
		rs_data<-list()
		tt2<-read.csv(fn,header=FALSE)
		rs_data$nspp<-as.numeric(as.character(tt2[[1]][1]))
		rs_data$nyrs<-as.numeric(as.character(tt2[[1]][2]))
		rs_data$ln_mn_rec<-as.numeric(na.omit(as.numeric(strsplit(as.character(tt2[[1]][[3]]),split=" ")[[1]])))
		
		
		rs_data$rec_dev<-allocate(3)
		rs_data$SSB<-allocate(6)
		rs_data$total_ration_1yrold<-allocate(9)
		rs_data$total_ration_2plus<-allocate(12)
		return(rs_data)
	}
	
# ###########################################################
# Now load the data
# ###########################################################
	
	# load model results:
		load(file.path(path,"results/CEATTLE_results.Rdata"))
	
	# read in the future and hindcast data sets
		fl<-file.path(DIR_main,cntl_path,ctl_filename)
		ifile <- scan(fl, what = "character", flush = T, blank.lines.skip = T, quiet = T)
		hind_fl<-ifile[grep("retrofile_name",ifile)+1]
		fut_fl<-ifile[grep("futfile_name",ifile)+1]
		rm(fl)
	
	# read in the models and covs

		fl<-file.path(DIR_main,data_path,fut_fl)
		ifile <- scan(fl, what = "character", flush = T, blank.lines.skip = T, quiet = T,sep=",")

		tt<-unlist(strsplit(ifile[grep("Covars",ifile)+1],split=" # "))
		tt<-unlist(strsplit(tt,split="# "))
		if(any(tt=="")) covars<-tt[-which(tt=="")]
		rm(tt)

		tt<-unlist(strsplit(ifile[grep("Models",ifile)+1],split=" # "))
		tt<-unlist(strsplit(tt,split="# "))
		if(any(tt=="")) models<-tt[-which(tt=="")]
		rm(tt)
		rm(fl)
		rm(ifile)

		ncovs<-length(covars)

	# create future control file for recruitment and CEATTLE
		fut_cntl_nm<-paste(strsplit(ctl_filename,".ctl")[[1]],"_fut.ctl",sep="")  # create future ctl file
		fl<-file.path(DIR_main,cntl_path,fut_cntl_nm)
		if(file.exists(fl)){file.remove(fl)}
		file.create(fl)
		cat(models,file=fl,append=FALSE,sep="	")
		cat("",file=fl,append=TRUE,sep="\n")

	# read in Retro data file:
		fl<-file.path(DIR_main,data_path,hind_fl)
		ifile <- scan(fl, what = "character", flush = T, blank.lines.skip = T, quiet = T,sep=",")
		cc<-grep("#",ifile)
		retro_yrs<-as.numeric(unlist(strsplit(ifile[grep("#Retro_years",ifile)+1],split=" ")))
			
		covs_ln<-c(grep("#COVAR_START",ifile[cc]),grep("#COVAR_END",ifile[cc]))
		cc<-cc[(covs_ln[1]+1):(covs_ln[2]-1)]
		retro<-data.frame(year=retro_yrs)
		for(i in 1:length(ifile[cc])){
			rmit<-c("","#",":")

			tt<-unlist(strsplit(ifile[cc][i],split=" "))
			nm<-tt[-which(tt%in%rmit)];rm(tt)  # name of covariate
			tt<-as.numeric(unlist(strsplit(ifile[cc[i]+1],split=" ")))
			eval(parse(text=paste("retro<-data.frame(retro,",nm,"=tt)",sep="")))
		}
	

	MSM_type<-Type1    # 2= multispecies, 0 = single species

	fig.file<-file.path(DIR_main,rec_path,"Rec_figs")
	print(file.path(DIR_main,rec_path,"Rec_figs"))
	dir(file.path(DIR_main,rec_path))

	if(file.exists(fig.file)){cat("")}else{dir.create(fig.file)}

	rs_data<-read_rs()
	tmp$Tot_rat_1yrold<-matrix(0,tmp$nspp,tmp$nyrs)
	tmp$Tot_rat_1yrold[1,]<-(tmp$AvgN_1*tmp$ration2_1)[,1]
	tmp$Tot_rat_1yrold[2,]<-(tmp$AvgN_2*tmp$ration2_2)[,1]
	tmp$Tot_rat_1yrold[3,]<-(tmp$AvgN_3*tmp$ration2_3)[,1]

	setwd(path)

	nyrs<-nyrs.all<-rep(0,np)
	covuse<-covuse.all<-Rec.obs<-list()
	tmsm_est<-tmp
	# names(ROMS_NPZ_covars[[1]])
	
	# create the covuse list for the covariates for each spp:

	for(i in 1:np){
		tmp1<-data.frame(
			Year=(1:tmp$nyrs[1,1]+tmp$styr[1,1]-1)[-1],
			SSB=eval(parse(text=paste("tmp$BiomassSSB_",i,"[1,]",sep="")))[-tmp$nyrs[1,1]],
			REC=eval(parse(text=paste("tmp$R_",i,"[1,]",sep="")))[-1],
			RAT1_lag1=tmp$Tot_rat_1yrold[i,-(tmp$nyrs[1,1])]		)
		retro_lag1<-retro
		retro_lag1[-1,-1]<-retro_lag1[-dim(retro)[1],-1]
		covuse.all[[i]]<-covuse[[i]]<-merge(y=retro,x=tmp1,by.y="year",by.x="Year")
		covuse.all[[i]]<-covuse[[i]]<-merge(y=retro_lag1,x=tmp1,by.y="year",by.x="Year")
		
		nyrs.all[i]<-nyrtmp<-dim(covuse[[i]])[1]
		covuse[[i]]<-covuse[[i]][1:(nyrtmp-rmYrs),]
		nyrs[i]<-dim(covuse[[i]])[1]
		Rec.obs[[i]]<-tmp1
	}



	n.ln_rec_dev<-tmp$rec_dev
	n.ln_mn_rec<-tmp$ln_mn_rec

	ny<-length(n.ln_rec_dev)/3
	rep(1,ny)
	endn<-ny*(1:3)
	stn<-ny*(0:2)+1
	Recest<-list()
	for(s in 1:np){
		rr<-stn[s]:endn[s]
		mu.mn<-n.ln_mn_rec[s]
		mu.sd<-tmp$ln_mn_rec.se[s]

		dev.mn<-tmp$rec_dev[rr]
		dev.se<-tmp$rec_dev.se[rr]

		Recest[[s]]<-data.frame(meanR=(mu.mn+dev.mn),R.se=mu.sd+dev.se)

	}

# ###########################################################
# Now write .dat files
# ###########################################################
	covar_start<-rep(5,np)

		if(file.exists(file.path(path,"rec_files"))){
			txt<-paste("rm -rf ",file.path(path,"rec_files/*"))
			system(txt)
		}else{
			dir.create(file.path(path,"rec_files"))
		}
	k<-1
	#if(file.exists(file.path(DIR_main,"ceattle_recruit-master","rec_files"))){}else{dir.create(file.path(DIR_main,"ceattle_recruit-master","rec_files"))}
	# make covariate data for estimation models
	for(k in 1:np){
		outfile<-paste(path,"/rec_files/",sp[k],"_data.dat",sep="")
		controlfile<-paste(path,"/rec_files/",sp[k],"_control.ctl",sep="")
		txtfile<-paste(path,"/rec_files/",sp[k],"covar_names.txt",sep="")

		if(file.exists(outfile)){}else{file.create(outfile)}
		if(file.exists(controlfile)){}else{file.create(controlfile)}
		if(file.exists(txtfile)){}else{file.create(txtfile)}
		cat("#Species ",file=outfile,append=FALSE,sep="\n")
		cat(paste("#",sp),file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#nyrs : number years for the covariate data ",file=outfile,append=TRUE,sep="\n")
		cat(nyrs[k],file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#ncov : number years for the covariate data ",file=outfile,append=TRUE,sep="\n")
		cat(ncovs,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#years : years for the covariate data ",file=outfile,append=TRUE,sep="\n")
		cat(covuse[[k]]$Year,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")

		cat("#SSB : spawning stock biomass ",file=outfile,append=TRUE,sep="\n")
			cat(covuse[[k]]$SSB,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#REC : Recruitment (can be 1 or 3 years) ",file=outfile,append=TRUE,sep="\n")
			cat(covuse[[k]]$REC,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#Ration : Ration of 1 year old fish in the previous year ",file=outfile,append=TRUE,sep="\n")
			cat(covuse[[k]]$RAT1_lag1,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#mnRat : mean Ration of 1 year old fish ",file=outfile,append=TRUE,sep="\n")
			cat(mean.na(covuse[[k]]$RAT1_lag1),file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#sdRat : sd Ration of 1 year old fish ",file=outfile,append=TRUE,sep="\n")
			cat(sd.na(covuse[[k]]$RAT1_lag1),file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		
		covs<-covuse[[k]][,covar_start[k]:dim(covuse[[k]])[2]]
		Eat_covs<-match(eat_parms,names(covs))
		#Eat_covs<-c(grep("fallZavg",names(covs)),grep("springZavg",names(covs)))
		cat("#covars : covariates of recruitment ",file=outfile,append=TRUE,sep="\n")	
			for(c in 1:ncovs){
				cat(covs[,c],file=outfile,append=TRUE,sep=" ")
				cat("",file=outfile,append=TRUE,sep="\n")
			}
		cat(12345,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")

		#cat("# covariate phase for each covs ",file=controlfile,append=FALSE,sep=" ")
		cat("",file=controlfile,append=FALSE,sep=" ")
		tmp1<-rep(-4,ncovs)
		tmp2<-rep(0,ncovs)
		#tmp<-1:ncovs[k]
		#cat(colnames(covs),file=controlfile,append=TRUE,sep=" ");cat("",file=controlfile,append=TRUE,sep="\n")
		cat(tmp1,file=controlfile,append=TRUE,sep=" ");cat("",file=controlfile,append=TRUE,sep="\n")
		cat(Eat_covs,file=controlfile,append=TRUE,sep=" ");cat("",file=controlfile,append=TRUE,sep="\n")
		cat(tmp2,file=controlfile,append=TRUE,sep=" ");cat("",file=controlfile,append=TRUE,sep="\n")
		#cat(12345,file=controlfile,append=TRUE,sep=" ");cat("",file=controlfile,append=TRUE,sep="\n")

		cat("# covariate phase for each covs ",file=txtfile,append=FALSE,sep="\n")
		cat(colnames(covs),file=txtfile,append=TRUE,sep=" ")
		cat("",file=txtfile,append=TRUE,sep="\n")
		
	}

	# make projection_data.dat data file:

	# Make sure this is consistent with makeROMS_NPZdata.R


	outfile<-paste(path,"/rec_files/spnames.ctl",sep="")
		#cat("# Species names",file=outfile,append=FALSE,sep="\n")
	cat(sp,file=outfile,append=FALSE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
	getwd()
	# print(covars)
	un.covs1<-unlist(strsplit(covars,split="avg"))
	un.covs2<-unlist(strsplit(covars,split="tot"))
	un.covs<-unique(c(un.covs1,un.covs2))

	exclude<-matrix(0,ncovs,ncovs);rownames(exclude)<-colnames(exclude)<-covars
	diag(exclude)<-1
	covmat<-(cor(covuse[[1]][,-(1:4)],covuse[[1]][,-(1:4)]))
	for(n in 1:length(un.covs)){
		rr<-grep(un.covs[n],rownames(exclude))
		exclude[rr,grep(un.covs[n],rownames(exclude))]<-1
	}
	for(n in 1:ncovs){
		exclude[n,(abs(covmat[n,])>.5)]<-1
	}
	
	main.effects<-matrix(0,ncovs,ncovs);rownames(main.effects)<-colnames(main.effects)<-covars
	diag(main.effects)<-1

	mod.num<-1:ncovs

	# make_model_set(parm.list) # parm.list<-c("fallZavg","springZavg","ColdPool","BottomTemp")

	create.modmat<-function(full=c("fallZavg","springZavg","ColdPool","BottomTemp")){
		cc<-which(is.element(covars,full))
		#cc<-unique(grep.list(find=full))
		covars[cc]
		full.mod<-data.frame(matrix(0,1,ncovs));colnames(full.mod)<-covars
		full.mod[1,cc]<-1
		for(n in 1:ncovs){
			tt1<-rep(0,ncovs)
			tt1[cc]<-1
			exclude[n,cc]
			if(sum(exclude[n,cc])==0){
				tt1[n]<-1
				full.mod<-rbind(full.mod,tt1)
			}
		}
		tmpt<-""
		for(n in 1:dim(full.mod)[1]){
			tmpt<-c(tmpt,paste(mod.num[full.mod[n,]>0],collapse="_"))
		}
		rownames(full.mod)<-tmpt[-1]
		return(full.mod)
	}

	# # now create set of interactions
	
	np1<-length(parm.list)
	mod.mat.tmp<-list()
	n<-0;i<-2
	for(i in 2:np1){
		if(i==2)
			mod.mat.tmp<-combn(parm.list,i,simplify=F)
		if(i>2){
			tt<-combn(parm.list,i,simplify=F)
			n1<-length(mod.mat.tmp)
			n2<-length(tt)
			for(ii in 1:n2){
				mod.mat.tmp[[n1+ii]]<-tt[[ii]]
			}
			
		}

	}
	
	np1<-length(parm.list)

	for(i in 2:np1){
		tt<-combn(parm.list,i,simplify=F)
		n1<-length(mod.mat.tmp)
		n2<-length(tt)
		for(ii in 1:n2){
			mod.mat.tmp[[n1+ii]]<-tt[[ii]]
		}
	}

	full.mat<-main.effects;rownames(full.mat)<-mod.num
	for(i in 1:length(mod.mat.tmp)){
		tt<-create.modmat(mod.mat.tmp[[i]])
		tt1<-which(rownames(tt)%in%rownames(full.mat)!=TRUE)
		if(length(tt1)>0){
			full.mat<-rbind(full.mat,tt[tt1,])
		}else{
			#skip
		}
		
		if(any(unlist(strsplit(rownames(full.mat),spli="_"))=="91")){
			message(paste("this is where 91 shows up, i = ",i))
		}

	}
	full.mat.names<-rownames(full.mat)

	if(any(table(full.mat.names)>1)){
		tt1<-which(table(full.mat.names)>1)
		tmpmat<-full.mat
		tmpm.names<-full.mat.names
		for(iii in 1:length(tt1)){
			rmc<-which(tmpm.names==names(tt1)[iii])[-1]
			tmpmat<-tmpmat[-rmc,]
			tmpm.names<-tmpm.names[-rmc]
		}
		full.mat<-tmpmat
		full.mat.names<-tmpm.names
	}

	mod.names<-"remove"
	for(n in 1:dim(main.effects)[1]){
		mod.names<-c(mod.names,paste(mod.num[main.effects[n,]>0],collapse="_"))
	}

	tmpfile<-file.path(DIR_main,rec_path,rec_cntl)
	if(file.exists(tmpfile)){}else{file.create(tmpfile)}
	write.table(t(c("#",rownames(full.mat))),row.names=FALSE,col.names=FALSE,file=tmpfile)
	write.table(t(c("#",colnames(full.mat))),row.names=FALSE,col.names=FALSE,file=tmpfile,append=TRUE)
	write.table(full.mat,row.names=FALSE,col.names=FALSE,file=tmpfile,append=TRUE)


# ###########################################################
# Now plot covariates
# ###########################################################
	
	# ncovs
	mods<-data.frame(matrix(0,ncovs,ncovs));rownames(mods)<-colnames(mods)<-covars
	diag(mods)<-1
	mods[,which(names(mods)=="fallZavg")]<-mods[,which(names(mods)=="fallZavg")]+1
	# apply(mods,1,max.na)

########################################################################################
########################################################################################
## NOW RUN MODEL FIT
########################################################################################
########################################################################################
		#/Users/kkari/Dropbox/AFSC_models/CEATTLE/CEATTLE_outputs/CEATTLE_newest/ceattle_0  
		#/Users/kkari/Dropbox/AFSC_models/CEATTLE/CEATTLE_outputs/CEATTLE_newest/  ..
		#/Users/kkari/Dropbox/AFSC_models/CEATTLE/CEATTLE_outputs/ ../..
		#/Users/kkari/Dropbox/AFSC_models/CEATTLE ../../../CEATTLE-master/ceattle_recruit-master/

	system("echo  \"\\ # testing bash #\"> test_bash.csv")
	 
	system(paste("rm -rf ",file.path(DIR_main,rec_path,"rec_files"),sep=""))
	system(paste("cp -r rec_files ",file.path(DIR_main,rec_path,"/"),sep=""))
	# system(paste("ls ",file.path(DIR_main,rec_path,"rec_files"),sep=""))

	
	# /Users/kkari/Dropbox/AFSC_models/CEATTLE/CEATTLE-master/ceattle_recruit-master/rec_files
	setwd(file.path(DIR_main,rec_path))

	# Run recruitment analysis for all possible combinations of 1 covariate
	#if(run_models==1)
	system("./run_ceattle_recruit")

########################################################################################
########################################################################################
## NOW use AIC to analyze results 
########################################################################################
########################################################################################
	# setwd("/Users/kkari/Dropbox/CEATTLE-master/ceattle_recruit-master")

	predictRS<-function(parms,B=SSB[[1]],ncov=1,covs=NULL,type="Ricker"){	
		logR_hat<-R_hat<-rep(0,length(B))
		if(type=="Ricker"){
			
			for(i in 1:length(B)){
				if(is.null(covs)){
					logR_hat[i]= (log(exp(parms[1])*B[i]) -exp(parms[2])*B[i])
				}else{
					covartmp<-rep(0,length(B))
					for(c in 1:ncov){
						covartmp[i]<-covartmp[i]+parms[2+c]*covs[i]
					}
					logR_hat[i]= (log(exp(parms[1])*B[i]) -exp(parms[2])*B[i]+covartmp[i])
				}
			}
		}
		if(type=="BLM"){
			aa_c<-(parms[1])
	    	bb_c<-(parms[2])
	    	for(i in 1:length(B)){
				if(is.null(covs)){
					logR_hat[i]= (aa_c+bb_c*B[i])
				}else{
					covartmp<-rep(0,length(B))
					for(c in 1:ncov){
						covartmp[i]<-covartmp[i]+parms[2+c]*covs[i]
					}
					logR_hat[i]= (aa_c+bb_c*log(B[i])+covartmp[i])
				}
			}
	    }
	    if(type=="LM"){
			aa_c<-(parms[1])
	    	for(i in 1:length(B)){
				if(is.null(covs)){
					logR_hat[i]= (aa_c)
				}else{
					covartmp<-rep(0,length(B))
					for(c in 1:ncov){
						covartmp[i]<-covartmp[i]+parms[1+c]*covs[i]
					}
					logR_hat[i]= (aa_c+covartmp[i])
				}
			}
	    }
		if(type=="BH"){
	       aa_c<-exp(parms[1])
	       bb_c<-exp(parms[2])
	      #alpha=(B0/R0)*(1-((z-0.2)/(0.8*z)))
	      #beta = (z-0.2)/(0.8*z*R0) 
	      for(i in 1:length(B))
	          R_hat[i]=SSB(i)/(aa_c+(bb_c*B[i]))
	      logR_hat=log(R_hat)
		}
		return(logR_hat)
	}
	aicfun<-function(npar,LL,n,type=2){
		if(type==1)
		return((2*npar-2*(LL)))
		if(type==2)
		return((2*npar-2*(LL))+(2*npar*(npar+1))/(n-npar-1))
	}

	AICselection<-function(LL,npar,n,mnames1=legend.nm,R2=R2[,s],type2=2,HESS){
		nn<-length(LL)
		tmp1<-data.frame(LL=LL,npar=npar,lab=1:nn,R2=R2)
	   	tmp1$names<-mnames1
	    tmp1$aicc<-tmp1$aicc_marg<-rep(0,nn)
	    for(i in 1:nn){
		    tmp1$aicc[i]<-aicfun(npar[i],LL[i],n[i],type=type2)
		 	#tmp1$aicc_marg[i]<-GET_HESS_AIC(HESS[[i]],npar=HESS[[i]]$num.pars,NLL=-1*LL[i])[[1]]
		 	tmp1$aicc_marg[i]<-GET_HESS_AIC(HESS[[i]],npar=npar[i],NLL=-1*LL[i])[[1]]
		}
		    #tmp1$aicc[i]<-(2*npar[i]-2*(LL[i]))+(2*npar[i]*(npar[i]+1))/(n[i]-npar[i]-1)

	    tmp1$deltaAIC<-(tmp1$aicc-min(tmp1$aicc))
	    tmp1$AICweight<-exp(-0.5*tmp1$deltaAIC)
	    tmp1$rank<-rank(tmp1$aicc)
	    tmp1<-tmp1[order(tmp1$aicc),]
	    
	    tmp1$AICw_std<-tmp1$AICweight/sum(tmp1$AICweight)
	    tmp1$cumlAIC<-cumsum(tmp1$AICw_std)
	    cutoff<-which(tmp1$cumlAIC>0.95)[1];if(is.na(cutoff)) cutoff<-nn
	    cutoff2<-which(tmp1$deltaAIC>2)[1];if(is.na(cutoff2)) cutoff2<-nn
	    cutoff4<-which(tmp1$deltaAIC>4)[1];if(is.na(cutoff4)) cutoff4<-nn

	    
	    t1<-rep("",nn);t1[1:cutoff]<-"o"
	    t2<-rep("",nn);t2[1:cutoff2]<-"*"
	    t4<-rep("",nn);t4[1:cutoff4]<-"*"

	    tmp1$topSet<-paste(t2,t4,t1,sep="")

	    tmp1$deltaAIC_marg<-(tmp1$aicc_marg-min(tmp1$aicc_marg))
	    tmp1$AICweight_marg<-exp(-0.5*tmp1$deltaAIC_marg)
	    tmp1$rank_marg<-rank(tmp1$aicc_marg)
	    #tmp1<-tmp1[order(tmp1$aicc),]
	    
	    tmp1$AICw_std_marg<-tmp1$AICweight_marg/sum(tmp1$AICweight_marg)
	    tmp1$cumlAIC_marg<-cumsum(tmp1$AICw_std_marg)
	    cutoff_marg<-which(tmp1$cumlAIC_marg>0.95)[1];if(is.na(cutoff_marg)) cutoff_marg<-nn
	    cutoff2_marg<-which(tmp1$deltaAIC_marg>2)[1];if(is.na(cutoff2_marg)) cutoff2_marg<-nn
	    cutoff4_marg<-which(tmp1$deltaAIC_marg>4)[1];if(is.na(cutoff4_marg)) cutoff4_marg<-nn
	    
	    t1_marg<-rep("",nn);t1_marg[1:cutoff_marg]<-"o"
	    t2_marg<-rep("",nn);t2_marg[1:cutoff2_marg]<-"*"
	    t4_marg<-rep("",nn);t4_marg[1:cutoff4_marg]<-"*"

	    tmp1$topSet_marg<-paste(t2_marg,t4_marg,t1_marg,sep="")
	    #tmp1$topSet<-rep("",nn);tmp1$topSet[1:cutoff]<-"*"

	    return(tmp1)
	}



	read.dat<-function(fn,nm)
	{
	    ifile <- scan(fn, what = "character", flush = T, blank.lines.skip = T, quiet = T)
	    iflex <- which(is.na(ifile))
	    idx<--999
	    for(i in 1:length(ifile)){
	    	if(is.na(as.numeric(ifile[i])))
	    	idx <- c(idx,i)
	    }
	    idx<-idx[-1]
	    idy<-idx
	    datnum <- which(idx == FALSE)
	    labnum <- which(idx == TRUE)
	    vnam <- ifile[idx]
	    tmp <- rep(0, length(vnam))
	    tt <- strsplit(vnam, split = "#")
	    for (i in 1:length(tmp)) if (is.na(as.numeric(tt[[i]][2]))) 
	        tmp[i] <- 1
	    vnam2 <- vnam[tmp == 1]
	    tt <- strsplit(vnam2, split = "#")
	    tmp <- rep(0, length(vnam2))
	    for (i in 1:length(tmp)) if (length(tt[[i]]) > 1) 
	        tmp[i] <- 1
	    vnam2 <- vnam2[tmp == 1]
	    labnum <- match(vnam2, ifile)
	    ifilet <- strsplit(ifile, split = "#")
	    vnamt <- strsplit(vnam2, split = "#")
	    for (i in 1:length(vnam2)) vnam2[i] <- vnamt[[i]][2]
	    for (i in 1:length(ifile)) ifile[i] <- ifilet[[i]][length(ifilet[[i]])]
	    vnam2 <- na.omit(vnam2)
	    nv <- length(vnam2)
	    A <- list()
	    ir <- 0
	    vnam <- vnam2
	    ii <- which(vnam == nm | vnam == paste(nm, ":", sep = "") | 
	        paste(vnam, ";", sep = "") == nm)
	    if (length(ii) == 0) 
	        stop(paste(nm, " >> name of object in the ", ADMBfilename, 
	            ".dat file does not match ", ADMBfilename, ".tpl file", 
	            sep = ""))
	    ir <- match(vnam[ii], ifile)
	    if (ii != nv) {
	        irr <- match(vnam[ii + 1], ifile)
	    }
	    else {
	        irr <- length(ifile) + 1
	    }
	    ans <- -999
	    which(is.na(as.numeric(ifile[ir:irr])) == FALSE)
	    irn <- ir + which(is.na(as.numeric(ifile[ir:irr])) == FALSE) - 
	        1
	    for (i in 1:length(irn)) {
	        tt <- as.double(scan(fn, skip = irn[i] - 1, nlines = 1, 
	            quiet = TRUE, what = ""))
	        ans <- c(ans, as.numeric(na.omit(tt)))
	    }
	    ans <- ans[-1]
	    return(ans)
	}
	RecHat<-function(coefs=best_model,coefs.se=best_model.SD/sqrt(nyrs2-1),SE=FALSE,CI=1.96){
		logR_hat<-matrix(0,nsppRS,nyrs2)
		aa_c<-exp(coefs$log_aa_c)
		bb_c<-exp(coefs$log_bb_c)
		spr_Z<-(coefs$spr_Z)
		fall_Z<-(coefs$fall_Z)
		TempCoef<-(coefs$TempCoef)
		 for (s in 1:nspp)
		 {
		    logR_hat[s,1]=logR_obs[s,1]
		    for(i in 2:nyrs2)
		    {
		      Eating=0
		      Eating = Total_ration[s,i]/fall_zoop[i]
		      logR_hat[s,i]= (log(aa_c[s]*SSB_rs[s,i-1]) -bb_c[s]*SSB_rs[s,i-1]+spr_Z[s]*spr_zoop[i] - fall_Z[s]*Eating+TempCoef[s]*bottomT[i])
		    }	

		  }	
		  if(SE==TRUE){
		  	logR_hat.plus<-matrix(0,nsppRS,nyrs2)
			aa_c.e<-exp(coefs$log_aa_c+CI*coefs.se$log_aa_c)
			bb_c.e<-exp(coefs$log_bb_c+CI*coefs.se$log_bb_c)
			spr_Z.e<-(coefs$spr_Z+CI*coefs.se$spr_Z)
			fall_Z.e<-(coefs$fall_Z+CI*coefs.se$fall_Z)
			TempCoef.e<-(coefs$TempCoef+CI*coefs.se$TempCoef)
			for (s in 1:nspp)
			 {
			    logR_hat[s,1]=logR_obs[s,1]
			    for(i in 2:nyrs2)
			    {
			      Eating=0
			      Eating = Total_ration[s,i]/fall_zoop[i]
			      logR_hat.plus[s,i]= (log(aa_c.e[s]*SSB_rs[s,i-1]) -bb_c.e[s]*SSB_rs[s,i-1]+spr_Z.e[s]*spr_zoop[i] - fall_Z.e[s]*Eating+TempCoef.e[s]*bottomT[i])		    
			    }	

			  }	
			logR_hat.minus<-matrix(0,nsppRS,nyrs2)
			aa_c.e<-exp(coefs$log_aa_c-CI*coefs.se$log_aa_c)
			bb_c.e<-exp(coefs$log_bb_c-CI*coefs.se$log_bb_c)
			spr_Z.e<-(coefs$spr_Z-CI*coefs.se$spr_Z)
			fall_Z.e<-(coefs$fall_Z-CI*coefs.se$fall_Z)
			TempCoef.e<-(coefs$TempCoef-CI*coefs.se$TempCoef)
			 for (s in 1:nspp)
			 {
			    logR_hat[s,1]=logR_obs[s,1]
			    for(i in 2:nyrs2)
			    {
			      Eating=0
			      Eating = Total_ration[s,i]/fall_zoop[i]
			      logR_hat.minus[s,i]= (log(aa_c.e[s]*SSB_rs[s,i-1]) -bb_c.e[s]*SSB_rs[s,i-1]+spr_Z.e[s]*spr_zoop[i] - fall_Z.e[s]*Eating+TempCoef.e[s]*bottomT[i])		    
			    }	

			  }	 
			  return(list(logR_hat=logR_hat,logR_hat.plus=logR_hat.plus,logR_hat.minus=logR_hat.minus))
		  	}else{
		  		return(logR_hat)
		  	}
		  
	}	
	make_Topdat<-function(outfile){
		if(file.exists(outfile)){}else{file.create(outfile)}
		nt<-dim(values[[1]])
		tmpv<-data.frame(sp1=values[[1]][mins[1],])
		for(s in 2:nspp)
			eval(parse(text=paste("tmpv<-data.frame(tmpv,sp",s,"=values[[s]][mins[s],])",sep="")))
		tmpv<-data.frame(t(tmpv))
		cat("#sigma",file=outfile,append=FALSE,sep="\n")
		cat(exp(tmpv$logsigma),file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#aa_c",file=outfile,append=TRUE,sep="\n")
		cat(exp(tmpv$log_aa_c),file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#bb_c",file=outfile,append=TRUE,sep="\n")
		cat(exp(tmpv$log_bb_c),file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#spr_Z",file=outfile,append=TRUE,sep="\n")
		cat((tmpv$spr_Z),file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#fall_Z",file=outfile,append=TRUE,sep="\n")
		cat((tmpv$fall_Z),file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#TempCoef",file=outfile,append=TRUE,sep="\n")
		cat((tmpv$TempCoef),file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#f",file=outfile,append=TRUE,sep="\n")
		cat(c(-9,-9,-9),file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#obj_fun",file=outfile,append=TRUE,sep="\n")
		cat(-999,file=outfile,append=TRUE,sep="\n")
	}

	make_AICtable<-function(outfile){
		if(file.exists(outfile)){}else{file.create(outfile)}
		cat("_________________________________________________",file=outfile,append=FALSE,sep="\n")
		cat("AIC table output for RS models",file=outfile,append=TRUE,sep=" ")
		cat("",file=outfile,append=TRUE,sep="\n")
		cat("_________________________________________________",file=outfile,append=TRUE,sep="\n")
		for(sp in 1:nspp)
		{
			 cat(paste("species_",sp,sep=""),file=outfile,append=TRUE,sep="\n")
			 cat(c("model",names(AICtable[[sp]])),file=outfile,append=TRUE,sep=" ")
			 cat("",file=outfile,append=TRUE,sep="\n")
			 nn<-dim( AICtable[[sp]])
			 for(r in 1:nn[1]){
			 	cat(c(rownames(AICtable[[sp]])[r]," "),file=outfile,append=TRUE,sep=" ")
			 	for(c in 1:nn[2]){
			 		cat(c(AICtable[[sp]][r,c]," "),file=outfile,append=TRUE,sep=" ")
					
			 	}	
			 	cat("",file=outfile,append=TRUE,sep="\n")
			 }

		}
	}
	make_AICtableofPar<-function(outfile){
		if(file.exists(outfile)){}else{file.create(outfile)}
		cat("_________________________________________________",file=outfile,append=FALSE,sep="\n")
		cat("mean model values for RS models",file=outfile,append=TRUE,sep=" ")
		cat("",file=outfile,append=TRUE,sep="\n")
		cat("_________________________________________________",file=outfile,append=TRUE,sep="\n")
		for(sp in 1:nspp)
		{
			 cat(paste("species_",sp,sep=""),file=outfile,append=TRUE,sep="\n")
			 cat(c("model",colnames(values[[sp]])),file=outfile,append=TRUE,sep=" ")
			 cat("",file=outfile,append=TRUE,sep="\n")
			 nn<-dim( values[[sp]])
			 for(r in 1:nn[1]){
			 	cat(c(rownames(values[[sp]])[r]," "),file=outfile,append=TRUE,sep=" ")
			 	for(c in 1:nn[2]){
			 		cat(c(values[[sp]][r,c]," "),file=outfile,append=TRUE,sep=" ")
					
			 	}	
			 	cat("",file=outfile,append=TRUE,sep="\n")
			 }

		}
		cat("_________________________________________________",file=outfile,append=TRUE,sep="\n")
		cat("Standard deviation of model values for RS models",file=outfile,append=TRUE,sep=" ")
		cat("",file=outfile,append=TRUE,sep="\n")
		cat("_________________________________________________",file=outfile,append=TRUE,sep="\n")
		for(sp in 1:nspp)
		{
			 cat(paste("species_",sp,sep=""),file=outfile,append=TRUE,sep="\n")
			 cat(c("model",colnames(std[[sp]])),file=outfile,append=TRUE,sep=" ")
			 cat("",file=outfile,append=TRUE,sep="\n")
			 nn<-dim( std[[sp]])
			 for(r in 1:nn[1]){
			 	cat(c(rownames(std[[sp]])[r]," "),file=outfile,append=TRUE,sep=" ")
			 	for(c in 1:nn[2]){
			 		cat(c(std[[sp]][r,c]," "),file=outfile,append=TRUE,sep=" ")
					
			 	}	
			 	cat("",file=outfile,append=TRUE,sep="\n")
			 }

		}
	}
	get.admb.hes <- function(flnm){

		#source code - use this function.
		 #wd.old <- getwd(); on.exit(setwd(wd.old))
		 #setwd(model.path)
		# filename <- file("admodel.hes", "rb")
		 filename <- file (flnm,"rb")
		 on.exit(close(filename), add=TRUE)
		 num.pars <- readBin(filename, "integer", 1)
		 hes.vec <- readBin(filename, "numeric", num.pars^2)
		 hes <- matrix(hes.vec, ncol=num.pars, nrow=num.pars)
		 hybrid_bounded_flag <- readBin(filename, "integer", 1)
		 scale <- readBin(filename, "numeric", num.pars)
		 result <- list(num.pars=num.pars, hes=hes,
		                hybrid_bounded_flag=hybrid_bounded_flag, scale=scale)
		 return(result)
	}
	GET_HESS_AIC<-function(HESS,NLL,npar){
		#Based on Jim Thorson code via ingrid
		require("corpcor")

		#this pulls out the hessian and then transforms it into parameter space
		#HESS = get.admb.hes(flnm)
		HESS=HESS
		NLL=NLL
		num.pars=npar
		# Calculate Hessian
		cov <- pseudoinverse(HESS$hes)
		#sqrt(diag(HESS$hes))
		scale <- HESS$scale
		cov.bounded <- cov*(scale %o% scale)
		Hess = pseudoinverse(cov.bounded)
		

		#NLL=4759.74 #this is the objective function value
		#num.pars=120
		LnDet = determinant(Hess, logarithm=TRUE)$modulus[[1]]
		Ln_Integral = log(2*pi) + (-1/2)*sum(LnDet) + -1*NLL #this is the MARGINAL likelihood
		AIC = -2*Ln_Integral + 2*num.pars
		return(AIC)
	}
	lookup_name<-function(sp_num=1,mod_name=tmpn,cov_names=cov_names){
		btxt<-rep("",length(mod_name))
		for(x in 1:length(mod_name)){
			ss=sp_num
			tmpp<-unlist(strsplit(mod_name[x],split=paste("RS",ss,"_",sep="")))[-1]
			tmpp<-unlist(strsplit(tmpp,split="_"))
			ttt<-which(is.na(as.numeric(tmpp)))
			if(length(ttt)>0){
				save.txt<-tmpp[ttt]
				if(length(tmpp)==1){
					nn<-0
				}else{
					nn<-as.numeric(tmpp[-ttt])
				}
			}else{
				save.txt<-""
				nn<-as.numeric(tmpp)
			}
			
			if(nn==0){
				txt.tmp<-paste(c("no covars",save.txt),collapse=" ",sep="")

			}else{
				txt.tmp<-paste(c(cov_names[nn],save.txt),collapse=" ",sep="")

			}
			btxt[x]<-txt.tmp

			
			# 	if(cov_num[x]>0){
			# 		btxt[x]<-tmp<-colnames(covuse[[ss]][,(covar_start[ss]-1+1:ncovs)])[cov_num[x]]
			# 		#tmp<-colnames(covars[[ss]])[covar_start[ss]-1+1:ncovs[ss]][cov_num]
			# 		#btxt[x]<-colnames(covars[[ss]])[covar_start[ss]-1+1:ncovs[ss]][cov_num[x]]
			# 	}else{
			# 		btxt[x]<-"no covars"
			# 	}
		}
		return(btxt)
	}

	#setwd("MSM_RS")
	# read in log file
	#spp<-unique(obs$spp)
	# aicfun(npar=2,LL=-15,n=2,type=2)
	#aicfun(npar=npart,LL=LLtest,n=ntest,type=1)

	spp<-sp
	nspp<-np
	AICtable<-covars_used<-smry_RS<-list()
	null.n<-fallZ.n<-SST.n<-BT.n<-fullset.n<-fullsetRat.n<-topR2n<-rep(0,nspp)
	R2_covn<-list()
	TopR2_Ricker.txt<-TopAIC_Ricker.txt<-TopAIC.txt<-TopR2.txt<-rep("",nspp)
	#DIR_main<-getwd()
	root2<-getwd()
	root2<-file.path(DIR_main,rec_path)
	# print(root2)
	topn<-rep(0,nspp)
	cov_names<-as.character(read.csv(file=file.path(DIR_main,rec_path,rec_cntl),nrows=1,skip=1,header=FALSE, sep=" ",colClasses="character")[-1][1,])
	mod_names<-as.character(read.csv(file=file.path(DIR_main,rec_path,rec_cntl),nrows=1,header=FALSE, sep=" ",colClasses="character")[-1])
	tmpdir<-dir("RS_fits"); datlist<-tmpdir[grep(".dat",tmpdir)]; datlist<-datlist[-grep("rs_data_output_",datlist)];datlist<-datlist[-grep("rs_data4CEATTLEcopy_",datlist)];datlist<-unlist(strsplit(datlist,split=".dat"))
	tmpdir<-dir("RS_fits");stdlist<-unlist(strsplit(unlist(strsplit(tmpdir[grep(".std",tmpdir)],split=".std")),split="ceattle_recruit_"));stdlist<-stdlist[seq(2,length(stdlist),2)]
	
	converge.fail<-""
	if(length(datlist)!=length(stdlist)){
		cat("some models did not converge:")
		cat("\n")
		converge.fail<-setdiff(datlist, stdlist)
		cat(paste(converge.fail),sep="\n")

	}else{
		cat("\n")
		cat("_______ ")
		cat(paste("all", length(datlist),"recruitment models converged"))
		cat(" _______")
		cat("\n")
	}

	if(file.exists("convergeFail_log.csv")){
		#
	}else{file.create("convergeFail_log.csv")}
	cat(converge.fail,file="convergeFail_log.csv",append=FALSE,sep="\n")

	ss<-1
	prev<-getwd()  #"/Users/kholsman/GitHub/CEATTLE/src/ceattle_recruit-master"

	for(ss in 1:nspp){
		main_name<-paste("RS",ss,sep="")
		log<-read.csv(paste(root2,"/",main_name,"_log.csv",sep=""),sep=" ",skip=1,header=FALSE)
		ncovs2<-dim(log)[2]-1
		nmods<-dim(log)[1]
		colnames(log)<-c("model",paste("cov",1:ncovs2,sep=""))
		mnames.all<-as.character(log[,1])
		setwd(file.path(root2,"RS_fits"))
		mnames<-mnames.all
		if(converge.fail!=""){
			tt<-match(converge.fail,mnames)
			if(is.na(tt)==FALSE){
				mnames<-mnames[-match(converge.fail,mnames)]
			}
		}
		###!!!!!!!!!!!!!!!!!!!!!!!
		###!!!!!!!!!!!!!!!!!!!!!!!
		# for now remove the Eat function models - they don't appear to work correctly in projections
		###!!!!!!!!!!!!!!!!!!!!!!!
		###!!!!!!!!!!!!!!!!!!!!!!!
		if(length(grep("Eat",mnames))>0) mnames<-mnames[-grep("Eat",mnames)]  
		###!!!!!!!!!!!!!!!!!!!!!!!
		

		tt<-read.csv(paste("ceattle_recruit",mnames[1],".rep",sep=""),sep=" ")[,1:5]
		obs<-data.frame(spp=tt[,1],year=tt[,2],SSB=tt[,3],Rec=tt[,4])
		yrs<-covuse[[ss]]$Year
		nyrs<-length(yrs)

		## FIND TOP MODELS
		tt<-read.csv(paste("ceattle_recruit",mnames[2],".rep",sep=""),sep=" ")[,1:5]
		tt2<-read.csv(paste("ceattle_recruit",mnames[3],".rep",sep=""),sep=" ")[,1:5]
		#tt3<-read.csv(paste("msm_rs_output",mnames[87],".rep",sep=""),sep=" ")[,1:5]
		npar<-rep(999,length(mnames))
		R2<-rep(999,length(mnames))
		names(R2)<-as.character(mnames)
		LL<-n<-R2_2<-R2
		#for(ss in 1:nspp){
		tt<-read.csv(paste("ceattle_recruit",mnames[2],".rep",sep=""),sep=" ")[,1:5]
		tmpm<-lm(log(Rec)~1,data=data.frame(Rec=tt$R_obs))
		LL[1]<-logLik(tmpm)[1]
		npar[1]<-2
		n[1]<-length(tt)
		R2[1]<-as.numeric(summary(tmpm)[8])#cor(tt$R_obs[tt$spp==ss],as.numeric(predict(tmpm))) #cor(subdat[,4],subdat[,5])
		#}
		i<-1
		s<-1
		i<-3
		# aicfun(npar[1],LL[1],n[1],1)
		i<-1
		HESS<-list()
		for(i in 1:(length(mnames))){
			ii<-i
			tt0<-read.csv(paste("ceattle_recruit",mnames[ii],".rep",sep=""),sep=" ")[,1:5]
			LL[i]<-read.dat(paste("rs_data_output_",mnames[ii],".dat",sep=""),nm="f")	
			stdtmp<-read.csv(paste("ceattle_recruit_",mnames[ii],".std",sep=""),sep="")	
			tt<-tt0	
			R2[i]<-cor(tt[,4],tt[,5])^2
			n[i]<-length(tt$spp)
			npar[i]<-dim(stdtmp)[1]#/nspp
			HESS[[i]]<-get.admb.hes(paste0("admodel_",mnames[ii],".hes"))
		}	
		AICtable[[ss]]<-AICselection(LL=(-1*LL),npar=npar,n=n,mnames1=mnames,R2=R2,HESS=HESS)

		null.n[ss]<-grep("_0_LM",AICtable[[ss]]$names)
		TopAIC.txt[ss]<-AICtable[[ss]]$names[1]
		topR2n[ss]<-which(AICtable[[ss]]$R2==max.na(AICtable[[ss]]$R2))[1]
		TopR2.txt[ss]<-AICtable[[ss]]$names[topR2n[ss]]
		TopAIC_Ricker.txt[ss]<-AICtable[[ss]]$names[-c(grep("LM",AICtable[[ss]]$names),grep("BH",AICtable[[ss]]$names))][1]
		mm<-max(AICtable[[ss]]$R2[-c(grep("LM",AICtable[[ss]]$names),grep("BH",AICtable[[ss]]$names))])
		TopR2_Ricker.txt[ss]<-AICtable[[ss]]$names[-c(grep("LM",AICtable[[ss]]$names),grep("BH",AICtable[[ss]]$names))][which(AICtable[[ss]]$R2[-c(grep("LM",AICtable[[ss]]$names),grep("BH",AICtable[[ss]]$names))]==mm)]

		###!!!!!!!!!!!!!!!!!!!!!!!
		###!!!!!!!!!!!!!!!!!!!!!!!
		# for now ignore non-ricker models
		###!!!!!!!!!!!!!!!!!!!!!!!
		###!!!!!!!!!!!!!!!!!!!!!!!
		# TopAIC.txt[ss]<-TopAIC_Ricker.txt[ss]
		# TopR2.txt[ss]<-TopR2_Ricker.txt[ss]

			
		tt0<-read.csv(paste("ceattle_recruit",TopR2.txt[ss],".rep",sep=""),sep=" ")[,1:5]		
		tmp<-data.frame(tt0[,1:4])
		tt0<-read.csv(paste("ceattle_recruit",main_name,"_0_LM",".rep",sep=""),sep=" ")[,1:5]
		tmp$mnRec<-tt0[,5]
		tt0<-read.csv(paste("ceattle_recruit",main_name,"_0",".rep",sep=""),sep=" ")[,1:5]
		tmp$mnRS_Rec<-tt0[,5]
		tt0<-read.csv(paste("ceattle_recruit",TopR2.txt[ss],".rep",sep=""),sep=" ")[,1:5]
		tmp$TopR2_Rec<-tt0[,5]
		tt0<-read.csv(paste("ceattle_recruit",TopAIC.txt[ss],".rep",sep=""),sep=" ")[,1:5]
		tmp$TopAIC_Rec<-tt0[,5]
		tt0<-read.csv(paste("ceattle_recruit",main_name,"_0_BLM",".rep",sep=""),sep=" ")[,1:5]
		tmp$mnRecwB<-tt0[,5]

		setwd(DIR_main)
		#covars[[ss]]<-read.csv(paste(sp[ss],".csv",sep=""),sep=",",header=FALSE,skip=1)
		covars_used[[ss]]<-covuse[[ss]][,(covar_start[ss]-1+1:ncovs)]
		colnames(covars_used[[ss]])<-colnames(covuse[[ss]][,(covar_start[ss]-1+1:ncovs)])#scan(paste(sp[ss],".csv",sep=""),sep=",",what=character(),nlines=1)
		ttt<-unlist(strsplit(rownames(AICtable[[ss]])[topR2n[ss]],split="_"))

		if(any(which(is.na(as.numeric(ttt))))){
			R2_covn[[ss]]<-as.numeric(ttt[-which(is.na(as.numeric(ttt)))])
		}else{
			R2_covn[[ss]]<-as.numeric(ttt)
		}
		tmptxt<-substr(AICtable[[ss]]$names,5,100)
		tmptxt[tmptxt=="BH"]<-"0_BH"
		nulln<-grep("LM",tmptxt)
		numbs<-as.numeric(unlist(lapply(strsplit(tmptxt,split="_"),'[[',1)))
		mod_name<-rownames(AICtable[[ss]])
		btxt<-rep("",length(mod_name))
		for(x in 1:length(mod_name)){
			tmpp<-unlist(strsplit(mod_name[x],split=paste("RS",ss,"_",sep="")))[-1]
			tmpp<-unlist(strsplit(tmpp,split="_"))

			if(any(is.na(as.numeric(tmpp)))){
				ttt<-which(is.na(as.numeric(tmpp)))
			
				save.txt<-tmpp[ttt]
				if(length(tmpp)==1){
					nn<-0
				}else{
					nn<-as.numeric(tmpp[-ttt])
				}
			}else{
				save.txt<-""
				nn<-as.numeric(tmpp)
			}
			
			if(nn[1]==0){
				txt.tmp<-paste(c("no_covars",save.txt),collapse="_",sep="")

			}else{
				txt.tmp<-paste(c(cov_names[nn],save.txt),collapse="_",sep="")

			}
			btxt[x]<-txt.tmp
		}

		AICtable[[ss]]$model<-btxt #lookup_name(sp_num=ss,mod_name=tpmn)
		mm<-1
		subn<-which(AICtable[[ss]]$topSet=="**o")
		sub<-AICtable[[ss]][subn,]
		topn[ss]<-length(subn)
		covarnum<-rep(0,topn[ss])

		for(mm in 1:topn[ss]){
			tmp_name<-sub$names[mm]
			covarnum[mm]<-as.numeric(strsplit(tmp_name,"_")[[1]][2])
			if(is.na(covarnum[mm])) covarnum[mm]<-0
			tt0<-read.csv(paste(file.path(DIR_main,rec_path),"/RS_fits/ceattle_recruit",tmp_name,".rep",sep=""),sep=" ")[,1:5]
			tmptype<-""
			if(length(grep("LM",tmp_name))==1){
				if(length(grep("BLM",tmp_name))==1){tmptype<-"BLM_"}else{tmptype<-"LM_"}
			}else{
				if(length(grep("BH",tmp_name))==1){tmptype<-"BH_"}else{tmptype<-"RCKR_"}
			}
			if(sub$model[mm]=="no_covars_"){
				eval(parse(text=paste0("tmp$RhatTop_",tmptype,"nocovs<-tt0[,5]")))
			}else{
				eval(parse(text=paste0("tmp$RhatTop_",tmptype,sub$model[mm],"<-tt0[,5]")))
			}
		}

		ttxt<-paste(colnames(covuse[[ss]][,(covar_start[ss]-1+1:ncovs)])[R2_covn[[ss]]],collapse="AND",sep="")
		eval(parse(text=paste("tmp$R2cov_",ttxt,"<-apply(data.frame(covuse[[ss]][,(covar_start[ss]-1+1:ncovs)][,R2_covn[[ss]]]),1,sum)",sep="")))
		tmptt1<-strsplit(AICtable[[ss]]$names[1],"_")[[1]]
		tmptt<-as.numeric(strsplit(AICtable[[ss]]$names[1],"_")[[1]][2])
		if(length(tmptt1)==2){
			if(is.numeric(tmptt)){
				nullt<-TRUE
				if(tmptt>0){
					nullt<-FALSE
				}

			}else{
				nullt<-TRUE
			}
		}else{
			nullt<-FALSE
		}
		ttt<-as.numeric(strsplit(AICtable[[ss]]$names[1],"_")[[1]])

		if(any(which(is.na(ttt))))
			ttt<-ttt[-which(is.na(ttt))]

			if(nullt){
				ttxt<-"null"
				eval(parse(text=paste("tmp$TopAICcov_",ttxt,"<-NA",sep="")))
			}else{
				ttxt<-paste(colnames(covuse[[ss]][,(covar_start[ss]-1+1:ncovs)])[ttt],collapse="AND",sep="")
				#ttxt<-colnames(covars[[ss]])[covar_start[ss]-1+1:ncovs[ss]][ttt]
				#	ttxt<-lookup_name(sp_num=ss,cov_num=ttt)
				if(length(ttt)>1){
					eval(parse(text=paste("tmp$TopAICcov_",ttxt,"<-apply(covuse[[ss]][,(covar_start[ss]-1+1:ncovs)][,ttt],1,sum)",sep="")))
				}else{
					eval(parse(text=paste("tmp$TopAICcov_",ttxt,"<-covuse[[ss]][,(covar_start[ss]-1+1:ncovs)][,ttt]",sep="")))
				}

			}
		mm<-1
		for(mm in 1:topn[ss]){
			tmp_name<-sub$names[mm]
			rownames(AICtable[[ss]])[mm]

			ttt<-as.numeric(strsplit(AICtable[[ss]]$names[mm],"_")[[1]])
			ttt<-ttt[-which(is.na(ttt))]
			#ttt<-covarnum[mm]
			if(length(ttt)==0){
				ttxt<-"null"
				eval(parse(text=paste("tmp$CovUse_",ttxt,"<-NA",sep="")))
			}else{
				ttxt<-paste(colnames(covuse[[ss]][,(covar_start[ss]-1+1:ncovs)])[ttt],collapse="AND",sep="")
				#ttxt<-colnames(covars[[ss]])[covar_start[ss]-1+1:ncovs[ss]][ttt]
				#	ttxt<-lookup_name(sp_num=ss,cov_num=ttt)
				if(length(ttt)>1){
					eval(parse(text=paste("tmp$CovUse_",ttxt,"<-apply(covuse[[ss]][,(covar_start[ss]-1+1:ncovs)][,ttt],1,sum)",sep="")))
				}else{
					eval(parse(text=paste("tmp$CovUse_",ttxt,"<-covuse[[ss]][,(covar_start[ss]-1+1:ncovs)][,ttt]",sep="")))
				}

			}
		}
		smry_RS[[ss]]<-tmp
		rm(tmp)
	}

	names(smry_RS)<-names(AICtable)<-sp
	# graphics.off()

	ss<-2
	if(MSM_type==2){
		system("rm -rf RecFigs_ceattle_2")
	}
	if(MSM_type==0){
		system("rm -rf RecFigs_ceattle_0")
	}

# ###########################################################
# Now write files for CEATTLE
# ###########################################################

	topR2ricker<-topricker<-topname<-rep("",np)
	for(s in 1:np){
		tmpp<-rownames(AICtable[[s]])
		topname[s]<-tmpp[1]
		rm1<-grep("LM",tmpp)
		rm2<-grep("BH",tmpp)
		topricker[s]<-tmpp[-c(rm1,rm2)][1]

		tmpp<-AICtable[[s]]$names[order(AICtable[[s]]$R2,decreasing=T)]
		rm1<-grep("LM",tmpp)
		rm2<-grep("BH",tmpp)
		topR2ricker[s]<-tmpp[-c(rm1,rm2)][1]
	}


	create_datfile<-function(RSmods=topricker,fl1="rs_data4CEATTLE_TOP.dat"){
		fl<-file.path(DIR_main,rec_path,"fits_4_CEATTLE")
		if(file.exists(fl)){		}else{dir.create(fl)}
		outfile<-file.path(fl,fl1)
		if(file.exists(outfile)){
			txt<-paste("rm -rf ",outfile)
			system(txt)
			file.create(outfile)
		}else{
			file.create(outfile)
		}

		BLM<-LM<-BevH<-ncov<-aa_c<-aa_c_std<-bb_c<-bb_c_std<-sigma<-sigma_std<-rep(0,np)
		rs_parm_std<-rs_parm<-cov_phase2<-cov_type2<-matrix(0,nspp,ncovs)
		for(s in 1:np){
			if(length(grep("BH",RSmods[s]))>0)
				BevH[s]<-1
			
			if(length(grep("LM",RSmods[s]))>0){
				if(length(grep("BLM",RSmods[s]))>0){
					BLM[s]<-1
				}else{
					LM[s]<-1
				}
			}	
			tmp.par<-read.csv(file.path(DIR_main,rec_path,"RS_fits",paste("ceattle_recruit_",RSmods[s],".std",sep="")),sep="")
			tmp.ctl<-as.numeric(read.csv(file.path(DIR_main,rec_path,"RS_fits",paste("ceattle_recruit_",RSmods[s],".ctl",sep="")),sep="",header=FALSE,skip=1,nrows=1))
			# HESStmp<-get.admb.hes(flnm=file.path(DIR_main,rec_path,"RS_fits",paste0("ceattle_recruit_",RSmods[s],".hes")))
			# tmp.hes<-GET_HESS_AIC(GET_HESS_AIC(HESStmp,npar=npar[i],NLL=-1*LL[i])[[1]])
			sigma[s]<-tmp.par$value[tmp.par$name=="logsigma"]
			sigma_std[s]<-tmp.par$std.dev[tmp.par$name=="logsigma"]

			aa_c[s]<-tmp.par$value[tmp.par$name=="log_aa_c"]
			aa_c_std[s]<-tmp.par$std.dev[tmp.par$name=="log_aa_c"]
			if(LM[s]!=1){
				bb_c[s]<-tmp.par$value[tmp.par$name=="log_bb_c"]
				bb_c_std[s]<-tmp.par$std.dev[tmp.par$name=="log_bb_c"]
			}
			ncov[s]<-length(covars_used[[s]][1,])
			pp<-grep("rs_parm",tmp.par$name)
			if(length(pp)>0){
				for (ii in 1:length(pp)){
					tt<-as.numeric(unlist(strsplit(as.character(tmp.par$name[pp[ii]]),split="[^0-9]+")))
					tt<-tt[-is.na(tt)]
					rs_parm[s,tt]<-tmp.par[pp[ii],]$value
					rs_parm_std[s,tt]<-tmp.par[pp[ii],]$std.dev
				}
				cov_phase2[s,]<-as.numeric(as.factor(rs_parm[s,]!=0))-1
				tttt<-grep("Eat",RSmods[s])
				if(length(tttt)>0){
					cov_type2[s,tmp.ctl]<-cov_phase2[s,tmp.ctl]
				}
			}
		}
		cat("#ncov ",file=outfile,append=FALSE,sep="\n")
		cat(ncov,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#Bev Holt ",file=outfile,append=TRUE,sep="\n")
		cat(BevH,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#BLM ",file=outfile,append=TRUE,sep="\n")
		cat(BLM,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#LM ",file=outfile,append=TRUE,sep="\n")
		cat(LM,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#sigma",file=outfile,append=TRUE,sep="\n")
		cat(sigma,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#sigma_std",file=outfile,append=TRUE,sep="\n")
		cat(sigma_std,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		#cat("#Ration : Ration of 1 year old fish in the previous year ",file=outfile,append=TRUE,sep="\n")
		#	cat(covuse[[k]]$RAT1_lag1,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#mnRat : mean Ration of 1 year old fish ",file=outfile,append=TRUE,sep="\n")
			cat(c(mean.na(covuse[[1]]$RAT1_lag1),mean.na(covuse[[2]]$RAT1_lag1),mean.na(covuse[[3]]$RAT1_lag1)),file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#sdRat : sd Ration of 1 year old fish ",file=outfile,append=TRUE,sep="\n")
			cat(c(sd.na(covuse[[1]]$RAT1_lag1),sd.na(covuse[[2]]$RAT1_lag1),sd.na(covuse[[3]]$RAT1_lag1)),file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#aa_c",file=outfile,append=TRUE,sep="\n")
		cat(aa_c,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#aa_c_std",file=outfile,append=TRUE,sep="\n")
		cat(aa_c_std,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")

		cat("#bb_c",file=outfile,append=TRUE,sep="\n")
		cat(bb_c,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		cat("#bb_c_std",file=outfile,append=TRUE,sep="\n")
		cat(bb_c_std,file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")

		cat("#rs_parm",file=outfile,append=TRUE,sep="\n")
		for(s in 1:np){
			cat(rs_parm[s,],file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		}
		cat("#rs_parm_std",file=outfile,append=TRUE,sep="\n")
		for(s in 1:np){
			cat(rs_parm_std[s,],file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		}
		cat("#cov_type2",file=outfile,append=TRUE,sep="\n")
		for(s in 1:np){
			cat(cov_type2[s,],file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		}
		cat("#cov_phase2",file=outfile,append=TRUE,sep="\n")
		for(s in 1:np){
			cat(cov_phase2[s,],file=outfile,append=TRUE,sep=" ");cat("",file=outfile,append=TRUE,sep="\n")
		}
		cat("#RS data test number rs_test_num",file=outfile,append=TRUE,sep="\n")
		cat(12345,file=outfile,append=TRUE,sep="\n")
		

		return(list(BevH=BevH,BLM=BLM,LM=LM,
			sigma=sigma,sigma_std=sigma_std,aa_c=aa_c,aa_c_std=aa_c_std,bb_c=bb_c,bb_c_std=bb_c_std,
			ncov=ncov,rs_parm=rs_parm,rs_parm_std=rs_parm_std,
			cov_type2=cov_type2,cov_phase2=cov_phase2))

	}


	topAIC.txt<-c(rownames(AICtable[[1]])[1],rownames(AICtable[[2]])[1],rownames(AICtable[[3]])[1])
	#c((AICtable[[1]]$names)[1],(AICtable[[2]]$names)[1],(AICtable[[3]]$names)[1])
	# ttt<-paste(grep.list(find=c("ColdPool","fallZavg","springZavg"),source=cov_names),sep="",collapse="_")
	ttt<-paste(which(cov_names%in%c("ColdPool","fallZavg","springZavg")),sep="",collapse="_")
	ttt<-paste(which(cov_names%in%c("BottomTemp","fallZavg","springZavg")),sep="",collapse="_")
	# paste("RS",1:3,"_Eat_",ttt,sep="")
	# ttt2<-paste(grep.list(find=c("ColdPool","fallZtot","springZtot"),source=cov_names),sep="",collapse="_")
	ttt2<-paste(which(cov_names%in%c("ColdPool","fallZtot","springZtot")),sep="",collapse="_")
	ttt2<-paste(which(cov_names%in%c("BottomTemp","fallZtot","springZavg")),sep="",collapse="_")
	# paste("RS",1:3,"_Eat_",ttt2,sep="")

	ttz<-paste(which(cov_names%in%c("fallZtot")),sep="",collapse="_")

	tmpttt<-create_datfile(RSmods=paste("RS",1:3,"_0",sep=""),fl1="rs_data4CEATTLE_0_0_0.dat")	#recMode = 2:

	tmpttt<-create_datfile(RSmods=topricker,fl1="rs_data4CEATTLE_TOPRicker.dat")
	tmpttt<-create_datfile(RSmods=topAIC.txt,fl1="rs_data4CEATTLE_TOP.dat")
	tmpttt<-create_datfile(RSmods=topR2ricker,fl1="rs_data4CEATTLE_TopR2_Ricker.dat")
	tmpttt<-create_datfile(RSmods=TopR2.txt,fl1="rs_data4CEATTLE_TopR2.dat")

	if(length(grep("SST_tmp",cov_names))>0){tmpttt<-tmpttt<-create_datfile(RSmods=paste("RS",1:3,"_",grep("SST_tmp",cov_names),sep=""),fl1="rs_data4CEATTLE_SST.dat")}else{	 cat("\n");cat("_______  rs_data4CEATTLE_SST.dat could not be generated because SST_tmp is not included in covariates _______"); cat("\n")}
	if(length(grep("BottomTemp",cov_names))>0){tmpttt<-create_datfile(RSmods=paste("RS",1:3,"_",grep("BottomTemp",cov_names),sep=""),fl1="rs_data4CEATTLE_BT.dat")}else{	 cat("\n");cat("_______  rs_data4CEATTLE_BT.dat could not be generated because BottomTemp is not included in covariates _______"); cat("\n")}
	if(length(grep("ColdPool",cov_names))>0){
		tmpttt<-create_datfile(RSmods=paste("RS",1:3,"_",grep("ColdPool",cov_names),sep=""),fl1="rs_data4CEATTLE_ColdPool.dat")
	}else{	 
		cat("\n");cat("_______  rs_data4CEATTLE_ColdPool.dat could not be generated because ColdPool is not included in covariates _______"); cat("\n")
	}

	tmpttt<-create_datfile(RSmods=paste("RS",1:3,"_Eat_",ttt,sep=""),fl1="rs_data4CEATTLE_fullEAT_avg.dat")
	tmpttt<-create_datfile(RSmods=paste("RS",1:3,"_",ttz,sep=""),fl1="rs_data4CEATTLE_fallZtot.dat")
	
	tmpttt<-create_datfile(RSmods=paste("RS",1:3,"_",ttt,sep=""),fl1="rs_data4CEATTLE_full_avg.dat")
	#tmpttt<-create_datfile(RSmods=paste("RS",1:3,"_",ttt2,sep=""),fl1="rs_data4CEATTLE_full_tot.dat")
	# tmpttt<-create_datfile(RSmods=paste("RS",1:3,"_Eat_",ttt2,sep=""),fl1="rs_data4CEATTLE_fullEAT_tot.dat")
	# tmpttt<-create_datfile(RSmods=paste("RS",1:3,"_",ttt2,sep=""),fl1="rs_data4CEATTLE_full_tot.dat")


# ###########################################################
# Now graph results
# ###########################################################
		col1<-colorRampPalette(colors()[c(71,73)])
		# getwd()
		ss<-1
		figure1<-function(){
			for(ss in 1:nspp){
				tmpnam<-colnames(smry_RS[[ss]])
				Obs_c<-grep("obs",tmpnam)
				mnRckr_c<-grep("mnRS_Rec",tmpnam)
				HR2_c<-grep("TopR2_Rec",tmpnam)
				AIC_c<-grep("TopAIC_Rec",tmpnam)
				R2Cov_c<-grep("R2cov",tmpnam)
				aicCov_c<-grep("TopAICcov",tmpnam)
				# figfile<-paste("RecFigs_tmsm_",MSM_type,sep="")
				# if(length(grep(figfile, dir()))>0){}else{dir.create(file.path(figfile))}
				# #if(file.exists(figfile)){}else{dir.create(figfile)}
				quartz(height=5,width=6)
				par(mfrow=c(2,1))
				par(mar=c(1,1,0,0)) # margins of graph: (bottom,left, top, right)
				par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
				par(oma=c(3.5,3.5,2,2))# outer margins of graph: (bottom,left, top, right)
				xatt<-pretty(covuse[[ss]]$Year,5)
				ylimm<-c(.8*min.na(log(smry_RS[[ss]][,4])),
					1.2*max.na(log(smry_RS[[ss]][,4])))
				plot(smry_RS[[ss]][,2],log(smry_RS[[ss]][,4]),type="b",main="",axes=FALSE)
				mtext(side=3,sp[ss],font=2)
				lgndtxt<-c("Observed","mean Ricker","topR2","topAIC")
				lgndtxt[3]<-paste(lgndtxt[3]," (",AICtable[[ss]]$names[topR2n[ss]],")",sep="")
				lgndtxt[4]<-paste(lgndtxt[4]," (",AICtable[[ss]]$names[1],")",sep="")

				#legend(covuse[[ss]]$Year[1],max.na(log(smry_RS[[ss]][,4])),
					legend("top",
					lgndtxt,
					lty=c(1,2,1,1),
					lwd=c(1,1,2,1),
					pch=c(1,-4,-4,-4),
					col=c("black","black","red","blue"),box.lty=0,cex=.5,horiz = TRUE,yjust=-.2)
				axis(1,at=xatt,lab=rep("",length(xatt)))
				axis(2);mtext(side=2,"log(R)",line=2,font=2)
				lines(smry_RS[[ss]][,2],log(smry_RS[[ss]][,Obs_c]),lty=3,ylim=c(0,6))
				lines(smry_RS[[ss]][,2],log(smry_RS[[ss]][,mnRckr_c]),lty=2,ylim=c(0,6))
				lines(smry_RS[[ss]][,2],log(smry_RS[[ss]][,HR2_c]),lty=1,ylim=c(0,6),col="red",lwd=2)
				lines(smry_RS[[ss]][,2],log(smry_RS[[ss]][,AIC_c]),lty=1,ylim=c(0,6),col="blue")

				mntxt1<-strsplit(colnames(smry_RS[[ss]])[R2Cov_c],split="R2cov_")[[1]][2]
				mntxt2<-strsplit(colnames(smry_RS[[ss]])[aicCov_c],split="TopAICcov_")[[1]][2]

				plot(smry_RS[[ss]][,2],scale(smry_RS[[ss]][,R2Cov_c]),type="l",ylim=c(-3,3),main="",axes=FALSE,col="red")
				if(length(smry_RS[[ss]][,aicCov_c])>0) lines(smry_RS[[ss]][,2],scale(smry_RS[[ss]][,aicCov_c]),col="blue")
				#mtext(side=3,mntxt,font=2,line=-1)
				axis(1,at=xatt)
				mtext(side=1,"Year",font=2,line=2)
				axis(2);mtext(side=2,"scaled covariate value",line=2,font=2)
				abline(h=0,lty=2)

				legend(covuse[[ss]]$Year[1],3,
					c(mntxt1,mntxt2),
					lty=c(1,1),
					pch=-4,
					col=c("red","blue"),box.lty=0,cex=.7)
				quartz.save(file.path(fig.file,paste("Fig1_",sp[ss],".jpg",sep="")), type = "jpeg", dpi = 500)
			}
		}
		
		ss<-1
		figure1v2<-function(col2=c(colors()[300],col1(10)[c(3,10,5)])){
			for(ss in 1:nspp){
				tmpnam<-colnames(smry_RS[[ss]])
				Obs_c<-grep("obs",tmpnam)
				mnRckr_c<-grep("mnRS_Rec",tmpnam)
				HR2_c<-grep("TopR2_Rec",tmpnam)
				AIC_c<-grep("TopAIC_Rec",tmpnam)
				R2Cov_c<-grep("R2cov",tmpnam)
				aicCov_c<-grep("TopAICcov",tmpnam)
				# figfile<-paste("RecFigs_tmsm_",MSM_type,sep="")
				# if(length(grep(figfile, dir()))>0){}else{dir.create(file.path(figfile))}
				
				quartz(height=5,width=6)
				par(mfrow=c(2,1))
				par(mar=c(1,1,0,0)) # margins of graph: (bottom,left, top, right)
				par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
				par(oma=c(3.5,3.5,2,2))# outer margins of graph: (bottom,left, top, right)
				xatt<-pretty(covuse[[ss]]$Year,5)
				ylimm<-c(.8*min.na(log(smry_RS[[ss]][,4])),
					1.2*max.na(log(smry_RS[[ss]][,4])))
				plot(smry_RS[[ss]][,2],log(smry_RS[[ss]][,4]),type="b",main="",axes=FALSE,lwd=2,col=col2[1])
				mtext(side=3,sp[ss],font=2)
				lgndtxt<-c("Observed","mean Ricker","topR2","topAIC")
				lgndtxt[3]<-paste(lgndtxt[3]," (",AICtable[[ss]]$names[topR2n[ss]],")",sep="")
				lgndtxt[4]<-paste(lgndtxt[4]," (",AICtable[[ss]]$names[1],")",sep="")

				#legend(covuse[[ss]]$Year[1],max.na(log(smry_RS[[ss]][,4])),
					legend("top",
					lgndtxt,
					lty=c(1,2,1,1),
					lwd=c(1,1,2,1),
					pch=c(1,-4,-4,-4),
					col=col2,box.lty=0,cex=.5,horiz = TRUE,yjust=-.2)
				axis(1,at=xatt,lab=rep("",length(xatt)))
				axis(2);mtext(side=2,"log(R)",line=2,font=2)

				reclist_c<-grep("RhatTop",colnames(smry_RS[[ss]]))
				covlist_c<-grep("CovUse",colnames(smry_RS[[ss]]))
				for(mm in 1:length(reclist_c)){
					lines(smry_RS[[ss]][,2],log(smry_RS[[ss]][,reclist_c[mm]]),col=makeTransparent(col2[2],alpha=100))
				}
				#lines(smry_RS[[ss]][,2],log(smry_RS[[ss]][,Obs_c]),lty=3,col=col2[1])
				lines(smry_RS[[ss]][,2],log(smry_RS[[ss]][,mnRckr_c]),lty=2,col=col2[2])
				lines(smry_RS[[ss]][,2],log(smry_RS[[ss]][,AIC_c]),lty=1,col=col2[4])
				lines(smry_RS[[ss]][,2],log(smry_RS[[ss]][,Obs_c]),lty=1,col=col2[1],lwd=2,type="b")	
				lines(smry_RS[[ss]][,2],log(smry_RS[[ss]][,HR2_c]),lty=1,col=col2[3],lwd=2)
				mntxt1<-strsplit(colnames(smry_RS[[ss]])[R2Cov_c],split="R2cov_")[[1]][2]
				mntxt2<-strsplit(colnames(smry_RS[[ss]])[aicCov_c],split="TopAICcov_")[[1]][2]
				mntxt2<-""
				plot(smry_RS[[ss]][,2],scale(smry_RS[[ss]][,R2Cov_c]),type="l",ylim=c(-3,3),main="",axes=FALSE,col=col2[3])
				for(mm in 1:length(covlist_c)){
					lines(smry_RS[[ss]][,2],scale(smry_RS[[ss]][,covlist_c[mm]]),col=makeTransparent(col2[2],alpha=100))
					mntxt2<-c(mntxt2,strsplit(colnames(smry_RS[[ss]])[covlist_c[mm]],split="CovUse_")[[1]][2])

				}
				mntxt2<-paste(mntxt2[mntxt2!=""],collapse=", ",sep="")
				lines(smry_RS[[ss]][,2],scale(smry_RS[[ss]][,aicCov_c]),col=col2)
				lines(smry_RS[[ss]][,2],scale(smry_RS[[ss]][,R2Cov_c]),type="l",col=col2[3],lwd=2)
				#mtext(side=3,mntxt,font=2,line=-1)
				axis(1,at=xatt)
				mtext(side=1,"Year",font=2,line=2)
				axis(2);mtext(side=2,"scaled covariate value",line=2,font=2)
				abline(h=0,lty=2)

				legend(covuse[[ss]]$Year[1],3,
					c(mntxt1,mntxt2),
					lty=c(1,1),
					pch=-4,
					lwd=c(2,1),
					col=c(col2[3],col2[2]),box.lty=0,cex=.5)
				quartz.save(file.path(fig.file,paste("Fig1v2_",sp[ss],".jpg",sep="")), type = "jpeg", device = dev.cur(), dpi = 500)
			}
		}
		#figure1v2()
		ss<-2

		includeCovs<-data.frame(
			rbind(c("BottomTemp","Summer BT"),
			c("NH4_tot","Nitrogen"),
			c("Cop_tot","Small Copepod B"),
			c("Nca_tot","Large Copepod B"),
			c("Eup_tot","Euphausiids"),
			c("fallZtot","Fall Zoop"),
			c("summerZtot","Summer Zoop")))
		names(includeCovs)<-c("covname","txt")
				
		includeCovs2<-data.frame(
			rbind(c("BottomTemp","Summer BT"),
			c("Cop_tot","Small Copepod B"),
			c("Nca_tot","Large Copepod B"),
			c("fallZtot","Fall Zoop")))
		names(includeCovs)<-c("covname","txt")		

		figure5<-function(alpha1=250,tmptype="Ricker",includeCovs1=includeCovs,col2=c(colors()[300],col1(10))){
			for(ss in 1:nspp){
				
				nn<-dim(includeCovs1)[1]
				plotc<-rep(NA,nn)
				tmpnam<-colnames(smry_RS[[ss]])
				covlist<-list()
				for(ni in 1:nn){
					ttmp<-grep(includeCovs1[ni,1],AICtable[[ss]]$model)
					ttmp<-AICtable[[ss]]$names[ttmp]
					if(tmptype=="Ricker"){
						txt<-ttmp[-grep("LM",ttmp)]
					}else{
						txt<-ttmp[grep(tmptype,ttmp)]
					}
					tt<-read.csv(file.path(DIR_main,rec_path,"RS_fits",paste("ceattle_recruit",txt,".rep",sep="")),sep=" ")[,1:5]
					covlist[[ni]]<-tt$R_hat
				}
			
				
				
				quartz(height=5,width=6)
				par(mfrow=c(2,1))
				par(mar=c(1,1,0,0)) # margins of graph: (bottom,left, top, right)
				par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
				par(oma=c(3.5,3.5,2,2))# outer margins of graph: (bottom,left, top, right)
				xatt<-pretty(covuse[[ss]]$Year,5)
				ylimm<-c(.8*min.na(log(smry_RS[[ss]][,4])),
					1.2*max.na(log(smry_RS[[ss]][,4])))
				plot(smry_RS[[ss]][,2],log(smry_RS[[ss]][,4]),type="b",main="",axes=FALSE,lwd=2,col=col2[1])
				mtext(side=3,sp[ss],font=2)
				lgndtxt<-c("Observed",as.character(includeCovs1[,2]))
				lgndtxt[3]<-paste(lgndtxt[3]," (",AICtable[[ss]]$names[topR2n[ss]],")",sep="")
				lgndtxt[4]<-paste(lgndtxt[4]," (",AICtable[[ss]]$names[1],")",sep="")

				#legend(covuse[[ss]]$Year[1],max.na(log(smry_RS[[ss]][,4])),
				if(1==10){	
					legend("top",
					lgndtxt,
					lty=c(1,2,1,1),
					lwd=c(1,1,2,1),
					pch=c(1,-4,-4,-4),
					col=col2,box.lty=0,cex=.5,horiz = TRUE,yjust=-.2)
				}	
				axis(1,at=xatt,lab=rep("",length(xatt)))
				axis(2);mtext(side=2,"log(R)",line=2,font=2)

				#reclist_c<-grep("RhatTop",colnames(smry_RS[[ss]]))
				#covlist_c<-grep("CovUse",colnames(smry_RS[[ss]]))
				for(mm in 1:length(covlist)){
					lines(smry_RS[[ss]][,2],log(covlist[[mm]]),col=makeTransparent(col2[1+mm],alpha=alpha1),lwd=2)
				}
				#lines(smry_RS[[ss]][,2],log(smry_RS[[ss]][,Obs_c]),lty=3,col=col2[1])
				scale(hindcast[,grep(includeCovs1[1,1],names(hindcast))])

				yrs<-smry_RS[[ss]][,2]
				rr<-match(yrs,hindcast$year)
				mm<-1
				mntxt2<-as.character(includeCovs1[mm,2])
				plot(hindcast$year[rr],scale(hindcast[,grep(includeCovs1[1,1],names(hindcast))])[rr],type="l",ylim=c(-3,3),main="",axes=FALSE,col=col2[1+mm],lwd=2)
				for(mm in 2:length(covlist)){
					lines(hindcast$year[rr],scale(hindcast[,grep(includeCovs1[mm,1],names(hindcast))])[rr],col=makeTransparent(col2[1+mm],alpha=alpha1),lwd=2)
					mntxt2<-c(mntxt2,as.character(includeCovs1[mm,2]))
				}
				mntxt2<-paste(mntxt2[mntxt2!=""],collapse=", ",sep="")
				#mtext(side=3,mntxt,font=2,line=-1)
				axis(1,at=xatt)
				mtext(side=1,"Year",font=2,line=2)
				axis(2);mtext(side=2,"scaled covariate value",line=2,font=2)
				abline(h=0,lty=2)

				legend(covuse[[ss]]$Year[1],3,
					includeCovs1[,2],
					lty=1,
					lwd=2,
					col=col2[-1][1:length(covlist)],box.lty=0,cex=.5,horiz=TRUE)
				quartz.save(file.path(fig.file,paste("Fig5_",sp[ss],".jpg",sep="")), type = "jpeg", device = dev.cur(), dpi = 500)
			}
		}
		#figure5(includeCovs1=includeCovs2,col2=c(colors()[300],col1(6)[c(1,3,4,6)]))
		figure2<-function(recpath=file.path(DIR_main,model_path,"ceattle_2")){
			ss<-1
			for(ss in 1:nspp){
				type1<-"Ricker"
				if(length(grep("_LM",TopR2.txt[ss]))==1)	type1<-"LM"
				if(length(grep("_BLM",TopR2.txt[ss]))==1)	type1<-"BLM"
				if(length(grep("_BH",TopR2.txt[ss]))==1)	type1<-"BH"
				

				#tt1<-read.csv(file.path(recpath,"Recruitment_files/RS_fits","ceattle_recruit_RS1_Eat_12_17_19_25.std"),sep="")
				#tt0<-read.csv(file.path(recpath,"Recruitment_files/RS_fits","ceattle_recruit_RS1_Eat_12_17_19_25_LM.std"),sep="")
				tt<-read.csv(file.path(recpath,"Recruitment_files/RS_fits",paste("ceattle_recruit_",TopR2.txt[ss],".std",sep="")),sep="")
				ttnull<-read.csv(file.path(recpath,"Recruitment_files/RS_fits",paste("ceattle_recruit_RS",ss,"_0.std",sep="")),sep="")
				SSBtmp<-pretty(0:max.na(covuse[[ss]]$SSB),1e6)
				#SSBtmp<-covuse[[ss]]$SSB
				mnRS<-exp(predictRS(parms=ttnull[2:3,3],B=SSBtmp,ncov=0,covs=NULL,type="Ricker"))
				mnRStop<-exp(predictRS(parms=tt[-1,3],B=SSBtmp,ncov=0,covs=NULL,type=type1))
				# figfile<-paste("RecFigs_tmsm_",MSM_type,sep="")
				# if(length(grep(figfile, dir()))>0){}else{dir.create(file.path(figfile))}
				quartz(height=4,width=6)
				par(mfrow=c(1,1))
				par(mar=c(1,1,0,0)) # margins of graph: (bottom,left, top, right)
				par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
				par(oma=c(3.5,3.5,2,2))# outer margins of graph: (bottom,left, top, right)
				maxx<-1.1*max.na(smry_RS[[ss]][,3])
				maxy<-1.1*max.na(smry_RS[[ss]][,4])
				xatt<-pretty(seq(0,(2*maxx),1e3),5)
				yatt<-as.numeric(pretty(seq(0,(2*maxy),1e6),5))
				plot(smry_RS[[ss]][,3],(smry_RS[[ss]][,4]),pch=16,main="",axes=FALSE,ylim=c(0,maxy),xlim=c(0,maxx))
				lines(SSBtmp,mnRS)
				mtext(side=3,sp[ss],font=2)
				legend(xatt[1],maxy,
					c("Observed","mean Ricker","Highest R2","topAIC"),
					lty=c(0,1,0,0),
					lwd=c(1,1,2,1),
					pch=c(16,-4,1,4),
					col=c("black","black","red","blue"),box.lty=0,cex=.9)
				axis(1,at=xatt)
				axis(2,at=yatt)
				mtext(side=2,"Recruitment",line=2,font=2)
				mtext(side=1,"Spawning Biomass",font=2,line=2)
				#points(smry_RS[[ss]][,3],(smry_RS[[ss]][,5]),lty=3,ylim=c(0,6))
				#points(smry_RS[[ss]][,3],(smry_RS[[ss]][,6]),lty=2,ylim=c(0,6))
				points(smry_RS[[ss]][,3],(smry_RS[[ss]][,7]),lty=1,ylim=c(0,6),col="red",lwd=2)
				points(smry_RS[[ss]][,3],(smry_RS[[ss]][,8]),lty=1,ylim=c(0,6),col="blue",pch=4)
				mntxt1<-strsplit(colnames(smry_RS[[ss]])[9],split="R2cov_")[[1]][2]
				mntxt2<-strsplit(colnames(smry_RS[[ss]])[10],split="TopAICcov_")[[1]][2]
				quartz.save(file.path(fig.file,paste("Fig2_",sp[ss],".jpg",sep="")), type = "jpeg", dpi = 500,height=4,width=6)
			}
		}
		#figure2() 
		ss<-1
		col1<-colorRampPalette(colors()[c(73,71)])
		col1<-colorRampPalette(colors()[c(71,73)])
			# NOT UPDATED:
				# calc_oosamp<-function(coln=4,tmptype=type1,sppp=1,usesigma=FALSE){
				# 	if(tmptype=="Ricker"){
				# 			txt<-paste("RS",sppp,"_",coln,sep="")
				# 	}else{
				# 		txt<-paste("RS",sppp,"_",coln,"_",tmptype,sep="")
				# 	}
				# 	ncov1<-length(coln)

				# 	if(coln==0) {covs.tmp=NULL}
				# 	stop("LINE 1606")
				# 	tt<-read.csv(paste(file.path(DIR_main,rec_path),"/RS_fits/ceattle_recruit_",txt,".std",sep=""),sep="")
				# 	outofsample<-data.frame(Rec.obs[[sppp]],hindcast=NA,CCCMA=NA,MIROC=NA,ECHOG=NA)	
				# 	rr<-na.omit(match(outofsample$Year,hindcast$year))

				# 	if(coln>0) outofsample$hindcast[na.omit(match(outofsample$Year,hindcast$year[rr]))]<-hindcast[rr,coln+1]

				# 	rout<-which(is.na(match(Rec.obs[[sppp]]$Year,covuse.all[[sppp]]$Year)))
				# 	Rechat<-list()
				# 	tmp1<-CCCMA
				# 		overlapyr.hind<-na.omit(match(tmp1$year,outofsample$Year))
				# 		overlapyr.fut<-na.omit(match(outofsample$Year,tmp1$year))
				# 		if(coln>0){
				# 			outofsample$CCCMA[overlapyr.hind]<-tmp1[overlapyr.fut,][,coln+1]
				# 			covs.tmp<-outofsample[overlapyr.hind,]$CCCMA
				# 		}
						
				# 		Rechat$cccma<-data.frame(year=outofsample$Year[overlapyr.hind],
				# 			hat=predictRS(parms=tt[-1,3],B=outofsample[overlapyr.hind,]$SSB,ncov=ncov1,covs=covs.tmp,type=tmptype))

				# 		tmp1<-MIROC
				# 		overlapyr.hind<-na.omit(match(tmp1$year,outofsample$Year))
				# 			overlapyr.fut<-na.omit(match(outofsample$Year,tmp1$year))
				# 			if(coln>0){
				# 				outofsample$MIROC[overlapyr.hind]<-tmp1[overlapyr.fut,][,coln+1]
				# 				covs.tmp<-outofsample[overlapyr.hind,]$MIROC
				# 			}
				# 			Rechat$miroc<-data.frame(year=outofsample$Year[overlapyr.hind],
				# 				hat=predictRS(parms=tt[-1,3],B=outofsample[overlapyr.hind,]$SSB,ncov=ncov1,covs=covs.tmp,type=tmptype))

				# 		tmp1<-ECHOG
				# 		overlapyr.hind<-na.omit(match(tmp1$year,outofsample$Year))
				# 			overlapyr.fut<-na.omit(match(outofsample$Year,tmp1$year))
				# 			if(coln>0){
				# 				outofsample$ECHOG[overlapyr.hind]<-tmp1[overlapyr.fut,][,coln+1]
				# 				covs.tmp<-outofsample[overlapyr.hind,]$ECHOG
				# 			}
				# 			Rechat$echog<-data.frame(year=outofsample$Year[overlapyr.hind],
				# 				hat=predictRS(parms=tt[-1,3],B=outofsample[overlapyr.hind,]$SSB,ncov=ncov1,covs=covs.tmp,type=tmptype))
				# 		sigma<-1
				# 		if(usesigma==TRUE) sigma<-sd.na(outofsample$REC[-rout])

				# 		mu<-mean.na(log(outofsample$REC[-rout]))
				# 		if(coln>0){
				# 				covs.tmp<-outofsample[-rout,]$hindcast
				# 			}
				# 		oosample0<-(predictRS(parms=tt[-1,3],B=outofsample[-rout,]$SSB,ncov=ncov1,covs=covs.tmp,type=tmptype))
				# 		names(oosample0)<-outofsample$Year[-rout]
				# 		oosample<-matrix(NA,4,length(rout))
				# 		rownames(oosample)<-c("mean","CCCMA","MIROC","ECHOG")
				# 		colnames(oosample)<-outofsample$Year[rout]
				# 		devs<-oosample
				# 		oosample[1,]<-rep(mu,length(rout))
				# 		if(coln>0){
				# 			covs.tmp<-outofsample[rout,]$CCCMA
				# 				oosample[2,]<-(predictRS(parms=tt[-1,3],B=outofsample[rout,]$SSB,ncov=ncov1,covs=covs.tmp,type=tmptype))
				# 			covs.tmp<-outofsample[rout,]$MIROC
				# 			oosample[3,]<-(predictRS(parms=tt[-1,3],B=outofsample[rout,]$SSB,ncov=ncov1,covs=covs.tmp,type=tmptype))
				# 				covs.tmp<-outofsample[rout,]$ECHOG
				# 			oosample[4,]<-(predictRS(parms=tt[-1,3],B=outofsample[rout,]$SSB,ncov=ncov1,covs=covs.tmp,type=tmptype))
				# 		}else{
				# 			oosample[2,]<-(predictRS(parms=tt[-1,3],B=outofsample[rout,]$SSB,ncov=ncov1,covs=covs.tmp,type=tmptype))
				# 			oosample[3,]<-(predictRS(parms=tt[-1,3],B=outofsample[rout,]$SSB,ncov=ncov1,covs=covs.tmp,type=tmptype))
				# 			oosample[4,]<-(predictRS(parms=tt[-1,3],B=outofsample[rout,]$SSB,ncov=ncov1,covs=covs.tmp,type=tmptype))
				# 		}
						
				# 		rownames(devs)[1]<-"null"
				# 		dev.explained<-devs
				# 		devs[2,]<-((outofsample$REC[rout]-exp(oosample[2,]))^2)/(sigma^2)
				# 		devs[3,]<-((outofsample$REC[rout]-exp(oosample[3,]))^2)/(sigma^2)
				# 		devs[4,]<-((outofsample$REC[rout]-exp(oosample[4,]))^2)/(sigma^2)
				# 		devs[1,]<-((outofsample$REC[rout]-oosample[1,])^2)/(sigma^2)
							

				# 			dev.explained[1,]<-(devs[1,]-devs[1,])/devs[1,]
				# 			dev.explained[2,]<-(devs[1,]-devs[2,])/devs[1,]
				# 			dev.explained[3,]<-(devs[1,]-devs[3,])/devs[1,]
				# 			dev.explained[4,]<-(devs[1,]-devs[4,])/devs[1,]

				# 			forecast.fit<-rep(0,3)
				# 			names(forecast.fit)<-rownames(oosample)[-1]
				# 			forecast.fit[1]<-(sum(devs[1,])-sum(devs[2,]))/sum(devs[1,])
				# 			forecast.fit[2]<-(sum(devs[1,])-sum(devs[3,]))/sum(devs[1,])
				# 			forecast.fit[3]<-(sum(devs[1,])-sum(devs[4,]))/sum(devs[1,])
						
				# 		dev.hind<-((outofsample$REC[-rout]-exp(oosample0))^2)/(sigma^2)
				# 		dev.null.hind<-((outofsample$REC[-rout]-exp(mu))^2)/(sigma^2)
				# 		devs.hind<-((dev.null.hind)-(dev.hind))/(dev.null.hind)
				# 		hind.fit<-(sum(dev.null.hind)-sum(dev.hind))/sum(dev.null.hind)

				# 	return(list(
				# 		outofsample=outofsample,
				# 		oosample0=oosample0,
				# 		oosample=oosample,
				# 		Rechat=Rechat,
				# 		forecast.fit=forecast.fit,
				# 		dev.explained=dev.explained,
				# 		devs=devs,rout=rout,devs.hind=devs.hind,hind.fit=hind.fit))		
				# }
				ss<-2


				# quartz(height=4.5,width=8)
				# 	par(mar=c(1,1,0,0)) # margins of graph: (bottom,left, top, right)
				# 	par(mgp=c(1,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
				# 	par(oma=c(3,3,2,1))# outer margins of graph: (bottom,left, top, right)
			
				# par(mfrow=c(1,1))
			
		figure3<-function(txt1=2009,typeuse="LM",include=c(1:6),ss=1,coll=c(colors()[300],col1(6)[c(1:5)]),ylimm=exp(c(0.01,18.5)),alpha1=80,alpha2=140){
				s<-ss
				type1<-typeuse
				#if(length(grep("_LM",TopR2.txt[ss]))==1)	type1<-"LM"
				#if(length(grep("_BLM",TopR2.txt[ss]))==1)	type1<-"BLM"
				#if(length(grep("_BH",TopR2.txt[ss]))==1)	type1<-"BH"
				colnn<-as.numeric(strsplit(TopR2.txt[ss],split="_")[[1]][2])
				ooTop1<-calc_oosamp(coln=colnn,tmptype="Ricker",sppp=s)
				ooTop<-calc_oosamp(coln=colnn,tmptype=type1,sppp=s)
				topname<-names(MIROC)[colnn+1]
				oonull1<-calc_oosamp(coln=0,tmptype="Ricker",sppp=s)
				oonull<-calc_oosamp(coln=0,tmptype=type1,sppp=s)
				ooCop1<-calc_oosamp(coln=grep("Cop_avg",names(MIROC)),tmptype="Ricker",sppp=s)
				ooCop<-calc_oosamp(coln=grep("Cop_avg",names(MIROC)),tmptype=type1,sppp=s)
				ooBT1<-calc_oosamp(coln=grep("BottomTemp",names(MIROC)),tmptype="Ricker",sppp=s)
				ooBT<-calc_oosamp(coln=grep("BottomTemp",names(MIROC)),tmptype=type1,sppp=s)
				ooPP1<-calc_oosamp(coln=grep("NH4_avg",names(MIROC)),tmptype="Ricker",sppp=s)
				ooPP<-calc_oosamp(coln=grep("NH4_avg",names(MIROC)),tmptype=type1,sppp=s)
				add.lines<-function(oodat=ooTop,textxy=c(2014,8e7),coluse=coll[1],poss=2,main=""){
					points(oodat$outofsample$Year[-oodat$rout],exp(oodat$oosample0),lty=1,col=coluse,pch=17,type="l",lwd=2)
					nx2<-length(oodat$outofsample$Year[-oodat$rout])
					yu<-exp(Recest[[s]][,1]+1.95*Recest[[s]][,2])
					yl<-exp(Recest[[s]][,1]-1.95*Recest[[s]][,2])
					cc<-match(oodat$outofsample$Year[-oodat$rout],yrs)
					win<-which((yu[cc]>=exp(oodat$oosample0)&yl[cc]<=exp(oodat$oosample0))==TRUE)
					pcnt.dev<-100*round(oodat$hind.fit,2)
					pcnt.r2<-round(cor(Recest[[s]][,1][cc],oodat$oosample0)^2,2)
					pcnt<-100*round((length(win)/length(cc)),3)
					text(textxy[1]-3,textxy[2],main,col=coluse,pos=poss)
					text(textxy[1],textxy[2],paste(pcnt.dev,"%"),col=coluse,pos=poss)
					text(textxy[1]+3,textxy[2],(pcnt.r2),col=coluse,pos=poss)
					text(textxy[1]+7,textxy[2],paste(pcnt,"%"),col=coluse,pos=poss)
				}
			 plot(ooTop$outofsample$Year,ooTop$outofsample$REC,lty=1,col="white",axes=FALSE,xlim=c(1979,2013),ylim=ylimm,pch=17,type="l")
				nx<-dim(Recest[[s]])[1]
				yrs<-1979+(1:nx)-1
				xat<-pretty(ooTop$outofsample$Year)
				axis(1,at=xat);axis(1,at=c(1800,3000))
				yat<-pretty(seq(ylimm[1],ylimm[2],1e3))
				axis(2,at=yat,lab=yat/1e6,las=2);axis(2,at=c(-1e10,10e9))
				mtext(side=2,"Recruitment (billions)",line=2)
				xx<-c(yrs,yrs[nx:1])
				yy<-c(exp(Recest[[s]][,1]+1.95*Recest[[s]][,2]),
					(exp(Recest[[s]][,1]-1.95*Recest[[s]][,2]))[nx:1])
				polygon(xx,yy,col=makeTransparent(coll[1],alpha=alpha1),border=F)	
				lines(1979+(1:34)-1,exp(Recest[[s]][,1]),type="b",pch=16,ylim=c(0,1e8),col=makeTransparent(coll[1],alpha=alpha2),lwd=2)
				
				if(any(include==1)) add.lines(main="Null",oodat=oonull,coluse=coll[2],textxy=c(txt1,ylimm[2]*.99))
				if(any(include==2)) add.lines(main="Bottom Temp",oodat=ooBT,coluse=coll[3],textxy=c(txt1,ylimm[2]*.91))
				if(any(include==3)) add.lines(main="Nitrogen",oodat=ooPP,coluse=coll[4],textxy=c(txt1,ylimm[2]*.84))
				if(any(include==4)) add.lines(main="Copepod B",oodat=ooCop,coluse=coll[5],textxy=c(txt1,ylimm[2]*.77))
				if(any(include==5)) add.lines(main=topname,oodat=ooTop,coluse=coll[6],textxy=c(txt1,ylimm[2]*.69))
				#text(txt1,ylimm[2]*1.1,"Deviance Explained",pos=2)
		}

		### now plot/bar graph of predictive power

		figure4<-function(txt1=2007,alpha3=250,typeuse="Ricker",add.include=-99,include=c(1:6),ss=1,coll=c(colors()[300],col1(6)[c(1:5)]),xlimm=c(1979,2013),ltyy1=1,ylimm=exp(c(0.01,18)),alpha1=80,alpha2=140){
				s<-ss
				type1<-typeuse
				#if(length(grep("_LM",TopR2.txt[ss]))==1)	type1<-"LM"
				#if(length(grep("_BLM",TopR2.txt[ss]))==1)	type1<-"BLM"
				#if(length(grep("_BH",TopR2.txt[ss]))==1)	type1<-"BH"
				colnn<-as.numeric(strsplit(TopR2.txt[ss],split="_")[[1]][2])
				ooTop1<-calc_oosamp(coln=colnn,tmptype="Ricker",sppp=s)
				ooTop<-calc_oosamp(coln=colnn,tmptype=type1,sppp=s)
				topname<-names(MIROC)[colnn+1]
				oonull1<-calc_oosamp(coln=0,tmptype="Ricker",sppp=s)
				oonull<-calc_oosamp(coln=0,tmptype=type1,sppp=s)
				ooCop1<-calc_oosamp(coln=grep("Cop_avg",names(MIROC)),tmptype="Ricker",sppp=s)
				ooCop<-calc_oosamp(coln=grep("Cop_avg",names(MIROC)),tmptype=type1,sppp=s)
				ooNca1<-calc_oosamp(coln=grep("Nca_avg",names(MIROC)),tmptype="Ricker",sppp=s)
				ooNca<-calc_oosamp(coln=grep("Nca_avg",names(MIROC)),tmptype=type1,sppp=s)
				ooBT1<-calc_oosamp(coln=grep("BottomTemp",names(MIROC)),tmptype="Ricker",sppp=s)
				ooBT<-calc_oosamp(coln=grep("BottomTemp",names(MIROC)),tmptype=type1,sppp=s)
				ooPP1<-calc_oosamp(coln=grep("NH4_avg",names(MIROC)),tmptype="Ricker",sppp=s)
				ooPP<-calc_oosamp(coln=grep("NH4_avg",names(MIROC)),tmptype=type1,sppp=s)

				
				add.lines<-function(oodat=ooTop,textxy=c(2014,8e7),coluse=coll[1],poss=2,main="",alpha31=alpha3){
					points(oodat$outofsample$Year[-oodat$rout],exp(oodat$oosample0),lty=1,col=makeTransparent(coluse,alpha=alpha31),pch=17,type="l",lwd=2)
					nx2<-length(oodat$outofsample$Year[-oodat$rout])
					yu<-exp(Recest[[s]][,1]+1.95*Recest[[s]][,2])
					yl<-exp(Recest[[s]][,1]-1.95*Recest[[s]][,2])
					cc<-match(oodat$outofsample$Year[-oodat$rout],yrs)
					win<-which((yu[cc]>=exp(oodat$oosample0)&yl[cc]<=exp(oodat$oosample0))==TRUE)
					pcnt.dev<-100*round(oodat$hind.fit,2)
					pcnt.r2<-round(cor(Recest[[s]][,1][cc],oodat$oosample0)^2,2)
					pcnt<-100*round((length(win)/length(cc)),3)
					text(textxy[1]-3,textxy[2],main,col=coluse,pos=poss)
					text(textxy[1],textxy[2],paste(pcnt.dev,"%"),col=coluse,pos=poss)
					text(textxy[1]+3,textxy[2],(pcnt.r2),col=coluse,pos=poss)
					text(textxy[1]+7,textxy[2],paste(pcnt,"%"),col=coluse,pos=poss)
				}
				add.lines.fut<-function(plotavg=TRUE,oodat=ooTop,ltyy=ltyy1,textxy=c(2014,8e7),pchh=17,coluse=coll[1],poss=2,main=""){
					
					if(plotavg==TRUE){
						nn<-length(oodat$oosample0)
						y<-c(oodat$oosample0[nn],apply(oodat$oosample[-1,],2,mean.na))
						x<-as.numeric(names(y))
						points(x,exp(y),lty=ltyy,col=coluse,pch=c(-4,rep(pchh,length(x)-1)),type="l",lwd=2)
						
					}else{
						nn<-length(oodat$oosample0)
						y<-c(oodat$oosample0[nn],oodat$oosample[2,])
						x<-as.numeric(names(y))
						points(x,exp(y),lty=ltyy,col=coluse,pch=c(-4,rep(pchh,length(x)-1)),type="l",lwd=2)
						#points(x,exp(y),lty=2,col=coluse,pch=c(-4,rep(pchh,length(x)-1)),type="p",lwd=2)
						points(x,exp(c(oodat$oosample0[nn],oodat$oosample[3,])),lty=ltyy,col=coluse,pch=c(-4,rep(pchh,length(x)-1)),type="l",lwd=2)
						points(x,exp(c(oodat$oosample0[nn],oodat$oosample[4,])),lty=ltyy,col=coluse,pch=c(-4,rep(pchh,length(x)-1)),type="l",lwd=2)
						
					}

					#points(x,exp(c(oodat$oosample0[nn],oodat$oosample[2,])),lty=2,col=coluse,pch=c(-4,rep(pchh,length(x)-1)),type="l",lwd=2)
					
					nx2<-length(oodat$outofsample$Year[-oodat$rout])
					yu<-exp(Recest[[s]][,1]+1.95*Recest[[s]][,2])
					yl<-exp(Recest[[s]][,1]-1.95*Recest[[s]][,2])
					cc<-match(oodat$outofsample$Year[-oodat$rout],yrs)
					win<-which((yu[cc]>=exp(oodat$oosample0)&yl[cc]<=exp(oodat$oosample0))==TRUE)
					pcnt.dev<-100*round(oodat$hind.fit,2)
					pcnt.r2<-round(cor(Recest[[s]][,1][cc],oodat$oosample0)^2,2)
					pcnt<-100*round((length(win)/length(cc)),3)
					#text(textxy[1]-3,textxy[2],main,col=coluse,pos=poss)
					text(textxy[1]-9,textxy[2],paste(100*round(mean(oodat$forecast.fit),2),"%"),col=coluse,pos=poss)
					text(textxy[1]-13,textxy[2],"",col=coluse,pos=poss)
					
					#text(textxy[1]+3,textxy[2],(pcnt.r2),col=coluse,pos=poss)
					#text(textxy[1]+7,textxy[2],paste(pcnt,"%"),col=coluse,pos=poss)
				}
			 	plot(ooTop$outofsample$Year,ooTop$outofsample$REC,lty=1,col="white",axes=FALSE,xlim=xlimm,ylim=ylimm,pch=17,type="l")
				nx<-dim(Recest[[s]])[1]
				yrs<-1979+(1:nx)-1
				xat<-pretty(ooTop$outofsample$Year)
				axis(1,at=xat);axis(1,at=c(1800,3000))
				yat<-pretty(seq(ylimm[1],ylimm[2],1e3))
				axis(2,at=yat,lab=yat/1e6,las=2);axis(2,at=c(-1e10,10e9))
				mtext(side=2,"Recruitment (billions)",line=2)
				xx<-c(yrs,yrs[nx:1])
				yy<-c(exp(Recest[[s]][,1]+1.95*Recest[[s]][,2]),
					(exp(Recest[[s]][,1]-1.95*Recest[[s]][,2]))[nx:1])
				polygon(xx,yy,col=makeTransparent(coll[1],alpha=alpha1),border=F)	
				lines(1979+(1:34)-1,exp(Recest[[s]][,1]),type="b",pch=16,ylim=c(0,1e8),col=makeTransparent(coll[1],alpha=alpha2),lwd=2)
				
				if(any(include==1)) 	add.lines(main="Null",oodat=oonull,coluse=coll[2],textxy=c(txt1,ylimm[2]*.99))
				if(any(add.include==1))	add.lines.fut(main="Null",oodat=oonull,coluse=coll[2],textxy=c(txt1,ylimm[2]*.99))
				
				if(any(include==2)) 	add.lines(main="Bottom Temp",oodat=ooBT,coluse=coll[3],textxy=c(txt1,ylimm[2]*.91))
				if(any(add.include==2))	add.lines.fut(main="Bottom Temp",oodat=ooBT,coluse=coll[3],textxy=c(txt1,ylimm[2]*.91))
				
				if(any(include==3))		add.lines(main="Nitrogen",oodat=ooPP,coluse=coll[4],textxy=c(txt1,ylimm[2]*.84))
				if(any(add.include==3))	add.lines.fut(main="Nitrogen",oodat=ooPP,coluse=coll[4],textxy=c(txt1,ylimm[2]*.84))
				
				if(any(include==4))		add.lines(main="Copepod B",oodat=ooCop,coluse=coll[5],textxy=c(txt1,ylimm[2]*.77))
				if(any(add.include==4))	add.lines.fut(main="Copepod B",oodat=ooCop,coluse=coll[5],textxy=c(txt1,ylimm[2]*.77))
					
				if(any(include==5)) 	add.lines(main="Large Zoop",oodat=ooNca,coluse=coll[6],textxy=c(txt1,ylimm[2]*.69))
				if(any(add.include==5))	add.lines.fut(main="Large Zoop",oodat=ooNca,coluse=coll[6],textxy=c(txt1,ylimm[2]*.69))
				
				if(any(include==6)) 	add.lines(main=topname,oodat=ooTop,coluse=coll[6],textxy=c(txt1,ylimm[2]*.61))
				if(any(add.include==6))	add.lines.fut(main=topname,oodat=ooTop,coluse=coll[6],textxy=c(txt1,ylimm[2]*.61))
				
			
				#text(txt1,ylimm[2]*1.1,"Deviance Explained",pos=2)
		}
		if(1==10){
			if(MSM_type==0){

				ylimm1<-exp(c(0.01,18.5))
				ylimm2<-exp(c(0.01,14.5))
				ylimm3<-exp(c(0.01,14))
			}else{
				ylimm1<-exp(c(0.01,19.5))
				ylimm2<-exp(c(0.01,15))
				ylimm3<-exp(c(0.01,14))
			}

				quartz(height=4.5,width=8)
						par(mar=c(1,1,0,0)) # margins of graph: (bottom,left, top, right)
						par(mgp=c(1,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
						par(oma=c(3,3,2,1))# outer margins of graph: (bottom,left, top, right)
				
					par(mfrow=c(1,1))
			figure4(ss=1,ylimm=ylimm1,include=c(1:5),typeuse="Ricker")
				mtext("Pollock",side=3,outer=TRUE,line=-1,font=2,adj=.1,cex=1.2)
				quartz.save(file.path(fig.file,"Fig4_1.jpg"),type="jpg",dpi=500,height=4.5,width=8)

			figure4(ss=2,ylimm=ylimm2,include=c(1:5),typeuse="Ricker")
				mtext("P. cod",side=3,outer=TRUE,line=-1,font=2,adj=.1,cex=1.2)
				quartz.save(file.path(fig.file,"Fig4_2.jpg"),type="jpg",dpi=500,height=4.5,width=8)

			figure4(ss=3,ylimm=ylimm3,include=c(1:6),typeuse="Ricker")
				mtext("Arrowtooth",side=3,outer=TRUE,line=-1,font=2,adj=.1,cex=1.2)
				quartz.save(file.path(fig.file,"Fig4_3.jpg"),type="jpg",dpi=500,height=4.5,width=8)


			figure4(ss=1,ylimm=ylimm1,alpha3=250,include=c(1:4),typeuse="Ricker")
				mtext("Pollock",side=3,outer=TRUE,line=-1,font=2,adj=.1,cex=1.2)
				quartz.save(file.path(fig.file,"Fig4_1sub0.jpg"),type="jpg",dpi=500,height=4.5,width=8)
			for(ii in 1:4){
				figure4(ss=1,ylimm=ylimm1,ltyy1=3,alpha3=250,add.include=1:ii,include=c(1:4),typeuse="Ricker")
				mtext("Pollock",side=3,outer=TRUE,line=-1,font=2,adj=.1,cex=1.2)
				quartz.save(file.path(fig.file,paste("Fig4_1sub",ii,".jpg",sep="")),type="jpg",dpi=500,height=4.5,width=8)
			}
			figure4(ss=2,ylimm=ylimm2,alpha3=250,include=c(1:4),typeuse="Ricker")
				mtext("P. cod",side=3,outer=TRUE,line=-1,font=2,adj=.1,cex=1.2)
				quartz.save(file.path(fig.file,"Fig4_2sub0.jpg"),type="jpg",dpi=500,height=4.5,width=8)
			for(ii in 1:4){
				figure4(ss=2,ylimm=ylimm2,ltyy1=3,alpha3=250,add.include=1:ii,include=c(1:4),typeuse="Ricker")
				mtext("P. cod",side=3,outer=TRUE,line=-1,font=2,adj=.1,cex=1.2)
				quartz.save(file.path(fig.file,paste("Fig4_2sub",ii,".jpg",sep="")),type="jpg",dpi=500,height=4.5,width=8)
			}
			figure4(ss=3,ylimm=ylimm3,alpha3=250,include=c(1:4),typeuse="Ricker")
				mtext("Arrowtooth",side=3,outer=TRUE,line=-1,font=2,adj=.1,cex=1.2)
				quartz.save(file.path(fig.file,"Fig4_3sub0.jpg"),type="jpg",dpi=500,height=4.5,width=8)
			for(ii in 1:4){
				figure4(ss=3,ylimm=ylimm3,ltyy1=3,alpha3=250,add.include=1:ii,include=c(1:4),typeuse="Ricker")
				mtext("Arrowtooth",side=3,outer=TRUE,line=-1,font=2,adj=.1,cex=1.2)
				quartz.save(file.path(fig.file,paste("Fig4_3sub",ii,".jpg",sep="")),type="jpg",dpi=500,height=4.5,width=8)
			}
		}
		# graphics.off()

# ###########################################################
# Now save results
# ###########################################################

	save(list=ls(),file=file.path(fig.file,"recruitment.Rdata"))
	sub.path<-path
	# print(sub.path)
	# print(path)
	if(file.exists(paste(sub.path,"/Recruitment_files",sep=""))){	system(paste("rm -rf ",sub.path,"/Recruitment_files",sep=""))}
	dir.create(file.path(path,"Recruitment_files"))
	if(plotit) figure1()
	# txt<-paste("cp -r CEATTLE-master/ceattle_recruit-master/* ",path,"/Recruitment_files/",sep="")
	cp_list<-c(
		"convergeFail_log.csv",
		"fits_4_CEATTLE",
		"Rec_figs",
		"rec_files",
		"RS_fits",paste("RS",1:np,"_log.csv",sep=""))

	for(i in 1:length(cp_list))
		system(paste("cp -r ",file.path(DIR_main,rec_path),"/",cp_list[i]," ",path,"/Recruitment_files/",sep=""))

	# rs.DIR_main<-file.path(DIR_main,rec_path)
	for(i in 1:length(cp_list))
		system(paste("rm -rf ",file.path(DIR_main,rec_path),"/",cp_list[i],sep=""))

	# 	system(" rm -rf ceattle_recruit-master/RS_fits")

	# system(" rm -rf ceattle_recruit-master/RS_fits")
	# system(" rm -rf ceattle_recruit-master/Rec_figs")
	# system(" rm -rf ceattle_recruit-master/fits_4_CEATTLE")
	print("Recruitment files saved")
	# print(smry_RS)
	

	print("Recruitment Analaysis Complete")




