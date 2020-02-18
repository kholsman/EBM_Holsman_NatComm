############################################################################################
## R code to plot CEATTLE outputs
## Kirstin Holsman
## Feb 2015
##
############################################################################################

	# rm(list=ls())	# clear workspaces
	graphics.off()	# close figures
	# rm(list=ls()); setwd("/Users/kkari/GitHub/CEATTLE/runs/pls_wrk_0"); #source("/Users/kkari/GitHub/CEATTLE/src/ceattle-master/Scripts/R_code/PLOT_CEATTLE.R")
	collrange1<-colorRampPalette(colors()[c(310,24)]) # grays
	collrange1<-colorRampPalette(colors()[c(310,24)]) # grays
	col2<-colorRampPalette(colors()[c(71,73)])
	col0<-col1<-colorRampPalette(c("orange","red"))
	

############################################################################################
## Set up
############################################################################################
	update.est<-1 		# update figures (0=no, 1=yes)
	update.proj<-0 		# update figures (0=no, 1=yes)

	# update.figs<-1 		# update figures (0=no, 1=yes)
	update.data<-1 		# update data (0=no, 1=yes)
	#________________________________________
	# Read in the .config file
	#________________________________________

		tt<-read.csv("tmpfile.txt",header=FALSE,colClasses="character")[[1]]
		nt<-length(grep("#",tt))
		tt_nm<-unlist(strsplit(tt[grep("#",tt)],split="#"))[seq(2,nt*2,2)]
		tt<-read.csv("tmpfile.txt",comment="#",header=FALSE,colClasses="character")[[1]]
		for(itt in 1:nt)
			eval(parse(text=paste(tt_nm[itt],"<-'",tt[itt],"'",sep="")))
		if(testread!=12345){message(paste("Error with reading tmpfile.txt  line 29 of MAKE_recruitment_files.R in ",getwd()))}
		model_path<-file.path(DIR_main,model_path)
		rm(tt_nm)
		rm(nt)
		rm(tt)
	#________________________________________
	#________________________________________

	Type1<-mode
	path<-file.path(DIR_main,output_path)
	fn.use<-paste("results/",filename,"_rs.dat",sep="")

	output_root<-strsplit(output_path,filename)[[1]]
	filename_root<-strsplit(output_path,"0")[[1]]
	out_0<-paste(filename_root,"0",sep="")
	out_2<-paste(filename_root,"2",sep="")

	
	output_path<-file.path(DIR_main,output_path)
	plot_file<-plot_fileJPG<-file.path(output_path,"R_figures")
	data_file<-file.path(model_path,"Scripts/Rdata_CSV")
	rec_file<-file.path(output_path,"Recruitment_files/Rec_figs")
	
	# assmt_tmp<-read.csv(file.path("/Users/kkari/Dropbox/Manuscripts/tmsm_manuscript/r_code_tmsm/ASSMT_2012_dat.csv"),sep=",")
	# assmt<-data.frame(t(assmt_tmp[,2:5]))
	# colnames(assmt)<-assmt_tmp[,1]; rm(assmt_tmp)
	source(file.path(model_path,"Scripts/R_code/PLOT_CEATTLE_FUN.R"))	


############################################################################################
## Functions
############################################################################################
	length.na <-function(x){if (any(is.na(x)==FALSE)){length(x[is.na(x)==FALSE])}else{NA}}
	mean.na<-function(x){mean(x,na.rm=TRUE)}
	sd.na<-function(x){sd(x,na.rm=TRUE)}
	se.na<-function(x){length.na(x)/sqrt(sd(x,na.rm=TRUE))}
	sum.na<-function(x){sum(x,na.rm=TRUE)}

	max.na<-function(x){max(x,na.rm=TRUE)}

	min.na<-function(x){min(x,na.rm=TRUE)}

	first.na<-function(x)
	{
		  if (any(is.na(x)==FALSE))
		  {
		    x[is.na(x)==FALSE][1]
		   }else{NA}
	}
	plusSE.na<-function(x){mean.na(x)+1.95*se.na(x)}
	minusSE.na<-function(x){mean.na(x)-1.95*se.na(x)}
	readdat<-function(fn,nm){
		# fn is the file name
		#nm is the object name
		ifile <- scan(fn, what="character",flush=T,blank.lines.skip=T, quiet=T)
		iflex<-which(is.na(ifile))
		idx <- sapply(as.double(ifile), is.na)
		idy<-which(idx)
		datnum<-which(idx==FALSE)
		labnum<-which(idx==TRUE)
		vnam <- ifile[idx] #list names 
		# remove vnam objects that are simply commented out numbers
		tmp<-rep(0,length(vnam))
		tt<-strsplit(vnam,split="#")
		for(i in 1:length(tmp))
			if(is.na(as.numeric(tt[[i]][2])))
				tmp[i]<-1
		vnam2<-vnam[tmp==1]
		tt<-strsplit(vnam2,split="#")
		tmp<-rep(0,length(vnam2))
		for(i in 1:length(tmp))
			if(length(tt[[i]])>1)
				tmp[i]<-1
		vnam2<-vnam2[tmp==1]
		labnum<-match(vnam2,ifile)
		ifilet<-strsplit(ifile,split="#")
		vnamt<-strsplit(vnam2,split="#")
		for(i in 1:length(vnam2))
			vnam2[i]<-vnamt[[i]][2]
		for(i in 1:length(ifile))
			ifile[i]<-ifilet[[i]][length(ifilet[[i]])]	
		vnam2<-na.omit(vnam2)
		nv <- length(vnam2) #number of objects
		A <- list()
		ir <- 0
		vnam<-vnam2
		ii<-which(vnam==nm|vnam==paste(nm,":",sep="")|paste(vnam,";",sep="")==nm)
		if(length(ii)==0)
				stop (paste(nm," >> name of object in the ",ADMBfilename,".dat file does not match ",ADMBfilename,".tpl file",sep=""))
		ir <- match(vnam[ii], ifile) # find the matching name in the ifile set
		if (ii!=nv) {
			irr <- match(vnam[ii+1], ifile)
		} else {
			irr <- length(ifile)+1 #next row
		}	
		ans<--999
		which(is.na(as.numeric(ifile[ir:irr]))==FALSE)
		irn<-ir+which(is.na(as.numeric(ifile[ir:irr]))==FALSE)-1
		for(i in 1:length(irn)){
			tt<-as.double(scan(fn, skip=irn[i]-1, nlines=1,quiet=TRUE, what=""))
			ans<-c(ans,as.numeric(na.omit(tt)))
		}
		ans<-ans[-1]
		
		return(ans)
	}	
	makeTransparent<-function(someColor, alpha=100)
	{
	  newColor<-col2rgb(someColor)
	  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
	    blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
	}



############################################################################################
## LOAD DATA
############################################################################################

	load(file.path(rec_file,"recruitment.Rdata"))

tt<-data.frame(
	num=1:length((AICtable[[1]]$R2)),
	model_rank=0,
	R2=(AICtable[[1]]$R2))



tt$mtype<-"Ricker"
tt$mtype[grep("BLM",AICtable[[1]]$model)]<-"Linear_Blag1"
cc<-grep("BLM",AICtable[[1]]$model)
cc1<-grep("LM",AICtable[[1]]$model)

tt$mtype[setdiff(cc1,cc)]<-"Linear"
tt$mtype2<-tt$mtype
tt$mtype[grep("Eat",AICtable[[1]]$model)]<-paste(tt$mtype[grep("Eat",AICtable[[1]]$model)],"_Eat",sep="")


tt<-tt[order(tt$R2),]
ntype<-length(unique(tt$mtype2))
plot(rev(tt$R2),ylim=c(0,1),col=rainbow(ntype)[as.numeric(as.factor(tt$mtype2))],pch="+")

# tt$mtype[grep("BH",AICtable[[1]]$model)]<-"Bev_holt"
factorW<-strsplit(tmp$model,split="_")
s<-1
nmod<-dim(AICtable[[s]])[1]
tmp<-AICtable[[s]]
for(i in 1:nmod){
	ttt<-rep(tmp$AICw_std[i],length(factorW[[i]]))
	names(ttt)<-factorW[[i]]
	factorW[[i]]<-ttt

}

tmp_p<-unlist(factorW)
par_wt<-tapply(tmp_p,names(tmp_p),sum)/sum(tmp$AICw_std)
barplot(par_wt,las=2)


############################################################
#Now graph results
############################################################
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
			}
		}
		quartz.save(file.path(fig.file,paste("Fig1_",sp[ss],".jpg",sep="")), type = "jpeg", dpi = 500)

		
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
			}
		}
		#figure1v2()
		ss<-2
		quartz.save(file.path(fig.file,paste("Fig1v2_",sp[ss],".jpg",sep="")), type = "jpeg", device = dev.cur(), dpi = 500)









