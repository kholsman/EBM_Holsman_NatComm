
############################################################################################
## R code to plot CEATTLE outputs
## Kirstin Holsman
## Feb 2015
##
############################################################################################


	rm(list=ls())
		# rm(list=ls());graphics.off(); setwd("/Users/kholsman/GitHub/CEATTLE/runs/aclim_00_Nov_2018_0/projections/aclim_00_Nov_2018_059_cf_0_5_12/Results")
############################################################################################
## Set up
############################################################################################
	wdpath<-getwd()

	WD<-getwd()
	print("current_directory")
	print(WD)
	update.est<-1 		# update figures (0=no, 1=yes)
	update.proj<-1 		# update figures (0=no, 1=yes)

	# update.figs<-1 		# update figures (0=no, 1=yes)
	update.data<-1 		# update data (0=no, 1=yes)
	#________________________________________
	# Read in the .config file
	#________________________________________
		tt<-read.csv("../../../tmpfile.txt",header=FALSE,colClasses="character")[[1]]
		nt<-length(grep("#",tt))
		tt_nm<-unlist(strsplit(tt[grep("#",tt)],split="#"))[seq(2,nt*2,2)]
		tt<-read.csv("../../../tmpfile.txt",comment="#",header=FALSE,colClasses="character")[[1]]
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

	# filename_root<-strsplit(output_path,"_0")[[1]]
	
	projname<-strsplit(wdpath,"projections")[[1]][2]
	projname<-strsplit(projname,"/")[[1]][2]
	# dir_est<-paste(filename_root,"_0",sep="")
	# out_2<-paste(filename_root,"_2",sep="")
	dir_est<-file.path("runs",filename)
	print(dir_est)

	filename_root<-paste0(filename,"/",projname)


	# source(file.path(DIR_main,"src/ceattle-master/Scripts/R_code/PLOT_CEATTLE_EST.R"))
	
	setwd("../")
	output_path<-getwd()
	setwd(WD)
	plot_file<-plot_fileJPG<-file.path(output_path,"R_figures")
	data_file<-WD
	if (file.exists(plot_file)){i=1} else {dir.create(plot_file)}
	if (file.exists(plot_fileJPG)){i=1} else {dir.create(plot_fileJPG)}
	
	source(file.path(model_path,"Scripts/R_code/PLOT_CEATTLE_FUN.R"))	
	runname<-unlist(strsplit(unlist(strsplit(output_path,filename))[2],"/projections/"))[2]


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

	#readdat(fn=file.path(DIR_main,"CEATTLE-master/Data/dat_input_files/diet.dat"),nm="mn_wt_stom")
	collrange1<-colorRampPalette(colors()[c(310,24)]) # grays

	load(file.path(DIR_main,dir_est,"results/CEATTLE_results.Rdata"))
	ceattle<-tmp
	nspp<-as.numeric(ceattle$nspp)
	nyrs<-as.numeric(ceattle$nyrs)
	styr<-as.numeric(ceattle$styr)
	years<-styr+1:nyrs -1

	ceattle<-assign.rec(target=ceattle,fn=file.path(DIR_main,dir_est,"results/ceattle_est.std"))	
	collrange1<-colorRampPalette(colors()[c(121,128)]) # blues
	collrange2_msm<-colorRampPalette(colors()[c(92,554)]) # orange
	collrange1<-colorRampPalette(colors()[c(121,173)]) # blues
	ylimm_all<-rbind(c(0,2e7),c(0,3e6),c(0,2e6))

	graphics.off()

	tmp<-unlist(strsplit(filename,split="_"))
	runname<-paste0(tmp[-length(tmp)],collapse="_")

############################################################################################
##  Projection mode
############################################################################################

		update.figs<-1 		# update figures (0=no, 1=yes)
		graphics.off()	
		# mod0_list<-dir(file.path(DIR_main,dir_est,"projections"))
		# mod2_list<-dir(file.path(DIR_main,out_2,"projections"))

		# mod_fn<-function(mod,flroot=runname){
		# 	tmp<-unlist(strsplit(mod,split="_"))
		# 	rec<-as.numeric(tmp[length(tmp)-1])
		# 	h<-as.numeric(tmp[length(tmp)])

		# 	ceattle_fut<-read.csv(file.path(DIR_main,dir_est,"projections",paste(flroot,mode,rec,h,sep="_"),"results/Future_report.rep"),sep=" ")
			
		# 	# ceattle_2_fut<-read.csv(file.path(DIR_main,out_2,"projections",paste(flroot,"2",rec,harvest,sep="_"),"results/Future_report.rep"),sep=" ")
		# 	return(ceattle_fut)
		# }

	if(length(grep("mc",output_path))>0){
		tt<-unlist(strsplit(output_path,split="_mc"))[[1]]
		hmode<-as.numeric(substring(tt,nchar(tt),nchar(tt)))
		rec<-as.numeric(substring(tt,nchar(tt)-2,nchar(tt)-2))
		mod<-paste0("_",rec,"_",hmode)
	}else{
		nkeep<-data.frame(hmode=-999,rec=-999,mod=-999)
		tn<-""
		j<-1
		for(i in (nchar(output_path)+0:-8)){
			tt<-(substring(output_path,i,i))
			if(tt!="_"){
				tn<-paste0(tt,tn)
			}else{
				nkeep[j]<-as.numeric(tn)
				tn<-""
				j<-j+1
			}
		}
		hmode<-nkeep$hmode
		rec<-nkeep$rec
		mod<-nkeep$mod
	}
		
		
		# ceattle_x<-mod_fn(mod=mod)
		ceattle_x<-read.csv(file.path(output_path,"results/Future_report.rep"),sep=" ")

		extract_ts<-function(dat1=ceatttle_x,types=c("SSB","rec","catch","Frate")){
			dat<-list()
			dat[["meta"]]<-dat1[,1:which(names(dat1)=="X")]
			yy<-substring(names(dat1[,(which(names(dat1)=="SSB.SSB0")+2):(grep("catch.biomass",names(dat1))-1)]),2,5)
			dat[["years"]]<-as.numeric(yy)
			for(type in types){
				if(type=="SSB") Bc<-(which(names(dat1)=="SSB.SSB0")+2):(grep("catch.biomass",names(dat1))-1)
				if(type=="rec") Bc<-(grep("recruits.numbers",names(dat1))+2):dim(dat1)[2]
				if(type=="catch") Bc<-(grep("catch.biomass",names(dat1))+2):(grep("recruits.numbers",names(dat1))-1)
				if(type=="Frate") Bc<-(grep("F_rate",names(dat1))+2):(grep("objective_fun",names(dat1))-1)
				dat[[type]]<-dat1[,Bc]; colnames(dat[[type]])<-yy
			}
			return(dat)

		}
		
			# load(file.path(DIR_main,"runs/aclim_nov2017_2","Recruitment_files/Rec_figs/recruitment.Rdata")); smry_RS2<-smry_RS
		load(file.path(DIR_main,dir_est,"Recruitment_files/Rec_figs/recruitment.Rdata")) # loads smry_RS
		# 
		tt<-read.csv(file.path(WD,"ceattle_R_projection.rep"),sep="")
		head(tt)
		proj<-list(ts=extract_ts(ceattle_x),smry_RS=smry_RS)
		save(proj,file=file.path(WD,"proj.Rdata"))
		
	############################################################################################
	##  Projected RS function
	############################################################################################

		plot_RS<-function(cexx=1.2,s=1,mode=0,dat1=ceattle_x,div=1e6,smry_RS_use=smry_RS,yend=rev(years)[1],coluse=c("orange",col1(6)))
		{
				Rc<-(grep("recruits.numbers",names(dat1))+2):dim(dat1)[2]
				Bc<-(which(names(dat1)=="SSB.SSB0")+2):(grep("catch.biomass",names(dat1))-1)
				rec<-dat1[,Rc]
				SSB<-dat1[,Bc]
				lab<-dat1[,which(names(dat1)=="SSB.SSB0")]
				fr<-which(lab=="SSB")
				fr0<-which(lab=="SSB0")
				dat0<-dat1[fr0,]
				dat<-dat1[fr,]

				
				i<-1;n<-1
				# tmp<-"/Users/kholsman/Documents/GitHub/CEATTLE/docs/2016_MSMassmnt/Nov_files/Assmnt_runs/Nov_assmt_update_0/Recruitment_files"
				parm<-read.csv(file.path(DIR_main,dir_est,paste("Recruitment_files/RS_fits/ceattle_recruit_RS",s,"_0.std",sep="")),header=T,sep="")
				# parm<-read.csv(file.path(tmp,paste("RS_fits/ceattle_recruit_RS",s,"_0.std",sep="")),header=T,sep="")
								
				sub.Rec<-dat0[dat0$fut_simulation==n&dat0$species==s,Rc][i,]
				sub.y<-grep(yend,names(sub.Rec))
				sub.B<-dat0[dat0$fut_simulation==n&dat0$species==s,Bc][i,]
				
				SSB1<-as.numeric(sub.B)[1:sub.y][-sub.y]
				Rec1<-as.numeric(sub.Rec)[1:sub.y][-1]
				if(s==1) SSB<-as.numeric(ceattle$BiomassSSB_1)[-sub.y]
				if(s==2) SSB<-as.numeric(ceattle$BiomassSSB_2)[-sub.y]
				if(s==3) SSB<-as.numeric(ceattle$BiomassSSB_3)[-sub.y]
				Rec<-exp(as.numeric(ceattle$logR_obs[s,-1]))
				xxval<-pretty(seq(0,max(SSB),1e3))
				yyval<-pretty(seq(0,max(Rec),1e3))

				plot(SSB,Rec,xlim=c(0,max(SSB,na.rm=T)),ylim=c(0,max(Rec,na.rm=T)),las=2,axes=F,col=F,ylab="",xlab="")
				# points(smry_RS_use[[s]]$SSB.y.1.,smry_RS_use[[s]]$R_obs)
				# smry_RS_use
				nyr<-length(SSB)
				col<-rep(colors()[300],length(1:sub.y))
				col[as.numeric(scale(ceattle$TempC[1,]))<=-1]<-"blue"
				col[as.numeric(scale(ceattle$TempC[1,]))>=1]<-"red"

				text(SSB,Rec,lab=substr(as.character(years),3,4)[-length(years)],cex=cexx,col=col[-length(years)])
				axis(1,at=xxval,lab=xxval/div)
				axis(2,at=yyval,lab=yyval/div,las=2)

				# points(R_obs~SSB.y.1.,data=smry_RS_use[[1]],col="red")
				# points(mnRS_Rec~SSB.y.1.,data=smry_RS_use[[1]],col="blue",pch=16)
				SSB_hat<-seq(100,6e6,1e4)

				aa_c<-exp(parm[2,3]);bb_c<-exp(parm[3,3])
				aa_c.pl<-exp(parm[2,3]+1.95*parm[2,4]/sqrt(nyr));aa_c.mn<-exp(parm[2,3]-1.95*parm[2,4]/sqrt(nyr))
				bb_c.mn<-exp(parm[3,3]+1.95*parm[3,4]/sqrt(nyr));bb_c.pl<-exp(parm[3,3]-1.95*parm[3,4]/sqrt(nyr))
				rec_hat<-exp(aa_c-bb_c*SSB_hat+log(SSB_hat))  # mueter formulation
				rec_hat.pl<- exp(aa_c.pl-bb_c.pl*SSB_hat+log(SSB_hat)) 
				rec_hat.mn<- exp(aa_c.mn-bb_c.mn*SSB_hat+log(SSB_hat)) 
				# rec_hat<- exp(log(aa_c*SSB_hat) -bb_c*SSB_hat)
				 
				# rec_hat.pl<- exp(log(aa_c.pl*SSB_hat) -bb_c.pl*SSB_hat+(parm[1,3]))
				# rec_hat.mn<- exp(log(aa_c.mn*SSB_hat) -bb_c.mn*SSB_hat-(parm[1,3]))

				lines(SSB_hat,rec_hat)
				lines(SSB_hat,rec_hat.pl,lty=2)
				lines(SSB_hat,rec_hat.mn,lty=2)
				
		}
		fig1<-function(){
			quartz(width=6,height=9)
			par(mfrow=c(3,2))
			par(mar=c(3,4,1,1)) # margins of graph: (bottom,left, top, right)
			par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
			par(oma=c(2,0,2,2))# outer margins of graph: (bottom,left, top, right)
			par(mfrow=c(3,1))

			plot_RS(dat1=ceattle_x,smry_RS_use=smry_RS,mode=mode,s=1)
			# plot_RS(dat1=ceattle_x,smry_RS_use=smry_RS2,mode=mode,s=1)
			mtext(side=3,outer=F,"single-species"); mtext(side=2, line=-1,outer=T,"Age 1 recruitment (billions)")
			mtext(side=1,outer=T,"Spawning biomass (million t)")
			mtext(side=4,outer=F,"Walleye pollock",line=1)
				
			plot_RS(s=2,dat1=ceattle_x,smry_RS_use=smry_RS,mode=mode)#;mtext(side=3,outer=F,"single-species"); mtext(side=2, line=-1,outer=T,"Age 1 recruitment (billions)")
			mtext(side=4,outer=F,"Pacific cod",line=1)

			plot_RS(s=3,dat1=ceattle_x,smry_RS_use=smry_RS,mode=mode)#;mtext(side=3,outer=F,"single-species"); mtext(side=2, line=-1,outer=T,"Age 1 recruitment (billions)")
			mtext(side=4,outer=F,"Arrowtooth flounder",line=1)
		}	
		graphics.off()
		fig1()
			if(update.figs==1)				quartz.save(file=file.path(plot_fileJPG,"Recruitment_SSB_all.pdf"),type="pdf",dpi=500)

	############################################################################################
	##  Projected SSB
	############################################################################################
		hcrset<-unique(ceattle_x$control_rule)
		graphics.off()
		plotcols<-c(colors()[320],"black")
		plotcols<-c("orange",col1(10)[3])
		btargets<-unique(ceattle_x$B_target)


		plot_TS<-function(type="SSB",est=ceattle,lgndT1=F,lgndT2=F,ltyy=c(2,rep(1),15),bt=0.4,div=1e6,div2=1e4,s=1,
			ylimm2=FALSE,xlimm=c(1979,2100),dat1=ceattle_x,yrs=yrs_in,hcrs=hcrset,lgnd1_loc="topleft",lgnd2_loc="bottomleft",coll=plotcols ){
			
			if(type=="SSB") {
				Bc<-(which(names(dat1)=="SSB.SSB0")+2):(grep("catch.biomass",names(dat1))-1)
				if(s==1) estdat<-as.numeric(est$BiomassSSB_1)
				if(s==2) estdat<-as.numeric(est$BiomassSSB_2)
				if(s==3) estdat<-as.numeric(est$BiomassSSB_3)
			}
			if(type=="rec") Bc<-(grep("recruits.numbers",names(dat1))+2):dim(dat1)[2]
			if(type=="catch") Bc<-(grep("catch.biomass",names(dat1))+2):(grep("recruits.numbers",names(dat1))-1)
			if(type=="Frate") Bc<-(grep("F_rate",names(dat1))+2):(grep("objective_fun",names(dat1))-1)
			
			# dat1$control_rule[rr]

			rr<-which(dat1$SSB.SSB0=="SSB0"&dat1$species==s)

			maxx<-max(unlist(c(dat1[rr,Bc],dat1[rr,Bc])),na.rm=T)
			if(ylimm2!=FALSE) maxx<-ylimm2
			
			rr<-rr[which(dat1$control_rule[rr]==hcrs[1])]
			if(length(rr)>1) rr<-rr[1]

			plot(yrs,as.numeric(dat1[rr,Bc]),type="l",ylim=c(0,maxx),col=coll[1],lwd=1,lty=ltyy[1],ylab="",xlab="",axes=F,xlim=xlimm)
			axis(1,at=pretty(yrs))
			axis(2,at=pretty(seq(0,maxx,div2)),lab=pretty(seq(0,maxx,div2))/div,las=2)
			abline(v=2016,lty=3)
				# lines(yrs,dat1[[2]][rr,Bc],type="l",lwd=2,lty=ltyy[1],col=coll[2])
			nHRC<-length(hcrs)
			mods<-unique(dat1$fut_simulation)
			nMod<-length(mods)
			for(m in 1:nMod){
				# rr<-which(dat1$B_target==bt&dat1$species==s&dat1$fut_simulation==mods[m])
				# rr0<-which(dat1$B_target==0&dat1$species==s&dat1$fut_simulation==mods[m])
				rr<-which(dat1$SSB.SSB0=="SSB"&dat1$species==s&dat1$fut_simulation==mods[m])
				rr0<-which(dat1$SSB.SSB0=="SSB0"&dat1$species==s&dat1$fut_simulation==mods[m])
				for(i in 1:nHRC){	
					rr_all<-rr[which(dat1$control_rule[rr]==hcrs[i])]
					rr_all0<-rr0[which(dat1$control_rule[rr0]==hcrs[i])]
				
					rr<-rr_all0[1];lines(yrs,dat1[rr0,Bc],col=coll[1],lwd=2,lty=ltyy[i+1])
					rr<-rr_all[1];lines(yrs,dat1[rr,Bc],col=coll[2],lwd=2,lty=ltyy[i+1])

					if(length(rr_all)>1){ 
						rr<-rr_all0[2];lines(yrs,dat1[rr0,Bc],col=coll[1],lwd=2,lty=ltyy[i+1])
						rr<-rr_all[2];lines(yrs,dat1[rr,Bc],col=coll[2],lwd=2,lty=ltyy[i+1])
					}
						# lines(yrs,dat1[[2]][rr,Bc],col=coll[2],type="l",lwd=1,lty=ltyy[i+1])
				}
			}
			if(type=="SSB") lines(1978+1:length(estdat),estdat,col="gray",lty=1,lwd=1)

			
			if(lgndT1==T)
					legend(lgnd1_loc, c("unfished","fished"), lty =1,col=coll, lwd=2,adj = c(0, 0.6),box.lty=0)
			if(lgndT2==T){
				ex.cs1 <- expression(SSB[0],  SSB[40],SSB[mMSY])  # 2 ways
				legend(lgnd2_loc, ex.cs1, lty = ltyy,col=c(rep(coll[2],3)), lwd=2,adj = c(0, 0.6),box.lty=0)

			}

		}
		
			# b0set[1,as.numeric(mode)]
		if(mode==0) b0set<-c(5386284.5 , 447268.5,  493190.2)
		if(mode==2) b0set<-c(3812316.4,398083.9,460652.7)
		
		quartz(width=8,height=6)
		mset<-ceattle_x
		yrs_in<-as.numeric(substring(names(ceattle_x)[(which(names(ceattle_x)=="SSB.SSB0")+2):(which(names(ceattle_x)=="catch.biomass.")-1)],2,5))


		
			par(mar=c(3,3,1,1)) # margins of graph: (bottom,left, top, right)
			par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
			par(oma=c(2,2,2,0))# outer margins of graph: (bottom,left, top, right)
			par(mfrow=c(3,1))
				plot_TS(s=1,dat1=mset,hcrs=hcrset);mtext(side=3,line=0,"a) Walleye pollock",adj=0.01,cex=.7,font=2)
				bline<-b0set[1]; abline(h= c(bline,bline*0.4),lty=c(1,2))	
				plot_TS(s=2,dat1=mset,lgndT1=T,hcrs=hcrset);mtext(side=3,line=0,"b) Pacific cod",adj=0.01,cex=.7,font=2)
				bline<-b0set[2]; abline(h= c(bline,bline*0.4),lty=c(1,2))
				plot_TS(s=3,dat1=mset,lgndT2=F,hcrs=hcrset);mtext(side=3,line=0,"c) Arrowtooth flounder",adj=0.01,cex=.7,font=2)
				bline<-b0set[3]; abline(h= c(bline,bline*0.4),lty=c(1,2))
				mtext(side=2,outer=T,"Spawning biomass (million t)")
				mtext(side=1,outer=T,"Year")
			if(update.figs==1)
						quartz.save(file=file.path(plot_fileJPG,"SSB_timeseries.pdf"),type="pdf",dpi=500)
			
			
		
	############################################################################################
	##  Projected Rec
	############################################################################################
		
	
	
		quartz(width=8,height=6)
	
		par(mar=c(3,3,1,1)) # margins of graph: (bottom,left, top, right)
		par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
		par(oma=c(2,2,2,0))# outer margins of graph: (bottom,left, top, right)
		par(mfrow=c(3,1))
			plot_TS(s=1,type="rec",dat1=mset,hcrs=hcrset);mtext(side=3,line=0,"a) Walleye pollock",adj=0.01,cex=.7,font=2)
			plot_TS(type="rec",s=2,lgndT1=T,dat1=mset,hcrs=hcrset);mtext(side=3,line=0,"b) Pacific cod",adj=0.01,cex=.7,font=2)
			plot_TS(type="rec",s=3,lgndT2=F,dat1=mset,hcrs=hcrset);mtext(side=3,line=0,"c) Arrowtooth flounder",adj=0.01,cex=.7,font=2)
			mtext(side=2,outer=T,"Age 1 recruitment (billions)")
			mtext(side=1,outer=T,"Year")
		if(update.figs==1)
					quartz.save(file=file.path(plot_fileJPG,"REC_timeseries.pdf"),type="pdf",dpi=500)
			
		
	quartz(width=8,height=6)
	
		par(mar=c(3,3,1,1)) # margins of graph: (bottom,left, top, right)
		par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
		par(oma=c(2,2,2,0))# outer margins of graph: (bottom,left, top, right)
		par(mfrow=c(3,1))
			plot_TS(s=1,type="Frate",ylimm2=1,dat1=mset,hcrs=hcrset);mtext(side=3,line=0,"a) Walleye pollock",adj=0.01,cex=.7,font=2)
			plot_TS(type="Frate",s=2,lgndT1=T,dat1=mset,hcrs=hcrset);mtext(side=3,line=0,"b) Pacific cod",adj=0.01,cex=.7,font=2)
			plot_TS(type="Frate",s=3,lgndT2=F,dat1=mset,hcrs=hcrset);mtext(side=3,line=0,"c) Arrowtooth flounder",adj=0.01,cex=.7,font=2)
			mtext(side=2,outer=T,"Fishing mortality (F)")
			mtext(side=1,outer=T,"Year")
		if(update.figs==1)
					quartz.save(file=file.path(plot_fileJPG,"Frate_timeseries_v2.pdf"),type="pdf",dpi=500)
			
			
	
	############################################################################################
	##  Projected catch
	############################################################################################
		
	
	
		quartz(width=8,height=6)
		
			par(mar=c(3,3,1,1)) # margins of graph: (bottom,left, top, right)
			par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
			par(oma=c(2,2,2,0))# outer margins of graph: (bottom,left, top, right)
			par(mfrow=c(3,1))
				plot_TS(type="catch",s=1,ylimm2=4e6,dat1=mset,hcrs=hcrset);mtext(side=3,line=0,"a) Walleye pollock",adj=0.01,cex=.7,font=2)
				plot_TS(type="catch",s=2,lgndT1=T,ylimm2=.4e6,dat1=mset,hcrs=hcrset);mtext(side=3,line=0,"b) Pacific cod",adj=0.01,cex=.7,font=2)
				plot_TS(type="catch",s=3,lgndT2=F,ylimm2=.06e6,dat1=mset,hcrs=hcrset);mtext(side=3,line=0,"c) Arrowtooth flounder",adj=0.01,cex=.7,font=2)
				mtext(side=2,outer=T,"Yield (million t)")
				mtext(side=1,outer=T,"Year")
		if(update.figs==1)
						quartz.save(file=file.path(plot_fileJPG,"catch_timeseries.pdf"),type="pdf",dpi=500)
				
					
			graphics.off()

		
	############################################################################################
	##  Overall summary plots
	############################################################################################


		# }

		# plot all patterns
		read.n.plot2<-function(dat=ceattle_x,fl="",SaveIt=TRUE,coluse=c("orange",col1(6)),ltyy1=1,alpha1=50,plot.path=plot_file){
			
			snames<-c("Pollock","P. cod","arrowtooth")
	
			tmp<-dat
			scenarios<-unique(tmp$fut_simulation)
			# print(scenarios)
			ltyyuse<-rep(ltyy1,max(scenarios))
			# dat<-tmp

			Fc<-(which(names(dat)=="F_rate")+2):(which(names(dat)=="objective_fun")-1)
			Bc<-(which(names(dat)=="SSB.SSB0")+2):(grep("catch.biomass",names(dat))-1)
			Rc<-(grep("recruits.numbers",names(dat))+2):dim(dat)[2]
			Cc<-(grep("catch.biomass",names(dat))+2):(grep("recruits.numbers",names(dat))-1)
			lab<-dat[,which(names(dat)=="SSB.SSB0")]
			fr<-which(lab=="SSB")
			fr0<-which(lab=="SSB0")
			dat0<-dat[fr0,]
			dat<-dat[fr,]
			rec<-dat[,Rc]
			Frate<-dat[,Fc]
			SSB<-dat[,Bc]
			catch<-dat[,Cc]
			ltyy<-ltyyuse
				itr<-dim(dat0[dat0$fut_simulation==scenarios[1]&dat0$species==1,Rc])[1]
			sub.yr<-as.numeric(substr(names(dat[,Rc]),2,5))
			s<-1;i<-1;n<-1
			quartz(width=7,height=6)
			par(mfrow=c(4,3))
			par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
			par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
			par(oma=c(2,3.5,4,2))# outer margins of graph: (bottom,left, top, right)

			for (s in 1:3){

				ylimm<-1.1*max.na(as.numeric(apply(dat[dat$species==s,Rc],2,max.na)))
				for(i in 1:itr){
					for(n in scenarios){
						sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s,Rc][i,]
						sub.yr<-as.numeric(substr(names(sub.sub),2,5))
						if(n==scenarios[1]&i==1){
							plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),col=makeTransparent(coluse[n],alpha=alpha1))
							mtext(snames[s],side=3,line=.5)
							if(s==1)
								mtext("Recruits",side=2, outer=FALSE,line=2)
						}else{
							lines(sub.yr,as.numeric(sub.sub),type="l",col=makeTransparent(coluse[n],alpha=alpha1))
						}
					}
					for(n in scenarios){
						sub.sub<-dat[dat$fut_simulation==n&dat$species==s,Rc][i,]
						lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[n],lty=ltyy[n])
					}
				}
					
			}
			for (s in 1:3){
				ylimm<-1.1*max.na(as.numeric(apply(dat0[dat0$species==s,Bc],2,max.na)))
				for(i in 1:itr){
					for(n in scenarios){
						sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s,Bc][i,]
						# sub.yr<-as.numeric(substr(names(sub.sub),2,5))
						if(n==scenarios[1]&i==1){
							plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),col=makeTransparent(coluse[n],alpha=alpha1))
							if(s==1)
							mtext("SSB",side=2, outer=FALSE,line=2)
							}else{
							lines(sub.yr,as.numeric(sub.sub),type="l",lty=ltyy[n],col=makeTransparent(coluse[n],alpha=alpha1))
						}
					}
					for(n in scenarios){
						sub.sub<-dat[dat$fut_simulation==n&dat$species==s,Bc][i,]
						lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[n],lty=ltyy[n])
						
					}
				}	
			}
			for (s in 1:3){
				ylimm<-1.1*max.na(as.numeric(apply(dat[dat$species==s,Cc],2,max.na)))
				for(i in 1:itr){
					for(n in scenarios){
						#sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s,Cc][i,]
						sub.sub<-dat[dat$fut_simulation==n&dat$species==s,Cc][i,]
						# sub.yr<-as.numeric(substr(names(sub.sub),2,5))
						if(n==scenarios[1]&i==1){
							plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),lty=ltyy[n],col=coluse[n])
							if(s==1)
							mtext("Catch",side=2, outer=FALSE,line=2)
							}else{
							lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[n],lty=ltyy[n])
						}
					}
					
				}		
			}
			for (s in 1:3){
				ylimm<-1.1*max.na(as.numeric(apply(dat[dat$species==s,Fc],2,max.na)))
				for(i in 1:itr){
					for(n in scenarios){
						#sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s,Fc][i,]
						sub.sub<-dat[dat$fut_simulation==n&dat$species==s,Fc][i,]
						# sub.yr<-as.numeric(substr(names(sub.sub),2,5))
						if(n==scenarios[1]&i==1){
							plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),lty=ltyy[n],col=coluse[n])
							if(s==1)
							mtext("Frate",side=2, outer=FALSE,line=2)
							}else{
							lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[n],lty=ltyy[n])
						}
					}
					
				}	
			}
			# mtext(fl,outer=TRUE,side=3,line=2)
			

			mtext(filename_root,outer=TRUE,side=3,line=2,font=2)

			if(SaveIt==TRUE)
				quartz.save(,type="pdf",file=file.path(plot.path,paste("Allplot_",fl,".pdf",sep="")))

		}
		
		read.n.plot2(coluse=col1(27),dat=ceattle_x)
		if(update.figs==1)	
			quartz.save(file=file.path(plot_fileJPG,"Summary_proj.pdf"),type="pdf",dpi=500)
		if(dir.exists(file.path(path,"projections/summaryFigs"))==FALSE)
			dir.create(file.path(path,"projections/summaryFigs"))
		if(update.figs==1)	
			quartz.save(
				file=file.path(path,paste0("projections/summaryFigs/Summary_proj_",projname,".pdf")),
				type="pdf",dpi=500)
		graphics.off()
		save.image(file.path(plot_fileJPG,"proj.Rdata"))





	