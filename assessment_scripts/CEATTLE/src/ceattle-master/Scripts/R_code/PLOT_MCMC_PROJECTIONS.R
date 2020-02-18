##_____________________________________________________
## Kirstin Holsman
## Mar 2015
## 
## R file to plot and summarize MCMC results
##_____________________________________________________

	# cd /Users/kkari/Dropbox/CEATTLE-master/ceattle-master/ceattle_0/projections/MCMC_ceattle_0_1_2/newest/results
	rm(list=ls())
	# setwd("/Users/kkari/Dropbox/CEATTLE-master/ceattle-master/ceattle_0/projections/MCMC_ceattle_0_2_2/newest/results")
	# setwd("/Users/kkari/Dropbox/CEATTLE-master/ceattle-master/ceattle_2/projections/MCMC_ceattle_2_2/newest/results")
	# setwd("/Users/kkari/Dropbox/CEATTLE-master/ceattle-master/ceattle_2/projections/ceattle_2_0_2/results")
	# setwd("/Users/kkari/Dropbox/AFSC_models/CEATTLE/CEATTLE_outputs/CEATTLE_newest/")
	source("/Users/kkari/Documents/science/R_funKir/funKir.R")
	path1<-path<-getwd()
	root1<-root<-(unlist(strsplit(path,split="/CEATTLE_outputs")))[1]
	load("ceattle_2/results/CEATTLE_RS_2/Recruitment_files/Rec_figs/recruitment.Rdata")
	AICtable_2<-AICtable
	covars_2<-covars
	covuse.all_2<-covuse.all
	covuse_2<-covuse
	TopAIC.txt_2<-TopAIC.txt
	TopR2.txt_2<-TopR2.txt
	smry_RS_2<-smry_RS
	TopVals_2<-data.frame(TopAIC.txt ,TopR2.txt,topAIC.txt,topn,topname,topR2n,topricker)

	load("ceattle_0/results/CEATTLE_RS_0/Recruitment_files/Rec_figs/recruitment.Rdata")
	path<-path1
	root<-root1
	AICtable_0<-AICtable
	covars_0<-pbinef
	covuse.all_0<-covuse.all
	covuse_0<-covuse
	TopAIC.txt_0<-TopAIC.txt
	TopR2.txt_0<-TopR2.txt
	smry_RS_0<-smry_RS
	TopVals_0<-data.frame(TopAIC.txt ,TopR2.txt,topAIC.txt,topn,topname,topR2n,topricker)

	outfile<-"/Users/kkari/Dropbox/Manuscripts/TMSM_climate_projections/Analysis"
	load(file.path(root,"CEATTLE-master/Data/ROMS_NPZ_data/NEWEST/ROMS_NPZ_covars.Rdata"))

	col1<-colorRampPalette(colors()[c(573,88,129)])
	# plot all covariates
	quartz(width=9,height=9)
		par(mfrow=c(5,3))
		par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
		par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
		par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)
		cnums<-seq(1,75,15)
		for(ii in 1:length(cnums)){
			for(covn in (cnums[ii]+(1:15))){
				test<-data.frame(yr=c((ROMS_NPZ_covars[[1]][,1]),(ROMS_NPZ_covars[[2]][,1])),
					dat=c((ROMS_NPZ_covars[[1]][,covn]),(ROMS_NPZ_covars[[2]][,covn])))
					plot(test,type="l",ylim=c(-4,4),col=col1(5)[1])
					if(covn==(cnums[ii]+1)){
						legend("bottomleft",names(ROMS_NPZ_covars)[2:4],col=col1(3),lwd=1,box.lty=0)
					}
				test<-data.frame(yr=c((ROMS_NPZ_covars[[1]][,1]),(ROMS_NPZ_covars[[2]][,1])),
					dat=c((ROMS_NPZ_covars[[1]][,covn]),(ROMS_NPZ_covars[[3]][,covn])))
					lines(test,type="l",ylim=c(-4,4),col=col1(5)[2])

				test<-data.frame(yr=c((ROMS_NPZ_covars[[1]][,1]),(ROMS_NPZ_covars[[2]][,1])),
					dat=c((ROMS_NPZ_covars[[1]][,covn]),(ROMS_NPZ_covars[[4]][,covn])))
					lines(test,type="l",ylim=c(-4,4),col=col1(5)[3])

				test<-data.frame(yr=c((ROMS_NPZ_covars[[1]][,1]),(ROMS_NPZ_covars[[2]][,1])),
					dat=c((ROMS_NPZ_covars[[1]][,covn]),(ROMS_NPZ_covars[[5]][,covn])))
					lines(test,type="l",ylim=c(-4,4),col=col1(5)[4])
				test<-data.frame(yr=c((ROMS_NPZ_covars[[1]][,1]),(ROMS_NPZ_covars[[2]][,1])),
					dat=c((ROMS_NPZ_covars[[1]][,covn]),(ROMS_NPZ_covars[[6]][,covn])))
					lines(test,type="l",ylim=c(-4,4),col=col1(5)[6])
				abline(h=mean.na(ROMS_NPZ_covars[[1]][,covn]),lty=2,col=colors()[320])

				mtext(paste(covn-1,":",names(ROMS_NPZ_covars[[1]])[covn]),outer=FALSE,side=3,line=-1.5,adj=.01)
				abline(v=2012)

			}
			quartz.save(,type="pdf",file=file.path(outfile,paste("covars",ii,".pdf",sep="")))
		}
		quartz.save(,type="pdf",file=file.path(outfile,paste("covars",ii,".pdf",sep="")))
		
	# plot(covuse[[1]][,1],covuse[[1]][,10],type="l",xlim=c(1980,2050))
	# lines(CCCMA[,1],CCCMA2.s[,10])
	# lines(MIROC2.s[,1],MIROC2.s[,10])
	# lines(ECHOG2.s[,1],ECHOG2.s[,10])


	par(mfrow=c(3,2))
		par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
		par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
		par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)
	for(s in 1:3){
		plot(smry_RS_0[[s]][,grep("TopAICcov_",names(smry_RS_0[[s]]))],smry_RS_0[[s]]$R_obs-smry_RS_0[[s]]$mnRec,pch="+")
		text(smry_RS_0[[s]][,grep("TopAICcov_",names(smry_RS_0[[s]]))],smry_RS_0[[s]]$R_obs-smry_RS_0[[s]]$mnRec,substr(smry_RS_2[[s]][,2],3,4))

		plot(smry_RS_0[[s]][,grep("R2cov_",names(smry_RS_0[[s]]))],smry_RS_0[[s]]$R_obs-smry_RS_0[[s]]$mnRec,pch="+")
		text(smry_RS_0[[s]][,grep("R2cov_",names(smry_RS_0[[s]]))],smry_RS_0[[s]]$R_obs-smry_RS_0[[s]]$mnRec,substr(smry_RS_2[[s]][,2],3,4))

	}
	for(s in 1:3){
		plot(smry_RS_2[[s]][,grep("TopAICcov_",names(smry_RS_2[[s]]))],smry_RS_2[[s]]$R_obs-smry_RS_2[[s]]$mnRec,pch="+")
		text(smry_RS_2[[s]][,grep("TopAICcov_",names(smry_RS_2[[s]]))],smry_RS_2[[s]]$R_obs-smry_RS_2[[s]]$mnRec,substr(smry_RS_2[[s]][,2],3,4))

		plot(smry_RS_2[[s]][,grep("R2cov_",names(smry_RS_2[[s]]))],smry_RS_2[[s]]$R_obs-smry_RS_2[[s]]$mnRec,pch="+")
		text(smry_RS_2[[s]][,grep("R2cov_",names(smry_RS_2[[s]]))],smry_RS_2[[s]]$R_obs-smry_RS_2[[s]]$mnRec,substr(smry_RS_2[[s]][,2],3,4))

	}

	for(s in 1:3){
		plot(smry_RS_0[[s]][,grep("TopAICcov_",names(smry_RS_0[[s]]))],smry_RS_0[[s]]$R_obs-smry_RS_0[[s]]$mnRS_Rec,pch="+")
		text(smry_RS_0[[s]][,grep("TopAICcov_",names(smry_RS_0[[s]]))],smry_RS_0[[s]]$R_obs-smry_RS_0[[s]]$mnRS_Rec,substr(smry_RS_2[[s]][,2],3,4))

		plot(smry_RS_0[[s]][,grep("R2cov_",names(smry_RS_0[[s]]))],smry_RS_0[[s]]$R_obs-smry_RS_0[[s]]$mnRS_Rec,pch="+")
		text(smry_RS_0[[s]][,grep("R2cov_",names(smry_RS_0[[s]]))],smry_RS_0[[s]]$R_obs-smry_RS_0[[s]]$mnRS_Rec,substr(smry_RS_2[[s]][,2],3,4))

	}
	for(s in 1:3){
		plot(smry_RS_2[[s]][,grep("TopAICcov_",names(smry_RS_2[[s]]))],smry_RS_2[[s]]$R_obs-smry_RS_2[[s]]$mnRS_Rec,pch="+")
		text(smry_RS_2[[s]][,grep("TopAICcov_",names(smry_RS_2[[s]]))],smry_RS_2[[s]]$R_obs-smry_RS_2[[s]]$mnRS_Rec,substr(smry_RS_2[[s]][,2],3,4))

		plot(smry_RS_2[[s]][,grep("R2cov_",names(smry_RS_2[[s]]))],smry_RS_2[[s]]$R_obs-smry_RS_2[[s]]$mnRS_Rec,pch="+")
		text(smry_RS_2[[s]][,grep("R2cov_",names(smry_RS_2[[s]]))],smry_RS_2[[s]]$R_obs-smry_RS_2[[s]]$mnRS_Rec,substr(smry_RS_2[[s]][,2],3,4))

	}
##_____________________________________________________
## Functions
##_____________________________________________________

	read_estrep<-function (fn,printt=TRUE) {
		    ifile <- scan(fn, what = "character", flush = T, blank.lines.skip = T, quiet = T)
		    tmp<-as.numeric(ifile)
		    idx<-which(is.na(tmp))
		    
		    idy<-idx
		   # idy <- which(idx2)
		    datnum <- which(idx == FALSE)
		    labnum <- which(idx == TRUE)
		    vnam <- ifile[idx]
		    special_vnam<-which(vnam=="ntemp_scen")
		    tmp <- rep(0, length(vnam))
		    tt <- strsplit(vnam, split = "#")
		    tmp[(is.na(as.numeric(unlist(tt))))]<-1
		    vnam2 <- vnam[tmp == 1]
		    labnum <- match(vnam2, ifile)
		    ifilet <- strsplit(ifile, split = "#")
		    vnamt <- vnam2
		    #for (i in 1:length(ifile)) 
		    #	ifile[i] <- ifilet[[i]][length(ifilet[[i]])]
		    ifile<-unlist(ifilet)
		    vnam2 <- na.omit(vnam2)
		    nv <- length(vnam2)
		    A <- list()
		    ir <- 0
		    vnam <- vnam2
		    all_dat<-list()

		    for(ii in 1:length(vnam))
		    {
		    	ir<-which(ifile==vnam[ii])
		    	if (ii != nv) 
		    	{
		     	   irr <- which(ifile==vnam[ii + 1])
		    	}else {
		        	irr <- length(ifile) + 1
		    	}
		    	irn <- ir + which(is.na(as.numeric(ifile[ir:irr])) == FALSE) - 1
		    	nr<-length(irn)
		    	nc<-0
		    	for (r in 1:nr){
		    		tt <- as.double(scan(fn, skip = irn[r] - 1, nlines = 1, quiet = TRUE, what = ""))
		    		nc<-max(nc,length(tt))
		    	}
		    	ans<-matrix(-999,nr,nc)
		    	for (r in 1:nr)
		    	{
		    		 tt<- as.double(scan(fn, skip = irn[r] - 1, nlines = 1, quiet = TRUE, what = ""))
		    		 if(length(tt)==length(ans[r,])){
		    		 	ans[r,]<-tt
		    		 }else{
		    		 	ans[r,1:length(tt)]<-tt
		    		 }
		    		 
		    	}
					
		        eval(parse(text=paste("all_dat$",vnam[ii],"<-ans",sep="")))
		        if(printt==TRUE)
		        	print(paste(round(ii/nv,2)*100,"% complete :",vnam[ii]))
			}

		    return(all_dat)
	}

	assign.rec<-function(target=ceattle_0,fn=file.path(root,"ceattle_0/results/ceattle.std")){
		tmp<-read.rec(fn)
		target$ln_mn_rec<-tmp$ln_mn_rec
		target$ln_mn_rec.se<-tmp$ln_mn_rec.se
		target$rec_dev<-tmp$rec_dev
		target$rec_dev.se<-tmp$rec_dev.se
		target$logR_obs<-tmp$logR_obs
		target$logR_obs.plus<-tmp$logR_obs.plus
		target$logR_obs.minus<-tmp$logR_obs.minus
		target$logR_obs.5.plus<-tmp$logR_obs.5.plus
		target$logR_obs.5.minus<-tmp$logR_obs.5.minus

		target$nages_1<-length(target$AvgN_1[1,])

		target$nages_2<-length(target$AvgN_2[1,])

		target$nages_3<-length(target$AvgN_3[1,])
		return(target)
	}

	read.rec<-function(fn){
		rec_dev<-matrix(0,nspp,nyrs)
		rec_dev.se<-matrix(0,nspp,nyrs)
		tmpt<-read.csv(fn,sep="")
		tmpr<-tmpt[tmpt[,2]=="rec_dev",3]
		ny<-length(tmpr)/3
		ln_mn_rec<-tmpt[tmpt[,2]=="ln_mn_rec",3]
		ln_mn_rec.se<-tmpt[tmpt[,2]=="ln_mn_rec",4]/sqrt(ny)
		for(s in 1:nspp){
			end<-ny*s
			st<-end-ny+1
			rec_dev.se[s,]<-tmpt[tmpt[,2]=="rec_dev",4][st:end]/sqrt(ny)
			rec_dev[s,]<-tmpt[tmpt[,2]=="rec_dev",3][st:end]
		}
		logR_obs<-matrix(0,nspp,nyrs)
		logR_obs.plus<-matrix(0,nspp,nyrs)
		logR_obs.minus<-matrix(0,nspp,nyrs)
		logR_obs.5.plus<-matrix(0,nspp,nyrs)
		logR_obs.5.minus<-matrix(0,nspp,nyrs)

		for (s in 1:nspp){
		 logR_obs[s,] = (ln_mn_rec[s] + rec_dev[s,] )
		 logR_obs.plus[s,] = (ln_mn_rec[s]+1.96*ln_mn_rec.se[s] + rec_dev[s,]+1.96*rec_dev.se[s,] )
		 logR_obs.minus[s,] = (ln_mn_rec[s]-1.96*ln_mn_rec.se[s] + rec_dev[s,]-1.96*rec_dev.se[s,] )
		 logR_obs.5.plus[s,] = (ln_mn_rec[s]+1*ln_mn_rec.se[s] + rec_dev[s,]+1*rec_dev.se[s,] )
		 logR_obs.5.minus[s,] = (ln_mn_rec[s]-1*ln_mn_rec.se[s] + rec_dev[s,]-1*rec_dev.se[s,] )
		} 
		return(list(ln_mn_rec=ln_mn_rec,ln_mn_rec.se=ln_mn_rec.se,rec_dev=rec_dev,rec_dev.se=rec_dev.se,logR_obs=logR_obs,
			logR_obs.plus=logR_obs.plus,logR_obs.minus=logR_obs.minus,
			logR_obs.5.plus=logR_obs.5.plus,logR_obs.5.minus=logR_obs.5.minus))
	}
	##_____________________________________________________

	col1<-colorRampPalette(colors()[c(71,73)])
	load(file.path(root,"CEATTLE_outputs/CEATTLE_newest/ceattle_2/results/CEATTLE_results.Rdata"))
	hind_dat<-tmp
	nspp<-as.numeric(hind_dat$nspp)
	nyrs<-as.numeric(hind_dat$nyrs)
	styr<-as.numeric(hind_dat$styr)

	sub.path<-(file.path(root,"CEATTLE_outputs/CEATTLE_newest/ceattle_0/projections/MCMC_ceattle_0_5_2/newest/results"))
	dir<-dir(sub.path)
	recfiles<-dir[grep("ceattle_fut_recruitment",dir)]
	R2files<-dir[grep("ceattle_R2",dir)]
	Fut_rep_files<-dir[grep("Future_report",dir)]

	tt<-unlist(strsplit(recfiles,split="i"))
	nn<-length(tt)/3; tt<-tt[seq(1,length(tt),3)+1]
	simulations<-(as.numeric(unlist(strsplit(tt,split="ceattle_fut_recru"))))
	s<-1
	setwd(sub.path)
	for(i in simulations){
		rec.tmp<-read.csv(recfiles[i],sep="")
		scen<-unique(rec.tmp$future_itr)
		sub<-rec.tmp[rec.tmp$spp==s,]
		SSB<-eval(parse(text=paste("hind_dat$BiomassSSB_",s,sep="")))[1,]
		hind<-data.frame(spp=s,future_itr=0,year=(hind_dat$styr+1:hind_dat$nyrs)-1,TempC=hind_dat$TempC[1,],Rec=exp(hind_dat$logR_obs[s,]),SSB=SSB, M2of1YrOlds=0)
		tmpdat<-rbind(hind,sub)
		scen<-unique(tmpdat$future_itr)
		nscen<-length(scen)
		par(mfrow=c(2,1))
		for(r in scen){
			if(r==0) {plot(ts(tmpdat[,c("SSB")],start=1979,end=2039,frequency=1))}else{
				rr<-c(which(tmpdat$future_itr==0),which(tmpdat$future_itr==r))
				lines(tmpdat$year[rr],tmpdat$SSB[rr],col=col1(nscen)[r])
			}
		}
		for(r in scen){
			if(r==0) {plot(ts(tmpdat[,c("Rec")],start=1979,end=2039,frequency=1))}else{
				rr<-c(which(tmpdat$future_itr==0),which(tmpdat$future_itr==r))
				lines(tmpdat$year[rr],tmpdat$Rec[rr],col=col1(nscen)[r])
			}
		}

	}
	root.root<-paste(strsplit(root,split="/")[[1]][1:2],collapse="_",sep="")
	graphics.off()
	setwd(path)
	read.n.plot<-function(fl="ceattle_0/projections/MCMC_ceattle_0_5_2",nsim=1,coluse=col1(2),ltyy1=1,cr=c(1)){
		# root<-paste(strsplit(fl,split="_")[[1]][1:2],collapse="_",sep="")
		# flname1<-paste("~/Dropbox/msm-master/",root,"/projections/",fl,"/results",sep="")
		tmppath<-file.path(root,"CEATTLE_outputs/CEATTLE_newest/",fl)
		dir<-dir(file.path(tmppath,"newest/results"))
		recfiles<-dir[grep("ceattle_fut_recruitment",dir)]
		R2files<-dir[grep("ceattle_R2",dir)]
		Fut_rep_files<-dir[grep("Future_report",dir)]

		snames<-c("Pollock","P. cod","arrowtooth")
		# tmp<-data.frame(read.csv(file.path(flname1,"Future_report.rep"),sep="",header=TRUE))
		tmp<-read.csv(file.path(tmppath,"newest/results",Fut_rep_files[nsim]),sep="")
		scenarios<-unique(tmp$fut_simulation)
		ltyyuse<-rep(ltyy1,length(scenarios))
		dat<-tmp
		itr<-11
		meta1<-which(names(dat)=="F_rate")
		contrule<-unique(dat$control_rule)

		Fc<-(which(names(dat)=="F_rate")+1):(which(names(dat)=="objective_fun")-1)
		Bc<-(which(names(dat)=="SSB.SSB0")+1):(grep("catch.biomass",names(dat))-1)
		Rc<-(grep("recruits.numbers",names(dat))+1):dim(dat)[2]
		Cc<-(grep("catch.biomass",names(dat))+1):(grep("recruits.numbers",names(dat))-1)
		lab<-dat[,which(names(dat)=="SSB.SSB0")]
		fr<-which(lab=="SSB")
		fr0<-which(lab=="SSB0")
		dat0<-dat[fr0,]
		dat<-dat[fr,]
		meta.0<-dat0[,1:meta1]
		meta<-dat[,1:meta1]
		rec<-dat[,Rc]
		Frate<-dat[,Fc]
		SSB<-dat[,Bc]
		catch<-dat[,Cc]
		ltyy<-ltyyuse
		itr<-dim(dat0[dat0$fut_simulation==scenarios[1]&dat0$species==1,Rc])[1]

		s<-1;i<-1;n<-2
		quartz(width=7,height=6)
		par(mfrow=c(4,3))
		par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
		par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
		par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)

		for (s in 1:3){
			ylimm<-1.1*max(na.omit(dat[dat$species==s,Rc]))
			for(i in 1:itr){
				for(n in scenarios){
					for(cc in cr){
						sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s&dat$control_rule==contrule[cc],Rc][i,]
						sub.yr<-as.numeric(substr(names(sub.sub),2,5))
						if(cc==cr[1]&n==scenarios[1]&i==1){#
							plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),col=coluse[1])
							mtext(snames[s],side=3,line=.5)
							if(s==1)
								mtext("Recruits",side=2, outer=FALSE,line=2)
							}else{
							lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[1],lty=ltyy[n])
						}
					}
				}
			}
			for(n in scenarios){
				for(cc in cr){
					sub.sub<-dat[dat$fut_simulation==n&dat$species==s&dat$control_rule==contrule[cc], Rc][i,]
					#sub.sub<-dat[dat$fut_simulation==n&dat$species==s,Rc]
					lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[2],lty=ltyy[n])
				}
			}	

		}
		for (s in 1:3){
			ylimm<-1.1*max(na.omit(dat0[dat0$species==s,Bc]))
			for(i in 1:itr){
				for(n in scenarios){
					for(cc in cr){
						sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s&dat$control_rule==contrule[cc],Bc][i,]
						#sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s,Bc][i,]
						sub.yr<-as.numeric(substr(names(sub.sub),2,5))
							if(cc==cr[1]&n==scenarios[1]&i==1){#if(n==scenarios[1]&i==1){
							plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),col=coluse[1],lty=ltyy[n])
							if(s==1)
							mtext("SSB",side=2, outer=FALSE,line=2)
							}else{
							lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[1],lty=ltyy[n])
						}
					}
				}
			}
			for(n in scenarios){
				for(cc in cr){
					sub.sub<-dat[dat$fut_simulation==n&dat$species==s&dat$control_rule==contrule[cc],Bc][i,]
					lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[2],lty=ltyy[n])
				}
					
					
			}
			
				
		}
		for (s in 1:3){
			ylimm<-1.1*max(na.omit(dat[dat$species==s,Cc]))
			for(i in 1:itr){
				for(n in scenarios){
					for(cc in cr){
						sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s&dat$control_rule==contrule[cc],Cc][i,]
						#sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s,Cc][i,]
						sub.yr<-as.numeric(substr(names(sub.sub),2,5))
						if(cc==cr[1]&n==scenarios[1]&i==1){#if(n==scenarios[1]&i==1){
							plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),col=coluse[1],lty=ltyy[n])
							if(s==1)
							mtext("Catch",side=2, outer=FALSE,line=2)
						}else{
							lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[1],lty=ltyy[n])
						}
					}
				}
			
				for(n in scenarios){
					for(cc in cr){
						sub.sub<-dat[dat$fut_simulation==n&dat$species==s&dat$control_rule==contrule[cc],Cc][i,]
						#sub.sub<-dat[dat$fut_simulation==n&dat$species==s,Cc]
						lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[2],lty=ltyy[n])
					}
				}
			}
				
		}
		for (s in 1:3){
			ylimm<-1.1*min(1,max(na.omit(dat[dat$species==s,Fc])))
			for(i in 1:itr){
				for(n in scenarios){
					for(cc in cr){
						sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s&dat$control_rule==contrule[cc],Fc][i,]
						#sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s,Fc][i,]
						sub.yr<-as.numeric(substr(names(sub.sub),2,5))
						if(cc==cr[1]&n==scenarios[1]&i==1){#if(n==scenarios[1]&i==1){
							plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),col=coluse[1],lty=ltyy[n])
							if(s==1)
							mtext("Frate",side=2, outer=FALSE,line=2)
							}else{
							lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[1],lty=ltyy[n])
						}
					}	
				}
			}
			for(n in scenarios){
				for(cc in cr){
					sub.sub<-dat[dat$fut_simulation==n&dat$species==s&dat$control_rule==contrule[cc],Fc]
					#sub.sub<-dat[dat$fut_simulation==n&dat$species==s,Fc]
					lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[2],lty=ltyy[n])
				}
			}
			
				
		}
		mtext(fl,outer=TRUE,side=1,line=2)
	}
	read.n.plot(nsim=1)


	read.n.plot2<-function(fl="ceattle_0_1_3",SaveIt=FALSE,alpha1=50,coluse=c("orange",col1(6)),ltyy1=1,plot.path="/Users/kkari/Dropbox/AFSC_models/CEATTLE/CEATTLE_outputs/CEATTLE_newest"){
		# root<-paste(strsplit(fl,split="_")[[1]][1:2],collapse="_",sep="")
		# flname1<-paste("~/Dropbox/msm-master/",root,"/projections/",fl,"/results",sep="")
		mode<-substr(fl,9,9)
		fl1<-paste("ceattle_",mode,"/projections/",sep="")
		tmppath<-(file.path(plot.path,fl1,fl,"results/Future_report.rep"))
		
		snames<-c("Pollock","P. cod","arrowtooth")
		# tmp<-data.frame(read.csv(file.path(flname1,"Future_report.rep"),sep="",header=TRUE))
		tmp<-read.csv(tmppath,sep="")
		scenarios<-unique(tmp$fut_simulation)
		print(scenarios)
		ltyyuse<-rep(ltyy1,max(scenarios))
		dat<-tmp

		Fc<-(which(names(dat)=="F_rate")+1):(which(names(dat)=="objective_fun")-1)
		Bc<-(which(names(dat)=="SSB.SSB0")+1):(grep("catch.biomass",names(dat))-1)
		Rc<-(grep("recruits.numbers",names(dat))+1):dim(dat)[2]
		Cc<-(grep("catch.biomass",names(dat))+1):(grep("recruits.numbers",names(dat))-1)
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

		s<-1;i<-1;n<-1
		quartz(width=7,height=6)
		par(mfrow=c(4,3))
		par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
		par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
		par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)

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
						lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[n],lty=ltyy[n])
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
					sub.yr<-as.numeric(substr(names(sub.sub),2,5))
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
					sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s,Cc][i,]
					sub.yr<-as.numeric(substr(names(sub.sub),2,5))
					if(n==scenarios[1]&i==1){
						plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),lty=ltyy[n],col=makeTransparent(coluse[n],alpha=alpha1))
						if(s==1)
						mtext("Catch",side=2, outer=FALSE,line=2)
						}else{
						lines(sub.yr,as.numeric(sub.sub),type="l",lty=ltyy[n],col=makeTransparent(coluse[n],alpha=alpha1))
					}
				}
				for(n in scenarios){
					sub.sub<-dat[dat$fut_simulation==n&dat$species==s,Cc][i,]
					lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[n],lty=ltyy[n])
					
				}
			}
			
			
				
		}
		for (s in 1:3){
			ylimm<-1.1*max.na(as.numeric(apply(dat[dat$species==s,Fc],2,max.na)))
			for(i in 1:itr){
				for(n in scenarios){
					sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s,Fc][i,]
					sub.yr<-as.numeric(substr(names(sub.sub),2,5))
					if(n==scenarios[1]&i==1){
						plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),lty=ltyy[n],col=makeTransparent(coluse[n],alpha=alpha1))
						if(s==1)
						mtext("Frate",side=2, outer=FALSE,line=2)
						}else{
						lines(sub.yr,as.numeric(sub.sub),type="l",lty=ltyy[n],col=makeTransparent(coluse[n],alpha=alpha1))
					}
				}
				for(n in scenarios){
					sub.sub<-dat[dat$fut_simulation==n&dat$species==s,Fc][i,]
					lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[n],lty=ltyy[n])
					
				}
			}	
		}
		mtext(fl,outer=TRUE,side=3,line=2)
		if(SaveIt==TRUE)
			quartz.save(,type="pdf",file=file.path(plot.path,paste("Allplot_",fl,".pdf",sep="")))
	
	}
	col<-colorRampPalette(colors()[c(73,71)])
	plot_rec_smry<-function(smry=smry_RS_2,sp=2,ylimm=c(12,15),ylabb="log(Rec)",mainn="P. cod, multi-species",col1=colorRampPalette(colors()[c(73,71)])){
		quartz(width=7,height=6)
		par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
		par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
		par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)

		test<-lm(log(smry[[sp]]$R_obs)~log(smry[[sp]]$TopR2_Rec))
		testtop<-lm(log(smry[[sp]]$R_obs)~log(smry[[sp]]$TopAIC_Rec))
		plot(smry[[sp]]$Rec_years,log(smry[[sp]]$R_obs),type="b",col=colors()[310],ylim=ylimm)
		lines(smry[[sp]]$Rec_years,log(smry[[sp]]$TopAIC_Rec),col=col1(3)[3],lwd=2)
		lines(smry[[sp]]$Rec_years,log(smry[[sp]]$TopR2_Rec),col=col1(3)[3],lwd=2)
		lines(smry[[sp]]$Rec_years,fitted(test),col=col1(3)[1],lwd=2)
		R2txt<-paste("Top R2, R2=", round(summary(test)$adj.r.squared,3),"; Top AIC, R2=", round(summary(testtop)$adj.r.squared,3))
		mtext(ylabb,side=2,line=1,font=2,outer=T)
		mtext(mainn,side=3,line=1,font=2,outer=T)
		legend("topleft",c("Obs","Top AIC","Top R2"),col=c(colors()[310],col1(3)[c(3,1)]),lwd=c(1,2,2),box.lty=0)
		mtext(side=3,R2txt,font=2,line=0,outer=T)
	}
	plot_rec_smry(smry=smry_RS_0,sp=2,mainn="P. cod, single-species",ylimm=c(12,15))
	plot_rec_smry(smry=smry_RS_2,sp=2,mainn="P. cod, multi-species",ylimm=c(12,15))

	plot_rec_smry(smry=smry_RS_0,sp=1,mainn="pollock, single-species",ylimm=c(14,20))
	plot_rec_smry(smry=smry_RS_2,sp=1,mainn="pollock, multi-species",ylimm=c(16,20))

	plot_rec_smry(smry=smry_RS_0,sp=3,mainn="atf, single-species",ylimm=c(10,13.5))
	plot_rec_smry(smry=smry_RS_2,sp=3,mainn="atf, multi-species",ylimm=c(10,13.5))

	plot_rec<-function(smry=smry_RS_2,sp=2,ylimm=c(12,15),ylabb="log(Rec)",mainn="P. cod, multi-species",col1=colorRampPalette(colors()[c(73,71)])){
		quartz(width=7,height=6)
		par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
		par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
		par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)

		test<-lm(log(smry[[sp]]$R_obs)~log(smry[[sp]]$TopR2_Rec))
		testtop<-lm(log(smry[[sp]]$R_obs)~log(smry[[sp]]$TopAIC_Rec))
		plot(smry[[sp]]$Rec_years,log(smry[[sp]]$R_obs),type="b",col=colors()[310],ylim=ylimm)
		lines(smry[[sp]]$Rec_years,log(smry[[sp]]$TopAIC_Rec),col=col1(3)[3],lwd=2)
		lines(smry[[sp]]$Rec_years,log(smry[[sp]]$TopR2_Rec),col=col1(3)[3],lwd=2)
		lines(smry[[sp]]$Rec_years,fitted(test),col=col1(3)[1],lwd=2)
		R2txt<-paste("Top R2, R2=", round(summary(test)$adj.r.squared,3),"; Top AIC, R2=", round(summary(testtop)$adj.r.squared,3))
		mtext(ylabb,side=2,line=1,font=2,outer=T)
		mtext(mainn,side=3,line=1,font=2,outer=T)
		legend("topleft",c("Obs","Top AIC","Top R2"),col=c(colors()[310],col1(3)[c(3,1)]),lwd=c(1,2,2),box.lty=0)
		mtext(side=3,R2txt,font=2,line=0,outer=T)
	}



	type<-c(0,2)
	rec<-0:8
	h<-2
	itr<-10


	for (t in type){
		for (r in rec){
			txt<-paste("ceattle_",t,"_",r,"_",h,sep="")
			read.n.plot2(txt)
		}
	}
	  #   // # 0 = project under mean  rec   (no RS)
  #   // # 1 = mean RS function (mean RS no covariates, ricker; rs_data4CEATTLE_0_0_0)
  #   // # 2 = RS function plus SST (rs_data4CEATTLE_SST
  #   // # 3 = RS function based on top AIC selected environm. parms for each spp (rs_data4CEATTLE_TOP)
  #   // # 4 = RS function based on model with top R2 value (rs_data4CEATTLE_TopR2)
  #   // # 5 = RS function based on Recfile_name (above)   
  #   // # 6 = RS function plus bottom temp 
  #   // # 7 = RS function plus Cold Pool
  #   // # 8 = RS function plus full model plus EAT (tot)
	read.n.plot2(fl="ceattle_0_7_2",SaveIt=FALSE)
	# read.n.plot2(fl="ceattle_0_2_2")#;quartz.save(,type="pdf",file=file.path(outfile,"Allplot_0_2_2.pdf"))
	# read.n.plot2(fl="ceattle_0_4_2")#;quartz.save(,type="pdf",file=file.path(outfile,"Allplot_0_5_2.pdf"))
	# read.n.plot2(fl="ceattle_0_3_2")#;quartz.save(,type="pdf",file=file.path(outfile,"Allplot_0_3_2.pdf"))
	# read.n.plot2(fl="ceattle_0_6_2")#;quartz.save(,type="pdf",file=file.path(outfile,"Allplot_0_3_2.pdf"))

	# read.n.plot2(fl="ceattle_2_2_2")#;quartz.save(,type="pdf",file=file.path(outfile,"Allplot_2_2_2.pdf"))
	# read.n.plot2(fl="ceattle_2_3_2")
	# read.n.plot2(fl="ceattle_2_5_2")#;quartz.save(,type="pdf",file=file.path(outfile,"Allplot_2_5_2.pdf"))
	# read.n.plot2(fl="ceattle_2_7_2")#;quartz.save(,type="pdf",file=file.path(outfile,"Allplot_2_3_2.pdf"))


	read.n.plot3<-function(fl="ceattle_0/projections/MCMC_ceattle_0_3_2",type="Catch",itr=1:10,ltyy1=1,cr=c(1)){
		# root<-paste(strsplit(fl,split="_")[[1]][1:2],collapse="_",sep="")
		# flname1<-paste("~/Dropbox/msm-master/",root,"/projections/",fl,"/results",sep="")
			s<-1;i<-1;n<-2
		quartz(width=7,height=6)
		par(mfrow=c(3,1))
		par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
		par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
		par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)


		tmppath<-file.path("/Users/kkari/Dropbox/CEATTLE-master/ceattle-master",fl)
		dir<-dir(file.path(tmppath,"newest/results"))
		recfiles<-dir[grep("ceattle_fut_recruitment",dir)]
		R2files<-dir[grep("ceattle_R2",dir)]
		Fut_rep_files<-dir[grep("Future_report",dir)]

		snames<-c("Pollock","P. cod","arrowtooth")

		for (s in 1:3){
			for(nsim in itr){
				# tmp<-data.frame(read.csv(file.path(flname1,"Future_report.rep"),sep="",header=TRUE))
				tmp<-read.csv(file.path(tmppath,"newest/results",Fut_rep_files[nsim]),sep="")
				#print(nsim)
				#scenarios<-unique(tmp$fut_simulation)
				ltyyuse<-rep(ltyy1,length(scenarios))
				dat<-tmp
				rm(tmp)
				#itr<-11
				meta1<-which(names(dat)=="F_rate")
				contrule<-unique(dat$control_rule)

				Fc<-(which(names(dat)=="F_rate")+1):(which(names(dat)=="objective_fun")-1)
				Bc<-(which(names(dat)=="SSB.SSB0")+1):(grep("catch.biomass",names(dat))-1)
				Rc<-(grep("recruits.numbers",names(dat))+1):dim(dat)[2]
				Cc<-(grep("catch.biomass",names(dat))+1):(grep("recruits.numbers",names(dat))-1)
				if(type=="Catch") cuse<-Cc
				if(type=="SSB") cuse<-Bc
				if(type=="Frate") cuse<-Fc
				if(type=="Rec") cuse<-Rc
			
				lab<-dat[,which(names(dat)=="SSB.SSB0")]
				fr<-which(lab=="SSB")
				fr0<-which(lab=="SSB0")
				dat0<-dat[fr0,]
				dat<-dat[fr,]
				meta.0<-dat0[,1:meta1]
				meta<-dat[,1:meta1]
				rec<-dat[,Rc]
				Frate<-dat[,Fc]
				SSB<-dat[,Bc]
				catch<-dat[,Cc]
				ltyy<-ltyyuse
				#itr<-dim(dat0[dat0$fut_simulation==scenarios[1]&dat0$species==1,Rc])[1]
				scenarios<-unique(dat$fut_simulation)	
					coluse<-col1(length(scenarios))	
				ylimm<-1.1*max(na.omit(dat[dat$species==s,cuse]))
				# for(i in 1:itr){
				for(n in scenarios){
					for(cc in cr){
						sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s&dat$control_rule==contrule[cc],cuse]
						sub.yr<-as.numeric(substr(names(sub.sub),2,5))
						if(cc==cr[1]&n==scenarios[1]&nsim==itr[1]){#
							plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),col=coluse[n],lty=2)
							mtext(snames[s],side=3,line=-1.5)
							mtext(type,side=2, outer=FALSE,line=2)
							}else{
							lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[n],lty=2)
						}
					}
				}
				for(n in scenarios){
					for(cc in cr){
						sub.sub<-dat[dat$fut_simulation==n&dat$species==s&dat$control_rule==contrule[cc], cuse]
						#sub.sub<-dat[dat$fut_simulation==n&dat$species==s,Rc]
						lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[n],lty=1)
					}
				}	

			}
		}
		mtext(fl,outer=TRUE,side=3,line=2)
	}
	compile.MCMC<-function(fl="ceattle_0/projections/MCMC_ceattle_0_3_2",type="Catch",itr=1:10,cr=c(1)){
		# root<-paste(strsplit(fl,split="_")[[1]][1:2],collapse="_",sep="")
		# flname1<-paste("~/Dropbox/msm-master/",root,"/projections/",fl,"/results",sep="")
			
		#tmppath<-file.path("/Users/kkari/Dropbox/CEATTLE-master/ceattle-master",fl)
		#
		tmppath<-file.path(getwd(),fl)
		dirc<-dir(file.path(tmppath,"newest/results"))
		recfiles<-dirc[grep("ceattle_fut_recruitment",dirc)]
		R2files<-dirc[grep("ceattle_R2",dirc)]
		Fut_rep_files<-dirc[grep("Future_report",dirc)]

		snames<-c("Pollock","P. cod","arrowtooth")

		for (s in 1:3){
			for(nsim in itr){
				# tmp<-data.frame(read.csv(file.path(flname1,"Future_report.rep"),sep="",header=TRUE))
				tmp<-read.csv(file.path(tmppath,"newest/results",Fut_rep_files[nsim]),sep="")
				dat<-tmp
				rm(tmp)
				#itr<-11
				meta1<-which(names(dat)=="F_rate")
				contrule<-unique(dat$control_rule)

				Fc<-(which(names(dat)=="F_rate")+1):(which(names(dat)=="objective_fun")-1)
				Bc<-(which(names(dat)=="SSB.SSB0")+1):(grep("catch.biomass",names(dat))-1)
				Rc<-(grep("recruits.numbers",names(dat))+1):dim(dat)[2]
				Cc<-(grep("catch.biomass",names(dat))+1):(grep("recruits.numbers",names(dat))-1)
				if(type=="Catch") cuse<-Cc
				if(type=="SSB") cuse<-Bc
				if(type=="Frate") cuse<-Fc
				if(type=="Rec") cuse<-Rc
			
				lab<-dat[,which(names(dat)=="SSB.SSB0")]
				fr<-which(lab=="SSB")
				fr0<-which(lab=="SSB0")
				dat0<-dat[fr0,]
				dat<-dat[fr,]
				meta.0<-dat0[,1:meta1]
				meta<-dat[,1:meta1]
				rec<-dat[,Rc]
				Frate<-dat[,Fc]
				SSB<-dat[,Bc]
				catch<-dat[,Cc]
				fut<-which(colnames(dat)=="fut_simulation")
				cuse<-c(fut,cuse)
				#itr<-dim(dat0[dat0$fut_simulation==scenarios[1]&dat0$species==1,Rc])[1]
				scenarios<-unique(dat$fut_simulation)	
				sub.sub0<-dat0[dat0$species==s&dat0$control_rule==contrule[cr],cuse]
				sub.sub<-dat[dat$species==s&dat$control_rule==contrule[cr],cuse]
				sub.yr<-as.numeric(substr(names(sub.sub[-1]),2,5))
				if(s==1&nsim==itr[1]){
					tmp.dat0<-data.frame(species=s,simulation=nsim,sub.sub0)
					tmp.dat<-data.frame(species=s,simulation=nsim,sub.sub)
					
				}else{
					tmp.dat0<-rbind(tmp.dat0,data.frame(species=s,simulation=nsim,sub.sub0))
					tmp.dat<-rbind(tmp.dat,data.frame(species=s,simulation=nsim,sub.sub))
					
				}
			}
		}
		colnames(tmp.dat)<-colnames(tmp.dat0)<-c("species","simulation","proj_model",sub.yr)
		return(list(dat0=tmp.dat0,dat=tmp.dat))
	}
	plot.quantMCMC<-function(fl="ceattle_0/projections/MCMC_ceattle_0_3_2",type="Catch",itr=1:100,cr=c(1)){}

##_____________________________________________________
## Now load MCMC runs
##_____________________________________________________
	graphics.off()
	if(1==10){
		tmpA<-rbind(data.frame(a=c(0:5),b=2),
			data.frame(a=c(0:5),b=3),
			data.frame(a=c(0:5),b=7))
		tmpA<-rbind(data.frame(a=c(0:8),b=2))

		modlist<-paste(tmpA[,1],tmpA[,2],sep="_")
		# paste(0,paste(tmpA[,1],tmpA[,2],sep="_"),sep="_"),
		# paste(2,paste(tmpA[,1],tmpA[,2],sep="_"),sep="_"))
		readlist<-""
		types<-c("Catch","Rec","SSB","Frate")

		for(m in 1:length(modlist)){
			if(tmpA[,2][m]==3){
				for(ti in 1:length(types)){

					t_tmp<-compile.MCMC(fl=paste("ceattle_0/projections/MCMC_ceattle_0_",modlist[m],sep=""),type=types[ti],cr=1)
					eval(parse(text=paste(types[ti],"0",modlist[m],"cr1<-t_tmp",sep="_")))
					t_tmp<-compile.MCMC(fl=paste("ceattle_0/projections/MCMC_ceattle_0_",modlist[m],sep=""),type=types[ti],cr=2)
					eval(parse(text=paste(types[ti],"0",modlist[m],"cr2<-t_tmp",sep="_")))

					t_tmp<-compile.MCMC(fl=paste("ceattle_2/projections/MCMC_ceattle_2_",modlist[m],sep=""),type=types[ti],cr=1)
					eval(parse(text=paste(types[ti],"2",modlist[m],"cr1<-t_tmp",sep="_")))
					t_tmp<-compile.MCMC(fl=paste("ceattle_2/projections/MCMC_ceattle_2_",modlist[m],sep=""),type=types[ti],cr=2)
					eval(parse(text=paste(types[ti],"2",modlist[m],"cr2<-t_tmp",sep="_")))

					readlist<-c(readlist,c(paste(types[ti],"0",modlist[m],"cr1",sep="_"),
					paste(types[ti],"0",modlist[m],"cr2",sep="_"),
					paste(types[ti],"2",modlist[m],"cr1",sep="_"),
					paste(types[ti],"2",modlist[m],"cr2",sep="_")))
				}

			}else{
				for(ti in 1:length(types)){

					t_tmp<-compile.MCMC(fl=paste("ceattle_0/projections/MCMC_ceattle_0_",modlist[m],sep=""),type=types[ti],cr=1)
					eval(parse(text=paste(types[ti],"_0_",modlist[m],"<-t_tmp",sep="")))
					
					t_tmp<-compile.MCMC(fl=paste("ceattle_2/projections/MCMC_ceattle_2_",modlist[m],sep=""),type=types[ti],cr=1)
					eval(parse(text=paste(types[ti],"_2_",modlist[m],"<-t_tmp",sep="")))

					readlist<-c(readlist,c(paste(types[ti],"0",modlist[m],sep="_"),
					paste(types[ti],"2",modlist[m],sep="_")))
				}
			}
		}
		# for(ii in 2:length(readlist)){
		# 	if(length(grep("cr",readlist[ii]))>0){

		# 	}else{
		# 		eval(parse(text=paste(readlist[ii],"<-",paste(readlist[ii],"_",sep=""),sep="")))
		# 	}
		# }

		save(list=readlist[-1],file=file.path(outfile,"compiledMCMC_results.Rdata"))
	}else{
		load(file.path(outfile,"compiledMCMC_results.Rdata"))
	}
		# Catch_2_4_2


##_____________________________________________________
## Plot 4 panel of projection trajectories:
##_____________________________________________________


	plot.quantMCMC<-function(test=Catch_0_3_2, Fbiomass=2,add=FALSE,projections=1:6,sp=1,ltyuse=rep(1,6),coluse=col1(6),alpha1=10,pq=c(0.05, .25, .5,.75,.95)){
		s<-1;i<-1;n<-1
		np<-length(projections)
		tt<-Fbiomass
		#for(sp in 1:3){
		ylimm<-c(0,max(rbind(apply(test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==1,],2,quantile,probs = pq)
			,apply(test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==2,],2,quantile,probs = pq)
			,apply(test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==3,],2,quantile,probs = pq)
			,apply(test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==4,],2,quantile,probs = pq))))

			#ylimm<-c(0,max(test[[tt]][test[[tt]]$species==sp,]))
			for(projm in projections){
				tmp<-apply(test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==projm,],2,quantile,probs = pq)
				if(projm==projections[1]&add==FALSE)
					plot(as.numeric(names(tmp[1,-c(1:3)])),tmp[1,-c(1:3)],col=FALSE,ylim=ylimm)
				tmpp<-c(tmp[,-c(1:3)][1,],rev(tmp[,-c(1:3)][5,]))
				polygon(as.numeric(names(tmpp)),as.numeric(tmpp),border=FALSE,col=makeTransparent(coluse[projm],alpha1),lty=ltyuse[projm])
				tmpp<-c(tmp[,-c(1:3)][2,],rev(tmp[,-c(1:3)][4,]))
				polygon(as.numeric(names(tmpp)),tmpp,border=FALSE,col=makeTransparent(coluse[projm],alpha1),lty=ltyuse[projm])
				lines(as.numeric(names(tmp[1,-c(1:3)])),tmp[,-c(1:3)][3,],type="l",col=coluse[projm],lwd=2,lty=ltyuse[projm])
			}
		#}
	}
	plot.quantMCMC(test=Catch_2_5_2)
	plot.quantMCMC(test=Catch_0_5_2)
	plot.quantMCMC_scaled2Null<-function(test=Catch_0_3_2, xlimm=c(2012,2100),prcnt=TRUE,Fbiomass=2,add=FALSE,projections=2:6,sp=1,ltyuse=rep(1,6),coluse=col1(6),alpha1=50,pq=c(0.05, .25, .5,.75,.95)){
		s<-1;i<-1;n<-1
		np<-length(projections)
		tt<-Fbiomass
		null<-test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==1,]
		#for(sp in 1:3){
			if(prcnt==TRUE){
				tmpp2<-(test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==2,][,-c(1:3)]-null[,-c(1:3)])/null[,-c(1:3)]
				tmpp3<-(test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==3,][,-c(1:3)]-null[,-c(1:3)])/null[,-c(1:3)]
				tmpp4<-(test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==4,][,-c(1:3)]-null[,-c(1:3)])/null[,-c(1:3)]
				ylimm<-100*c(min(data.frame(tmpp2,tmpp3,tmpp4)),max(data.frame(tmpp2,tmpp3,tmpp4)))
				for(projm in projections){
					tmpp1<-test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==projm,]
					tmp<-apply(100*(tmpp1-null)/null,2,quantile,probs=pq)
					if(projm==projections[1]&add==FALSE)
						plot(as.numeric(names(tmp[1,-c(1:3)])),tmp[1,-c(1:3)],col=FALSE,ylim=ylimm,xlim=xlimm)
					tmpp<-c(tmp[,-c(1:3)][1,],rev(tmp[,-c(1:3)][5,]))
					polygon(as.numeric(names(tmpp)),tmpp,border=FALSE,col=makeTransparent(coluse[projm],alpha1),lty=ltyuse[projm])
					tmpp<-c(tmp[,-c(1:3)][2,],rev(tmp[,-c(1:3)][4,]))
					polygon(as.numeric(names(tmpp)),tmpp,border=FALSE,col=makeTransparent(coluse[projm],alpha1),lty=ltyuse[projm])
					lines(as.numeric(names(tmp[1,-c(1:3)])),tmp[,-c(1:3)][3,],type="l",col=coluse[projm],lwd=2,lty=ltyuse[projm])
				}
				abline(h=0,lty=2,col=colors()[320])	
			}else{
				tmpp2<-test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==2,][,-c(1:3)]-null[,-c(1:3)]
				tmpp3<-test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==3,][,-c(1:3)]-null[,-c(1:3)]
				tmpp4<-test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==4,][,-c(1:3)]-null[,-c(1:3)]
				ylimm<-c(min(data.frame(tmpp2,tmpp3,tmpp4)),max(data.frame(tmpp2,tmpp3,tmpp4)))
				for(projm in projections){
					tmpp1<-test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==projm,]
					tmp<-apply(tmpp1-null,2,quantile)
					if(projm==projections[1]&add==FALSE)
						plot(as.numeric(names(tmp[1,-c(1:3)])),tmp[1,-c(1:3)],col=FALSE,ylim=ylimm)
					tmpp<-c(tmp[,-c(1:3)][1,],rev(tmp[,-c(1:3)][5,]))
					polygon(as.numeric(names(tmpp)),tmpp,border=FALSE,col=makeTransparent(coluse[projm],alpha1),lty=ltyuse[projm])
					tmpp<-c(tmp[,-c(1:3)][2,],rev(tmp[,-c(1:3)][4,]))
					polygon(as.numeric(names(tmpp)),tmpp,border=FALSE,col=makeTransparent(coluse[projm],alpha1),lty=ltyuse[projm])
					lines(as.numeric(names(tmp[1,-c(1:3)])),tmp[,-c(1:3)][3,],type="l",col=coluse[projm],lwd=2,lty=ltyuse[projm])
				}	
			}

	}
	plot.quantMCMC_scaled2Null(test=Catch_0_5_2)
	plot.quantMCMC_scaled2Null(test=Catch_2_5_2)

		plot.quantMCMC_scaled2Null(test=Catch_0_7_2)
	plot.quantMCMC_scaled2Null(test=Catch_2_7_2)

	col2<-colorRampPalette(colors()[c(71,73)])
	col2<-colorRampPalette(colors()[c(17,72)])
	col2<-colorRampPalette(colors()[c(496,72)])
	#  | ECHOG
	#  | CCCMA
	#  | MIROC
	plot4panel<-function(mainn="Pollock catch based on mean historical F",lgndloc="bottomleft",Fbiomass1=2,legendtext1=c("single-species","multi-species"),paneltext=c("null","ECHOG","CCCMA","MIROC"),tmpMCMC_0=Catch_0_3_2,tmpMCMC_2=Catch_2_3_2,pquse=c(.1,.1,.5,.9,.9),alpha11=20,spp=1,c1=col2(2)[1],c2=col2(2)[2]){
			quartz(width=7,height=6)
		 	par(mfrow=c(2,2))
			par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
			par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
			par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)
			coltmp1<-rep(c1,4)
			coltmp2<-rep(c2,4)

			plot.quantMCMC(test=tmpMCMC_2,Fbiomass=Fbiomass1,projections=1,ltyuse=c(2,1,1,1),coluse=coltmp2,sp=spp,alpha1=alpha11,pq=pquse)
			plot.quantMCMC(test=tmpMCMC_0,Fbiomass=Fbiomass1,add=TRUE,projections=1,ltyuse=c(2,1,1,1),coluse=coltmp1,sp=spp,alpha1=alpha11,pq=pquse)
			plot.quantMCMC(test=tmpMCMC_2,Fbiomass=Fbiomass1,projections=1,add=TRUE,ltyuse=c(2,1,1,1),coluse=coltmp2,sp=spp,alpha1=0,pq=pquse)
			
			mtext(side=3,paneltext[1],outer=FALSE,line=-1.5,font=2,adj=.01)
			legend(lgndloc,legendtext1,col=makeTransparent(c(coltmp1[1],coltmp2[1]),alpha11), lty=1,lwd=9,box.lty=0)
			legend(lgndloc,legendtext1,col=c(coltmp1[1],coltmp2[1]), lty=1,lwd=2,box.lty=0)
			for(pp in 2:4){
				plot.quantMCMC(test=tmpMCMC_2,Fbiomass=Fbiomass1,projections=c(1,pp),ltyuse=c(2,1,1,1),coluse=coltmp2,sp=spp,alpha1=alpha11,pq=pquse)
				plot.quantMCMC(test=tmpMCMC_0,Fbiomass=Fbiomass1,add=TRUE,projections=c(1,pp),ltyuse=c(2,1,1,1),coluse=coltmp1,sp=spp,alpha1=alpha11,pq=pquse)
				plot.quantMCMC(test=tmpMCMC_2,Fbiomass=Fbiomass1,add=TRUE,projections=c(1,pp),ltyuse=c(2,1,1,1),coluse=coltmp2,sp=spp,alpha1=0,pq=pquse)
				mtext(side=3,paneltext[pp],outer=FALSE,line=-1.5,font=2,adj=.01)
				
					legend(lgndloc,c("null",paneltext[pp]),col=makeTransparent(c(coltmp2[1],coltmp2[1]),alpha11), lty=1,lwd=9,box.lty=0)
					legend(lgndloc,c("null",paneltext[pp]),col=c(coltmp2[1],coltmp2[1]), lty=c(2,1),lwd=2,box.lty=0)
			
				
			}
			mtext(side=3,mainn,outer=TRUE,line=1,cex=1.5,font=2)
	}
plot6panel<-function(mainn="Pollock catch based on mean historical F",lgndloc="bottomleft",Fbiomass1=2,legendtext1=c("single-species","multi-species"),
	paneltext=c("null","ECHOG","CCCMA","MIROC","GFDL_RCP45","GFDL_RCP85"),tmpMCMC_0=Catch_0_3_2,tmpMCMC_2=Catch_2_3_2,pquse=c(.1,.1,.5,.9,.9),alpha11=20,spp=1,c1=col2(2)[1],c2=col2(2)[2]){
	quartz(width=7,height=6)
		 	par(mfrow=c(3,2))
			par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
			par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
			par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)
			coltmp1<-rep(c1,6)
			coltmp2<-rep(c2,6)
			ltyuse1<-c(2,1,1,1,1,1)

			plot.quantMCMC(test=tmpMCMC_2,Fbiomass=Fbiomass1,projections=1,ltyuse=ltyuse1,coluse=coltmp2,sp=spp,alpha1=alpha11,pq=pquse)
			plot.quantMCMC(test=tmpMCMC_0,Fbiomass=Fbiomass1,add=TRUE,projections=1,ltyuse=ltyuse1,coluse=coltmp1,sp=spp,alpha1=alpha11,pq=pquse)
			plot.quantMCMC(test=tmpMCMC_2,Fbiomass=Fbiomass1,projections=1,add=TRUE,ltyuse=ltyuse1,coluse=coltmp2,sp=spp,alpha1=0,pq=pquse)
			
			mtext(side=3,paneltext[1],outer=FALSE,line=-1.5,font=2,adj=.01)
			legend(lgndloc,legendtext1,col=makeTransparent(c(coltmp1[1],coltmp2[1]),alpha11), lty=1,lwd=9,box.lty=0)
			legend(lgndloc,legendtext1,col=c(coltmp1[1],coltmp2[1]), lty=1,lwd=2,box.lty=0)
			for(pp in 2:6){
				plot.quantMCMC(test=tmpMCMC_2,Fbiomass=Fbiomass1,projections=c(1,pp),ltyuse=ltyuse1,coluse=coltmp2,sp=spp,alpha1=alpha11,pq=pquse)
				plot.quantMCMC(test=tmpMCMC_0,Fbiomass=Fbiomass1,add=TRUE,projections=c(1,pp),ltyuse=ltyuse1,coluse=coltmp1,sp=spp,alpha1=alpha11,pq=pquse)
				plot.quantMCMC(test=tmpMCMC_2,Fbiomass=Fbiomass1,add=TRUE,projections=c(1,pp),ltyuse=ltyuse1,coluse=coltmp2,sp=spp,alpha1=0,pq=pquse)
				mtext(side=3,paneltext[pp],outer=FALSE,line=-1.5,font=2,adj=.01)
				
					legend(lgndloc,c("null",paneltext[pp]),col=makeTransparent(c(coltmp2[1],coltmp2[1]),alpha11), lty=1,lwd=9,box.lty=0)
					legend(lgndloc,c("null",paneltext[pp]),col=c(coltmp2[1],coltmp2[1]), lty=c(2,1),lwd=2,box.lty=0)
			
				
			}
			mtext(side=3,mainn,outer=TRUE,line=1,cex=1.5,font=2)
}
	graphics.off()
	col2<-colorRampPalette(colors()[c(496,72)])
	plot6panel(mainn="PLK catch, mean F, R/S+(SST)",tmpMCMC_0=Catch_0_2_2,tmpMCMC_2=Catch_2_2_2,spp=1,pquse=c(0.1,.1,.5,.9,.9))
	
	plot6panel(mainn="PLK catch, mean F, R/S+(Zoop-ration,BT,CP)",tmpMCMC_0=Catch_0_5_2,tmpMCMC_2=Catch_2_5_2,spp=1,pquse=c(0.1,.1,.5,.9,.9))
	plot6panel(mainn="PLK catch, mean F, R/S+BT",tmpMCMC_0=Catch_0_3_2,tmpMCMC_2=Catch_2_3_2,spp=1,pquse=c(0.1,.1,.5,.9,.9))
					
		quartz.save(,type="pdf",file=file.path(outfile,"plot4panel_0_5_2.pdf"))
	plot6panel(mainn="PLK catch, mean F, meanR",tmpMCMC_0=Catch_0_1_2,tmpMCMC_2=Catch_2_1_2,spp=1,pquse=c(0.1,.1,.5,.9,.9))
		quartz.save(,type="pdf",file=file.path(outfile,"plot4panel_0_2_2.pdf"))

	plot4panel(mainn="PLK catch, mean F, R/S+(SST)",tmpMCMC_0=Catch_0_2_3,tmpMCMC_2=Catch_2_2_3,spp=1,pquse=c(0.1,.1,.5,.9,.9))
		quartz.save(,type="pdf",file=file.path(outfile,"plot4panel_0_2_3.pdf"))

	plot4panel(mainn="PLK SSB, mean F, R/S+(Zoop,BT,CP)",tmpMCMC_0=SSB_0_5_2,tmpMCMC_2=SSB_2_5_2,spp=1,pquse=c(0.1,.1,.5,.9,.9))
	plot4panel(mainn="PLK SSB, mean F, R/S+(SST)",tmpMCMC_0=SSB_0_2_2,tmpMCMC_2=SSB_2_2_2,spp=1,pquse=c(0.1,.1,.5,.9,.9))


	plot4panel(mainn="PLK Rec, mean F, R/S+(Zoop,BT,CP)",lgndloc="topright",tmpMCMC_0=Rec_0_5_2,tmpMCMC_2=Rec_2_5_2,spp=1,pquse=c(0.1,.1,.5,.9,.9))
	plot4panel(mainn="PLK Rec, mean F, R/S+(SST)",lgndloc="topright",tmpMCMC_0=Rec_0_2_2,tmpMCMC_2=Rec_2_2_2,spp=1,pquse=c(0.1,.1,.5,.9,.9))

	plot4panel(mainn="PCOD catch, mean F, R/S+(Zoop,BT,CP)",tmpMCMC_0=Catch_0_5_2,tmpMCMC_2=Catch_2_5_2,spp=2,pquse=c(0.1,.1,.5,.9,.9))
	plot4panel(mainn="PCOD catch, mean F, R/S+(SST)",tmpMCMC_0=Catch_0_2_2,tmpMCMC_2=Catch_2_2_2,spp=2,pquse=c(0.1,.1,.5,.9,.9))

	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_0_2,tmpMCMC_2=Catch_2_0_2,spp=1)
	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_1_2,tmpMCMC_2=Catch_2_1_2,spp=1)
	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_2_2,tmpMCMC_2=Catch_2_2_2,spp=1)

	col2<-colorRampPalette(colors()[c(72,92)])

	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_2_2_2,tmpMCMC_2=Catch_2_5_2,spp=1,legendtext1=c("R/S+SST","R/S+(Zoop,BT,CP)"))
	plot4panel(mainn="Pollock catch, F40",tmpMCMC_0=Catch_2_2_3_cr2,tmpMCMC_2=Catch_2_5_3_cr2,spp=1,legendtext1=c("R/S+SST","R/S+(Zoop,BT,CP)"))

	plot4panel(mainn="Pollock SSB, mean F",tmpMCMC_0=SSB_2_2_2,tmpMCMC_2=SSB_2_5_2,spp=1,legendtext1=c("R/S+SST","R/S+(Zoop,BT,CP)"))
	plot4panel(mainn="Pollock SSB, F40",tmpMCMC_0=SSB_2_2_3_cr2,tmpMCMC_2=SSB_2_5_3_cr2,spp=1,legendtext1=c("R/S+SST","R/S+(Zoop,BT,CP)"))
	plot4panel(mainn="Pollock Rec, F40",tmpMCMC_0=Rec_2_2_3_cr2,tmpMCMC_2=Rec_2_5_3_cr2,spp=1,Fbiomass1=2,legendtext1=c("R/S+SST","R/S+(Zoop,BT,CP)"))
	plot4panel(mainn="Pollock Rec, mean F",tmpMCMC_0=Rec_2_2_2,tmpMCMC_2=Rec_2_5_2,spp=1,Fbiomass1=2,legendtext1=c("R/S+SST","R/S+(Zoop,BT,CP)"))


	plot4panel(mainn="P. cod catch, mean F",tmpMCMC_0=Catch_0_3_2,tmpMCMC_2=Catch_2_3_2,spp=2)
	plot4panel(mainn="Arrowtooth catch, mean F",tmpMCMC_0=Catch_0_3_2,tmpMCMC_2=Catch_2_3_2,spp=3)


	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_2_2,tmpMCMC_2=Catch_2_2_2,spp=1,alpha11=0)
	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_5_2,tmpMCMC_2=Catch_2_5_2,spp=1,alpha11=0)

	plot4panel(mainn="Pollock catch, 40%B0",tmpMCMC_0=Catch_0_3_3,tmpMCMC_2=Catch_2_3_3,spp=1)
	plot4panel(mainn="P. cod catch, 40%B0",tmpMCMC_0=Catch_0_3_3,tmpMCMC_2=Catch_2_3_3,spp=2)
	plot4panel(mainn="Arrowtooth catch, 40%B0",tmpMCMC_0=Catch_0_3_3,tmpMCMC_2=Catch_2_3_3,spp=3)

	plot4panel(mainn="Pollock catch, 40%B0",tmpMCMC_0=Catch_0_3_3cr2,tmpMCMC_2=Catch_2_3_3cr2,spp=1)
	plot4panel(mainn="P. cod catch, 40%B0",tmpMCMC_0=Catch_0_3_3cr2,tmpMCMC_2=Catch_2_3_3cr2,spp=2)
	plot4panel(mainn="Arrowtooth catch, 40%B0",tmpMCMC_0=Catch_0_3_3cr2,tmpMCMC_2=Catch_2_3_3cr2,spp=3)


	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_4_2,tmpMCMC_2=Catch_2_4_2,spp=1,pquse=c(.25,.25,.5,.75,.75))
	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_4_2,tmpMCMC_2=Catch_2_4_2,spp=2,pquse=c(.25,.25,.5,.75,.75))

	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_4_3,tmpMCMC_2=Catch_2_4_3,spp=1,pquse=c(.25,.25,.5,.75,.75))


## Plot 4 panel of projection trajectories, scaled to null:
	plot4panel_scaled2Null<-function(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_3_2,tmpMCMC_2=Catch_2_3_2,alpha11=50,spp=1,c1=col2(2)[1],c2=col2(2)[2],pqq=c(0.05, .05, .5,.95,.95)){
			quartz(width=7,height=6)
		 	par(mfrow=c(2,2))
			par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
			par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
			par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)
			coltmp<-c(c1,rep(c2,3))

			plot.quantMCMC_scaled2Null(test=tmpMCMC_0,projections=1,ltyuse=c(2,2,2,2),coluse=coltmp,sp=spp,alpha1=alpha11,pq=pqq)
			plot.quantMCMC_scaled2Null(test=tmpMCMC_2,add=TRUE,projections=1,ltyuse=c(1,1,1,1),coluse=coltmp,sp=spp,alpha1=alpha11,pq=pqq)
			for(pp in 2:4){
				plot.quantMCMC_scaled2Null(test=tmpMCMC_0,projections=pp,ltyuse=c(2,2,2,2),coluse=coltmp,sp=spp,alpha1=alpha11,pq=pqq)
				plot.quantMCMC_scaled2Null(test=tmpMCMC_2,add=TRUE,projections=c(1,pp),ltyuse=c(1,1,1,1),coluse=coltmp,sp=spp,alpha1=alpha11,pq=pqq)
			}
			mtext(side=3,mainn,outer=TRUE,line=1,cex=2,font=2)
	}
	plot4panel_scaled2Null(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_2_2,tmpMCMC_2=Catch_2_2_2,spp=1,alpha11=20)
	plot4panel_scaled2Null(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_5_2,tmpMCMC_2=Catch_2_5_2,spp=1,alpha11=20)

	plot4panel_scaled2Null(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_3_3cr2,tmpMCMC_2=Catch_2_3_3cr2,spp=1,alpha11=20)
	plot4panel_scaled2Null(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_5_3cr2,tmpMCMC_2=Catch_2_5_3cr2,spp=1,alpha11=20)


	plot4panel_scaled2Null(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_4_2,tmpMCMC_2=Catch_2_4_2,spp=1,alpha11=20)

	plot4panel_scaled2Null(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_2_7,tmpMCMC_2=Catch_2_2_7,spp=1,alpha11=20)
	
## Plot PDF of change from null:
	plotPDF_scaled2Null<-function(test=Catch_0_3_2,ylimuse=c(0,1),projm=1,probvals=c(0,-10,-50,-100),xlimuse=c(-50,50),plotyr=2020,prcnt=TRUE,Fbiomass=2,add=FALSE,projections=2:4,sp=1,ltyuse=rep(1,4),coluse=col1(4),alpha1=50,pq=c(0.05, .25, .5,.75,.95)){
		s<-1;i<-1;n<-1
		np<-length(projections)
		tt<-Fbiomass
		null<-test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==1,]
		#for(sp in 1:3){
			if(prcnt==TRUE){
				tmpp2<-(test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==2,][,-c(1:3)]-null[,-c(1:3)])/null[,-c(1:3)]
				tmpp3<-(test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==3,][,-c(1:3)]-null[,-c(1:3)])/null[,-c(1:3)]
				tmpp4<-(test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==4,][,-c(1:3)]-null[,-c(1:3)])/null[,-c(1:3)]
				tmpAll<-rbind(tmpp2,tmpp3,tmpp4)
				ylimm<-100*c(min(data.frame(tmpp2,tmpp3,tmpp4)),max(data.frame(tmpp2,tmpp3,tmpp4)))
				#for(projm in projections){
					#tmpp1<-test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==projm,]
					tmpp1<-tmpAll
					tmpp1<-100*tmpAll
					nc<-which((names(tmpp1))==as.character(plotyr))
					tt_dat<-(density(tmpp1[,nc]))
					probs<-data.frame(probvals,prob=0)
					for(iii in 1:length(probvals)){
						probs$prob[iii]<-sum(tt_dat$y[which(tt_dat$x<=probvals[iii])])/sum(tt_dat$y)
					}
					
					tmp<-apply(tmpp1,2,quantile,probs=pq)[,nc]
					sub1<-which(tt_dat$x>=tmp[1]&tt_dat$x<=tmp[5])
					sub2<-which(tt_dat$x>=tmp[2]&tt_dat$x<=tmp[4])
					tmpp_1<-c(tt_dat$y[sub1],rep(0,length(sub1)))
					tmpp_2<-c(tt_dat$y[sub2],rep(0,length(sub2)))
					#if(projm==projections[1]&add==FALSE)
					if(add==FALSE)
						plot(tt_dat$x,tt_dat$y,col=FALSE,ylab="",ylim=ylimuse,xlim=xlimuse)
					polygon(c(tt_dat$x[sub1],rev(tt_dat$x[sub1])),tmpp_1,border=FALSE,col=makeTransparent(coluse[projm],alpha1),lty=ltyuse[projm])
					polygon(c(tt_dat$x[sub2],rev(tt_dat$x[sub2])),tmpp_2,border=FALSE,col=makeTransparent(coluse[projm],alpha1),lty=ltyuse[projm])
					polygon(c(tt_dat$x,rev(tt_dat$x)),(c(tt_dat$y,rep(0,length(tt_dat$y)))),border=FALSE,col=makeTransparent(coluse[projm],alpha1),lty=ltyuse[projm])
					# lines(rep((tt_dat$x[rev(sub1)])[1],2),c(0,tt_dat$y[rev(sub1)[1]]),type="l",col=coluse[projm],lwd=2,lty=2)
					# lines(rep(tt_dat$x[sub1][1],2),c(0,tt_dat$y[sub1[1]]),type="l",col=coluse[projm],lwd=2,lty=2)
					
					# lines(rep((tt_dat$x[rev(sub2)])[1],2),c(0,tt_dat$y[rev(sub2)[1]]),type="l",col=coluse[projm],lwd=2,lty=3)
					# lines(rep(tt_dat$x[sub2][1],2),c(0,tt_dat$y[sub2[1]]),type="l",col=coluse[projm],lwd=2,lty=3)
					
					lines(rep(tt_dat$x[which(tt_dat$x>=tmp[3])[1]],2),c(0,tt_dat$y[which(tt_dat$x>=tmp[3])[1]]),type="l",col=coluse[projm],lwd=2,lty=ltyuse[projm])
				#}
			}else{
				tmpp2<-test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==2,][,-c(1:3)]-null[,-c(1:3)]
				tmpp3<-test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==3,][,-c(1:3)]-null[,-c(1:3)]
				tmpp4<-test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==4,][,-c(1:3)]-null[,-c(1:3)]
				tmpAll<-rbind(tmpp2,tmpp3,tmpp4)
				ylimm<-c(min(data.frame(tmpp2,tmpp3,tmpp4)),max(data.frame(tmpp2,tmpp3,tmpp4)))
				#for(projm in projections){
				#	tmpp1<-test[[tt]][test[[tt]]$species==sp&test[[tt]]$proj_model==projm,]
					tmpp1<-tmpAll
					nc<-which((names(tmpp1))==as.character(plotyr))
					tt_dat<-(density(tmpp1[,nc]))
					probs<-data.frame(probvals,prob=0)
					for(iii in 1:length(probvals)){
						probs$prob[iii]<-sum(tt_dat$y[which(tt_dat$x<=probvals[iii])])/sum(tt_dat$y)
					}
					tmp<-apply(tmpp1,2,quantile,probs=pq)[,nc]
					sub1<-which(tt_dat$x>=tmp[1]&tt_dat$x<=tmp[5])
					sub2<-which(tt_dat$x>=tmp[2]&tt_dat$x<=tmp[4])
					tmpp_1<-c(tt_dat$y[sub1],rep(0,length(sub1)))
					tmpp_2<-c(tt_dat$y[sub2],rep(0,length(sub2)))
				#	if(projm==projections[1]&add==FALSE)
					if(add==FALSE)	
						plot(tt_dat$x,tt_dat$y,col=FALSE,ylab="",ylim=ylimuse,xlim=xlimuse)
					polygon(c(tt_dat$x[sub1],rev(tt_dat$x[sub1])),tmpp_1,border=FALSE,col=makeTransparent(coluse[projm],alpha1),lty=ltyuse[projm])
					polygon(c(tt_dat$x[sub2],rev(tt_dat$x[sub2])),tmpp_2,border=FALSE,col=makeTransparent(coluse[projm],alpha1),lty=ltyuse[projm])
					polygon(c(tt_dat$x,rev(tt_dat$x)),(c(tt_dat$y,rep(0,length(tt_dat$y)))),border=FALSE,col=makeTransparent(coluse[projm],alpha1),lty=ltyuse[projm])
					# lines(rep((tt_dat$x[rev(sub1)])[1],2),c(0,tt_dat$y[rev(sub1)[1]]),type="l",col=coluse[projm],lwd=2,lty=2)
					# lines(rep(tt_dat$x[sub1][1],2),c(0,tt_dat$y[sub1[1]]),type="l",col=coluse[projm],lwd=2,lty=2)
					
					# lines(rep((tt_dat$x[rev(sub2)])[1],2),c(0,tt_dat$y[rev(sub2)[1]]),type="l",col=coluse[projm],lwd=2,lty=3)
					# lines(rep(tt_dat$x[sub2][1],2),c(0,tt_dat$y[sub2[1]]),type="l",col=coluse[projm],lwd=2,lty=3)
					
					lines(rep(tt_dat$x[which(tt_dat$x>=tmp[3])[1]],2),c(0,tt_dat$y[which(tt_dat$x>=tmp[3])[1]]),type="l",col=coluse[projm],lwd=2,lty=ltyuse[projm])
				#}	
			}
			return(probs=probs)

	}
	plot4panel_PDF_scaled2Null<-function(mainn="Pollock catch, mean F",projm=1,prcnt1=TRUE,lgndloc="bottomleft",Fbiomass1=2,legendtext1=c("single-species","multi-species"),paneltext=c("null","ECHOG","CCCMA","MIROC"),probvalsuse=c(0,-10,-50,-100),plotyrs=c(2015,2020,2030,2039),ylimuse1=c(.06,.03,.03,.03),xlimuse1=c(-75,75),tmpMCMC_0=Catch_0_3_2,tmpMCMC_2=Catch_2_3_2,alpha11=50,spp=1,c1=col2(2)[1],c2=col2(2)[2],pqq=c(0,0.1, .5,.9,1)){
			quartz(width=7,height=6)
		 	par(mfrow=c(4,1))
			par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
			par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
			par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)
			coltmp<-c(c1,rep(c2,3))
			xi<-0
			mod0<-mod2<-list()
			for(pp in plotyrs){
				xi<-xi+1
				mod0[[xi]]<-plotPDF_scaled2Null(test=tmpMCMC_0,prcnt=prcnt1,plotyr=pp,probvals=probvalsuse,ylimuse=c(0,ylimuse1[xi]),xlimuse=xlimuse1,ltyuse=c(1,1,1,1),coluse=rep(c1,4),sp=spp,alpha1=alpha11,pq=pqq,Fbiomass=Fbiomass1)
				abline(v=0,lty=1)
				mod2[[xi]]<-plotPDF_scaled2Null(test=tmpMCMC_2,prcnt=prcnt1,add=TRUE,probvals=probvalsuse,ylimuse=ylimuse1,xlimuse=xlimuse1,plotyr=pp,ltyuse=c(1,1,1,1),coluse=rep(c2,4),sp=spp,alpha1=alpha11,pq=pqq,Fbiomass=Fbiomass1)
				mtext(paste(letters[xi],")",pp),side=3,adj=.01,outer=FALSE,line=-2)
				if(pp==plotyrs[1]){
					legend(lgndloc,legendtext1,col=makeTransparent(c(c1,c2),alpha11*2), lty=1,lwd=11,box.lty=0)
				}
				
				mtext("Increase: ",side=3,adj=.74,outer=FALSE,line=-2)
				mtext(paste(100*round(1-mod0[[xi]][,2][which(mod0[[xi]][,1]==0)],2),"%"),side=3,adj=.85,outer=FALSE,line=-2,col=c1)
				mtext(paste(100*round(1-mod2[[xi]][,2][which(mod2[[xi]][,1]==0)],2),"%"),side=3,adj=.93,outer=FALSE,line=-2,col=c2)
				
				mtext("10 % decline: ",side=3,adj=.74,outer=FALSE,line=-3.2)
				mtext(paste(100*round(mod0[[xi]][,2][which(mod0[[xi]][,1]==-25)],2),"%"),side=3,adj=.85,outer=FALSE,line=-3.2,col=c1)
				mtext(paste(100*round(mod2[[xi]][,2][which(mod2[[xi]][,1]==-25)],2),"%"),side=3,adj=.93,outer=FALSE,line=-3.2,col=c2)
				mtext("50 % decline: ",side=3,adj=.74,outer=FALSE,line=-4.4)
				mtext(paste(100*round(mod0[[xi]][,2][which(mod0[[xi]][,1]==-50)],2),"%"),side=3,adj=.85,outer=FALSE,line=-4.4,col=c1)
				mtext(paste(100*round(mod2[[xi]][,2][which(mod2[[xi]][,1]==-50)],2),"%"),side=3,adj=.93,outer=FALSE,line=-4.4,col=c2)
			


			}
			
		
			mtext(side=3,mainn,outer=TRUE,line=1,cex=1.5,font=2)
			mtext(side=1,"% change from null",outer=TRUE,line=0,cex=1.5,font=2)
			return(list(mod0=mod0,mod2=mod2))
	}
	graphics.off()
	plot4panel_PDF_scaled2Null(mainn="Pollock catch, F40, R/S+(Zoop,BT,CP)",prcnt1=FALSE,ylimuse=c(0.000008,0.000004,.000002,.000002),xlimuse=c(-1.5e6,.7e6),probvalsuse=c(0,-25,-50,-75,-100),tmpMCMC_0=Catch_0_5_2,tmpMCMC_2=Catch_2_5_2,spp=1,alpha11=20)


	plot4panel_PDF_scaled2Null(mainn="PLK catch, mean F, R/S+(Zoop,BT,CP)",probvalsuse=c(0,-10,-25,-50,-75,-100),tmpMCMC_0=Catch_0_5_2,tmpMCMC_2=Catch_2_5_2,spp=1,alpha11=20)

	plot4panel_PDF_scaled2Null(mainn="PLK catch, mean F, R/S+SST",probvalsuse=c(0,-10,-25,-50,-75,-100),tmpMCMC_0=Catch_0_2_2,tmpMCMC_2=Catch_2_2_2,spp=1,alpha11=20)

	plot4panel_PDF_scaled2Null(mainn="Pollock catch, mean F",probvalsuse=c(0,-10,-25,-50,-75,-100),tmpMCMC_0=Catch_0_4_2,tmpMCMC_2=Catch_2_4_2,spp=1,alpha11=20)

	plot4panel_PDF_scaled2Null(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_5_2,tmpMCMC_2=Catch_2_5_2,spp=1,alpha11=20)
	plot4panel_PDF_scaled2Null(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_5_2,tmpMCMC_2=Catch_2_5_2,spp=2,alpha11=20)
	plot4panel_PDF_scaled2Null(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_5_2,tmpMCMC_2=Catch_2_5_2,spp=3,alpha11=20)


## Plot 4 panel of projection trajectories:
	
	col2<-colorRampPalette(colors()[c(71,73)])
	col2<-colorRampPalette(colors()[c(17,72)])
	col2<-colorRampPalette(colors()[c(496,72)])
	#  | ECHOG
	#  | CCCMA
	#  | MIROC
plot9panel<-function(mainn="Catch based on mean historical F",lgndloc="bottomleft",Fbiomass1=2,legendtext1=c("single-species","multi-species"),paneltext=c("null","ECHOG","CCCMA","MIROC"),tmpMCMC_0=Catch_0_3_2,tmpMCMC_2=Catch_2_3_2,pquse=c(.1,.1,.5,.9,.9),alpha11=20,c1=col2(2)[1],c2=col2(2)[2]){
			quartz(width=8,height=6.5)
		 	layout(rbind(
		 		c(1,4,7),
		 		c(2,5,8),
		 		c(3,6,9)))
			par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
			par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
			par(oma=c(2,3.5,4.5,2))# outer margins of graph: (bottom,left, top, right)
			layout.show(9)
			coltmp1<-rep(c1,4)
			coltmp2<-rep(c2,4)
			for(spp in 1:3){
				# plot.quantMCMC(test=tmpMCMC_2,Fbiomass=Fbiomass1,projections=1,ltyuse=c(2,1,1,1),coluse=coltmp2,sp=spp,alpha1=alpha11,pq=pquse)
				# plot.quantMCMC(test=tmpMCMC_0,Fbiomass=Fbiomass1,add=TRUE,projections=1,ltyuse=c(2,1,1,1),coluse=coltmp1,sp=spp,alpha1=alpha11,pq=pquse)
				# plot.quantMCMC(test=tmpMCMC_2,Fbiomass=Fbiomass1,projections=1,add=TRUE,ltyuse=c(2,1,1,1),coluse=coltmp2,sp=spp,alpha1=0,pq=pquse)
				
				# mtext(side=3,paneltext[1],outer=FALSE,line=-1.5,font=2,adj=.01)
				# legend(lgndloc,legendtext1,col=makeTransparent(c(coltmp1[1],coltmp2[1]),alpha11), lty=1,lwd=9,box.lty=0)
				# legend(lgndloc,legendtext1,col=c(coltmp1[1],coltmp2[1]), lty=1,lwd=2,box.lty=0)
				for(pp in 2:4){
					plot.quantMCMC(test=tmpMCMC_2,Fbiomass=Fbiomass1,projections=c(1,pp),ltyuse=c(2,1,1,1),coluse=coltmp2,sp=spp,alpha1=alpha11,pq=pquse)
					plot.quantMCMC(test=tmpMCMC_0,Fbiomass=Fbiomass1,add=TRUE,projections=c(1,pp),ltyuse=c(2,1,1,1),coluse=coltmp1,sp=spp,alpha1=alpha11,pq=pquse)
					plot.quantMCMC(test=tmpMCMC_2,Fbiomass=Fbiomass1,add=TRUE,projections=c(1,pp),ltyuse=c(2,1,1,1),coluse=coltmp2,sp=spp,alpha1=0,pq=pquse)
					if(pp==2) mtext(side=3,c("Pollock","P. cod","Arrowtooth")[spp],outer=FALSE,line=0,font=2)
					if(spp==1) mtext(side=2,paneltext[pp],outer=FALSE,line=2,font=2)
					if(spp==1){
						if(pp==2){
							legend("bottomright",legendtext1,col=makeTransparent(c(coltmp1[1],coltmp2[1]),alpha11), lty=1,lwd=9,box.lty=0)
							legend("bottomright",legendtext1,col=c(coltmp1[1],coltmp2[1]), lty=1,lwd=2,box.lty=0)
						}
						legend(lgndloc,c("null",paneltext[pp]),col=makeTransparent(c(coltmp2[1],coltmp2[1]),alpha11), lty=1,lwd=9,box.lty=0)
						legend(lgndloc,c("null",paneltext[pp]),col=c(coltmp2[1],coltmp2[1]), lty=c(2,1),lwd=2,box.lty=0)
					}
					
				}
			}
			mtext(side=3,mainn,outer=TRUE,line=2.5,cex=1.5,font=2)
}

	graphics.off()
	col2<-colorRampPalette(colors()[c(496,72)])
	plot9panel(mainn="Catch, mean F, R/S+(Zoop,BT,CP)",tmpMCMC_0=Catch_0_5_2,tmpMCMC_2=Catch_2_5_2,pquse=c(0.1,.1,.5,.9,.9))
	
	plot9panel(mainn="Catch, mean F, R/S+(SST)",tmpMCMC_0=Catch_0_2_2,tmpMCMC_2=Catch_2_2_2,pquse=c(0.1,.1,.5,.9,.9))

	plot9panel(mainn="Catch, mean F, R/S+bestAIC",tmpMCMC_0=Catch_0_3_2,tmpMCMC_2=Catch_2_3_2,pquse=c(0.1,.1,.5,.9,.9))

	plot9panel(mainn="Catch, mean F, R/S+topR2",tmpMCMC_0=Catch_0_4_2,tmpMCMC_2=Catch_2_4_2,pquse=c(0.1,.1,.5,.9,.9))

	plot4panel(mainn="PLK SSB, mean F, R/S+(Zoop,BT,CP)",tmpMCMC_0=SSB_0_5_2,tmpMCMC_2=SSB_2_5_2,spp=1,pquse=c(0.1,.1,.5,.9,.9))
	plot4panel(mainn="PLK SSB, mean F, R/S+(SST)",tmpMCMC_0=SSB_0_2_2,tmpMCMC_2=SSB_2_2_2,spp=1,pquse=c(0.1,.1,.5,.9,.9))


	plot4panel(mainn="PLK Rec, mean F, R/S+(Zoop,BT,CP)",lgndloc="topright",tmpMCMC_0=Rec_0_5_2,tmpMCMC_2=Rec_2_5_2,spp=1,pquse=c(0.1,.1,.5,.9,.9))
	plot4panel(mainn="PLK Rec, mean F, R/S+(SST)",lgndloc="topright",tmpMCMC_0=Rec_0_2_2,tmpMCMC_2=Rec_2_2_2,spp=1,pquse=c(0.1,.1,.5,.9,.9))

	plot4panel(mainn="PCOD catch, mean F, R/S+(Zoop,BT,CP)",tmpMCMC_0=Catch_0_5_2,tmpMCMC_2=Catch_2_5_2,spp=2,pquse=c(0.1,.1,.5,.9,.9))
	plot4panel(mainn="PCOD catch, mean F, R/S+(SST)",tmpMCMC_0=Catch_0_2_2,tmpMCMC_2=Catch_2_2_2,spp=2,pquse=c(0.1,.1,.5,.9,.9))

	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_0_2,tmpMCMC_2=Catch_2_0_2,spp=1)
	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_1_2,tmpMCMC_2=Catch_2_1_2,spp=1)
	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_2_2,tmpMCMC_2=Catch_2_2_2,spp=1)

	col2<-colorRampPalette(colors()[c(72,92)])

	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_2_2_2,tmpMCMC_2=Catch_2_5_2,spp=1,legendtext1=c("R/S+SST","R/S+(Zoop,BT,CP)"))
	plot4panel(mainn="Pollock catch, F40",tmpMCMC_0=Catch_2_2_3_cr2,tmpMCMC_2=Catch_2_5_3_cr2,spp=1,legendtext1=c("R/S+SST","R/S+(Zoop,BT,CP)"))

	plot4panel(mainn="Pollock SSB, mean F",tmpMCMC_0=SSB_2_2_2,tmpMCMC_2=SSB_2_5_2,spp=1,legendtext1=c("R/S+SST","R/S+(Zoop,BT,CP)"))
	plot4panel(mainn="Pollock SSB, F40",tmpMCMC_0=SSB_2_2_3_cr2,tmpMCMC_2=SSB_2_5_3_cr2,spp=1,legendtext1=c("R/S+SST","R/S+(Zoop,BT,CP)"))
	plot4panel(mainn="Pollock Rec, F40",tmpMCMC_0=Rec_2_2_3_cr2,tmpMCMC_2=Rec_2_5_3_cr2,spp=1,Fbiomass1=2,legendtext1=c("R/S+SST","R/S+(Zoop,BT,CP)"))
	plot4panel(mainn="Pollock Rec, mean F",tmpMCMC_0=Rec_2_2_2,tmpMCMC_2=Rec_2_5_2,spp=1,Fbiomass1=2,legendtext1=c("R/S+SST","R/S+(Zoop,BT,CP)"))


	plot4panel(mainn="P. cod catch, mean F",tmpMCMC_0=Catch_0_3_2,tmpMCMC_2=Catch_2_3_2,spp=2)
	plot4panel(mainn="Arrowtooth catch, mean F",tmpMCMC_0=Catch_0_3_2,tmpMCMC_2=Catch_2_3_2,spp=3)


	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_2_2,tmpMCMC_2=Catch_2_2_2,spp=1,alpha11=0)
	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_5_2,tmpMCMC_2=Catch_2_5_2,spp=1,alpha11=0)

	plot4panel(mainn="Pollock catch, 40%B0",tmpMCMC_0=Catch_0_3_3,tmpMCMC_2=Catch_2_3_3,spp=1)
	plot4panel(mainn="P. cod catch, 40%B0",tmpMCMC_0=Catch_0_3_3,tmpMCMC_2=Catch_2_3_3,spp=2)
	plot4panel(mainn="Arrowtooth catch, 40%B0",tmpMCMC_0=Catch_0_3_3,tmpMCMC_2=Catch_2_3_3,spp=3)

	plot4panel(mainn="Pollock catch, 40%B0",tmpMCMC_0=Catch_0_3_3cr2,tmpMCMC_2=Catch_2_3_3cr2,spp=1)
	plot4panel(mainn="P. cod catch, 40%B0",tmpMCMC_0=Catch_0_3_3cr2,tmpMCMC_2=Catch_2_3_3cr2,spp=2)
	plot4panel(mainn="Arrowtooth catch, 40%B0",tmpMCMC_0=Catch_0_3_3cr2,tmpMCMC_2=Catch_2_3_3cr2,spp=3)


	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_4_2,tmpMCMC_2=Catch_2_4_2,spp=1,pquse=c(.25,.25,.5,.75,.75))
	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_4_2,tmpMCMC_2=Catch_2_4_2,spp=2,pquse=c(.25,.25,.5,.75,.75))

	plot4panel(mainn="Pollock catch, mean F",tmpMCMC_0=Catch_0_4_3,tmpMCMC_2=Catch_2_4_3,spp=1,pquse=c(.25,.25,.5,.75,.75))




plot.quantMCMC(test=Rec_2_3_2)

read.n.plot3(fl="ceattle_0/projections/MCMC_ceattle_0_4_2",type="Rec")

read.n.plot3(fl="ceattle_0/projections/MCMC_ceattle_0_2_2")

read.n.plot3(fl="ceattle_0/projections/MCMC_ceattle_0_5_2")

read.n.plot3(fl="ceattle_0/projections/MCMC_ceattle_0_5_3",type="Rec")

read.n.plot3(fl="ceattle_2/projections/MCMC_ceattle_2_5_3",type="Rec")
read.n.plot(fl="ceattle_2/projections/MCMC_ceattle_2_5_3")
read.n.plot2(fl="ceattle_2/projections/ceattle_2_4_2")

read.n.plot3(fl="ceattle_0/projections/MCMC_ceattle_0_5_7",type="Catch")


compare.plotTrend<-function(fl.list=c("_1_2","_5_2"),navgyr=5,type="Rec",order=c(1,2,3,4),contrule=1,mods=c("ECHOG","CCCMA","MIROC"),
		nspp=3,coluse=col1(3),ltyy1=1,plot.path="/Users/kkari/Dropbox/CEATTLE-master/ceattle-master"){
	# root<-paste(strsplit(fl,split="_")[[1]][1:2],collapse="_",sep="")
	# flname1<-paste("~/Dropbox/msm-master/",root,"/projections/",fl,"/results",sep="")
	ncontrast<-length(fl.list)
	nmods<-length(mods)
	fl.use<-tmp<-list()
	i<-m<-1
	s<-1;n<-1
	snames<-c("plk","pcod","atf")
	summarized<-array(NA,c(ncontrast,2,nspp,nmods))
	dimnames(summarized)[[1]]<-fl.list
	dimnames(summarized)[[2]]<-c("single_sp","multi_sp")
	dimnames(summarized)[[3]]<-snames
	dimnames(summarized)[[4]]<-mods
	for(i in 1:ncontrast){
		fl.use[[i]]<-paste(c("ceattle_0/projections/ceattle_0","ceattle_2/projections/ceattle_2"),fl.list[i],sep="")
		tmppath<-(file.path(plot.path,fl.use[[i]],"results/Future_report.rep"))
		tmp<-list()
		
		tmp[[1]]<-read.csv(tmppath[1],sep="")
		tmp[[2]]<-read.csv(tmppath[2],sep="")
		
		
		# tmp<-data.frame(read.csv(file.path(flname1,"Future_report.rep"),sep="",header=TRUE))
		
		plot.dat.all<-list()
		for(m in 1:2){
			control_rules<-unique(tmp[[m]]$control_rule)
			n_cr<-length(control_rules)
			dat<-tmp[[m]]
			scenarios<-unique(dat$fut_simulation)
			ltyyuse<-rep(ltyy1,max(scenarios))
			
			Fc<-(which(names(dat)=="F_rate")+1):(which(names(dat)=="objective_fun")-1)
			Bc<-(which(names(dat)=="SSB.SSB0")+1):(grep("catch.biomass",names(dat))-1)
			Rc<-(grep("recruits.numbers",names(dat))+1):dim(dat)[2]
			Cc<-(grep("catch.biomass",names(dat))+1):(grep("recruits.numbers",names(dat))-1)
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
			nyrs<-dim(catch)[2]
			years<-as.numeric(substr(names(catch),2,5))
			nc<-nyrs+-(navgyr-1):0			
			cr<-1
			plot.dat.tmp<-list()
			if(type=="SSB") ccuse<-Bc
			if(type=="SSB0") ccuse<-Bc
			if(type=="Catch") ccuse<-Cc
			if(type=="Frate") ccuse<-Fc
			if(type=="Rec") ccuse<-Rc

			for (cr in 1:n_cr){
				plot.dat.se<-plot.dat<-array(0,c(3,length(scenarios),nyrs))
				for (s in 1:3){
					#ylimm<-1.1*max(na.omit(dat[dat$species==s,Rc]))
					# for(ii in 1:itr){
					for(n in scenarios){
						sub.sub<-dat[dat$fut_simulation==n&dat$species==s&dat$control_rule==control_rules[cr],ccuse]
						if(type=="SSB0") sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s&dat0$control_rule==control_rules[cr],ccuse]
						sub.yr<-as.numeric(substr(names(sub.sub),2,5))
						plot.dat[s,n,]<-as.numeric(sub.sub)
						#plot.dat.se[s,n]<-se.na(as.numeric(sub.sub[,nc])) #(sd.na(as.numeric(sub.sub[,nc]))/sqrt(dim(sub.sub)[1]*length(nc)))
					}
					# }
				}
				dimnames(plot.dat)[[1]]<-snames
				dimnames(plot.dat)[[2]]<-paste("scen",(scenarios))
				dimnames(plot.dat)[[3]]<-sub.yr
				plot.dat.tmp[[cr]]<-plot.dat
			}
			names(plot.dat.tmp)<-control_rules
			plot.dat.all[[m]]<-plot.dat.tmp
		}
		names(plot.dat.all)<-c("single_sp","multi_sp")
		# cr<-contrule;m<-1;summarized[i,m,,]<-100*(plot.dat.all[[m]][[cr]][,-1]-plot.dat.all[[m]][[cr]][,1])/plot.dat.all[[m]][[cr]][,1]
		# cr<-contrule;m<-2;summarized[i,m,,]<-100*(plot.dat.all[[m]][[cr]][,-1]-plot.dat.all[[m]][[cr]][,1])/plot.dat.all[[m]][[cr]][,1]
		# if(i==1){
		# 	tmpt<-list()
		# 	for(s in 1:3){
		# 		tmpt[[s]]<-rbind(summarized[i,1,,][s,],summarized[i,2,,][s,])
		# 		rownames(tmpt[[s]])<-paste(snames[s],c("_0","_2"),fl.list[i],sep="")
		# 	}
		# }else{
		# 	for(s in 1:3){
		# 		ttt.tmp<-rbind(summarized[i,1,,][s,],summarized[i,2,,][s,])
				
		# 		rownames(ttt.tmp)<-paste(paste(snames[s],c("_0","_2"),sep=""),fl.list[i],sep="")
		# 		tmpt[[s]]<-rbind(tmpt[[s]],ttt.tmp)
		# 	}
		# }
	}
	ylimm<-c(floor(min.na(unlist(tmpt))),ceiling(max.na(unlist(tmpt))))
	quartz(width=5,height=6)
	par(mfrow=c(3,1))
	par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
	par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
	par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)
	for(s in 1:3){
		plotdat<-rbind(summarized[,1,s,],summarized[,2,s,])
		rownames(plotdat)<-paste(c(rep(0,ncontrast),rep(2,ncontrast)),rownames(plotdat),sep="")
		barplot(t(plotdat[order,]),beside=TRUE,col=col1(3),ylim=ylimm)
		abline(h=0)
	}
	
	mtext(paste(type,control_rules[contrule]),side=3,line=1,outer=T)
}		
	# attach(list(fl.list=c("_1_2","_5_2"),navgyr=5,type="Rec",order=c(1,2,3,4),contrule=1,mods=c("ECHOG","CCCMA","MIROC"),nspp=3,coluse=col1(3),ltyy1=1,plot.path="/Users/kkari/Dropbox/CEATTLE-master/ceattle-master"))
	col1<-colorRampPalette(colors()[c(71,73)])
	col2<-colorRampPalette(colors()[c(73,73)])
barplot.se<-function(mnDat=plotdat[order,],se.dat=plotdat.se[order,],width=1,cap=.5,xstart=-.4,between=0.05,among=0,xlim_mult=1.4,ylim_mult=c(1,1.2),err_mult=1,cols=col1(3),line.cols=col2(3),border.col=col2(3),border.lty=1,border.lwd=1,line.lty=1,line.lwd=2){
		up.err<-mnDat+err_mult*se.dat
		down.err<-mnDat-err_mult*se.dat
		ylimm<-c(min.na(down.err),max.na(up.err))*ylim_mult
		ylimm[1]<-min(0,ylimm[1])
			ylimm[2]<-max(0,ylimm[2])
		nbar<-dim(plotdat)[2]
		ngroups<-dim(plotdat)[1]
		xlimm<-c(0,nbar*ngroups*xlim_mult)
		space=between*xlimm[2]
		smallspace<-among*xlimm[2]
		xseq<-seq(0,ngroups*nbar)
		xA<-xB<-xC<-xD<-rep(0,(ngroups*nbar))
		yA<-yB<-yC<-yD<-rep(0,(ngroups*nbar))
		i<-1

		xstart_use<-xstart
		for(g in 1:ngroups){
			xerr<-xA<-xB<-xC<-xD<-rep(0,(nbar))
			yerr<-yA<-yB<-yC<-yD<-rep(0,(nbar))
			xstart_use<-xstart_use+space
			xA[1]<-xB[1]<-xstart_use
			xC[1]<-xD[1]<-xB[1]+width
			yB[1]<-yC[1]<-mnDat[g,1]
			for(i in 2:(nbar)){
				xA[i]<-smallspace+xD[i-1]
				xB[i]<-smallspace+xC[i-1]
				xC[i]<-xB[i]+width
				xD[i]<-xB[i]+width
				yB[i]<-yC[i]<-mnDat[g,i]
			}
			for(i in 1:(nbar)){
				xx<-c(xA[i],xB[i],xC[i],xD[i],xA[i])
				yy<-c(yA[i],yB[i],yC[i],yD[i],yA[i])
				if(i==1&g==1) {plot(xx,yy,pch=-4,col=F,xlim=xlimm,ylim=ylimm,axes=F,ylab="",xlab=""); abline(h=0)}
				polygon(xx,yy,col=cols[i],border=border.col[i],lty=border.lty,lwd=border.lwd)
				lines(rep(xA[i]+width/2,2),c(down.err[g,i],up.err[g,i]),col=line.cols[i],lwd=line.lwd,lty=line.lty)
				lines(c(-cap/2,cap/2)+(xA[i]+width/2),rep(up.err[g,i],2),col=line.cols[i],lwd=line.lwd,lty=line.lty)
				lines(c(-cap/2,cap/2)+(xA[i]+width/2),rep(down.err[g,i],2),col=line.cols[i],lwd=line.lwd,lty=line.lty)
			}
			xstart_use<-xD[nbar]

		}


}
compare.plot<-function(fl.list=c("_1_2","_5_2"),typeplot=1,ref=1,navgyr=5,type="Rec",order=c(1,2,3,4),contrule=1,mods=c("ECHOG","CCCMA","MIROC"),nspp=3,coluse=col1(3),ltyy1=1,plot.path="/Users/kkari/Dropbox/CEATTLE-master/ceattle-master"){
	# root<-paste(strsplit(fl,split="_")[[1]][1:2],collapse="_",sep="")
	# flname1<-paste("~/Dropbox/msm-master/",root,"/projections/",fl,"/results",sep="")
	ncontrast<-length(fl.list)
	nmods<-length(mods)
	fl.use<-tmp<-list()
	i<-m<-1
	s<-1;n<-1
	snames<-c("plk","pcod","atf")
	summarized<-array(NA,c(ncontrast,2,nspp,nmods))
	dimnames(summarized)[[1]]<-fl.list
	dimnames(summarized)[[2]]<-c("single_sp","multi_sp")
	dimnames(summarized)[[3]]<-snames
	dimnames(summarized)[[4]]<-mods
	summarized.se<-summarized
	for(i in 1:ncontrast){
		fl.use[[i]]<-paste(c("ceattle_0/projections/ceattle_0","ceattle_2/projections/ceattle_2"),fl.list[i],sep="")
		tmppath<-(file.path(plot.path,fl.use[[i]],"results/Future_report.rep"))
		tmp<-list()
		
		tmp[[1]]<-read.csv(tmppath[1],sep="")
		tmp[[2]]<-read.csv(tmppath[2],sep="")
		names(tmp)<-c("ceattle_0","ceattle_2")
		
		# tmp<-data.frame(read.csv(file.path(flname1,"Future_report.rep"),sep="",header=TRUE))
		m<-1

		plot.dat.all<-plot.dat.all.se<-list()
		for(m in 1:2){
			control_rules<-unique(tmp[[m]]$control_rule)
			n_cr<-length(control_rules)
			dat<-tmp[[m]]
			scenarios<-unique(dat$fut_simulation)
			ref_scn<-which(scenarios==ref)
			ltyyuse<-rep(ltyy1,max(scenarios))
			
			Fc<-(which(names(dat)=="F_rate")+1):(which(names(dat)=="objective_fun")-1)
			Bc<-(which(names(dat)=="SSB.SSB0")+1):(grep("catch.biomass",names(dat))-1)
			Rc<-(grep("recruits.numbers",names(dat))+1):dim(dat)[2]
			Cc<-(grep("catch.biomass",names(dat))+1):(grep("recruits.numbers",names(dat))-1)
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
			nyrs<-dim(catch)[2]
			years<-as.numeric(substr(names(catch),2,5))
			nc<-nyrs+-(navgyr-1):0			
			cr<-1
			plot.dat.tmp<-plot.dat.tmp.se<-list()
			if(type=="SSB") ccuse<-Bc
			if(type=="SSB0") ccuse<-Bc
			if(type=="Catch") ccuse<-Cc
			if(type=="Frate") ccuse<-Fc
			if(type=="Rec") ccuse<-Rc

			for (cr in 1:n_cr){
				plot.dat.se<-plot.dat<-matrix(0,3,length(scenarios))
				for (s in 1:3){
					#ylimm<-1.1*max(na.omit(dat[dat$species==s,Rc]))
					# for(ii in 1:itr){
						tmp.dat<-matrix(0,length(scenarios),length(nc))
						j<-0
					for(n in scenarios){

						j<-j+1
						sub.sub<-dat[dat$fut_simulation==n&dat$species==s&dat$control_rule==control_rules[cr],ccuse]
						if(type=="SSB0") sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s&dat0$control_rule==control_rules[cr],ccuse]
						sub.yr<-as.numeric(substr(names(sub.sub),2,5))
						tmp.dat[j,]<-as.numeric(sub.sub[,nc])
						plot.dat[s,n]<-mean.na(as.numeric(sub.sub[,nc]))
						plot.dat.se[s,n]<-se.na(as.numeric(sub.sub[,nc])) #(sd.na(as.numeric(sub.sub[,nc]))/sqrt(dim(sub.sub)[1]*length(nc)))
						#if(n==ref) plot(sub.yr,as.numeric(sub.sub),type="l")
						#lines(sub.yr,as.numeric(sub.sub),type="l")
					}
					for(n in scenarios){
						plot.dat[s,n]<-mean.na(100*(tmp.dat[n,]-tmp.dat[ref,])/tmp.dat[ref,])
						plot.dat.se[s,n]<-se.na(100*(tmp.dat[n,]-tmp.dat[ref,])/tmp.dat[ref,])
					}
					# }
				}
				rownames(plot.dat.se)<-rownames(plot.dat)<-snames
				plot.dat.tmp[[cr]]<-plot.dat
				plot.dat.tmp.se[[cr]]<-plot.dat.se
			}
			names(plot.dat.tmp.se)<-names(plot.dat.tmp)<-control_rules
			plot.dat.all[[m]]<-plot.dat.tmp
			plot.dat.all.se[[m]]<-plot.dat.tmp.se
		}

		names(plot.dat.all)<-c("single_sp","multi_sp")
		cr<-contrule;m<-1;summarized[i,m,,]<-plot.dat.all[[m]][[cr]][,-1] #100*(plot.dat.all[[m]][[cr]][,-1]-plot.dat.all[[m]][[cr]][,1])/plot.dat.all[[m]][[cr]][,1]
		cr<-contrule;m<-2;summarized[i,m,,]<-plot.dat.all[[m]][[cr]][,-1] #100*(plot.dat.all[[m]][[cr]][,-1]-plot.dat.all[[m]][[cr]][,1])/plot.dat.all[[m]][[cr]][,1]

		cr<-contrule;m<-1;summarized.se[i,m,,]<-plot.dat.all.se[[m]][[cr]][,-1] #100*(plot.dat.all[[m]][[cr]][,-1]-plot.dat.all[[m]][[cr]][,1])/plot.dat.all[[m]][[cr]][,1]
		cr<-contrule;m<-2;summarized.se[i,m,,]<-plot.dat.all.se[[m]][[cr]][,-1] #100*(plot.dat.all[[m]][[cr]][,-1]-plot.dat.all[[m]][[cr]][,1])/plot.dat.all[[m]][[cr]][,1]
		if(i==1){
			tmpt.se<-tmpt<-list()
			for(s in 1:3){
				tmpt[[s]]<-summarized[i,1:2,s,]
				tmpt.se[[s]]<-summarized[i,1:2,s,]
				rownames(tmpt.se[[s]])<-rownames(tmpt[[s]])<-paste(snames[s],c("_0","_2"),fl.list[i],sep="")
			}
		}else{
			for(s in 1:3){
				ttt.tmp<-summarized[i,1:2,s,]
				ttt.tmp.se<-summarized.se[i,1:2,s,]
				
				rownames(ttt.tmp)<-rownames(ttt.tmp.se)<-paste(paste(snames[s],c("_0","_2"),sep=""),fl.list[i],sep="")
				tmpt[[s]]<-rbind(tmpt[[s]],ttt.tmp)
				tmpt.se[[s]]<-rbind(tmpt.se[[s]],ttt.tmp.se)
			}
		}
	}
	ylimm<-c(floor(min.na(unlist(tmpt))),ceiling(max.na(unlist(tmpt))))
	quartz(width=5,height=6)
	par(mfrow=c(3,1))
	par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
	par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
	par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)
	for(s in 1:3){
		plotdat<-rbind(summarized[,1,s,],summarized[,2,s,])
		plotdat.se<-rbind(summarized.se[,1,s,],summarized.se[,2,s,])
		rownames(plotdat)<-rownames(plotdat.se)<-paste(c(rep(0,ncontrast),rep(2,ncontrast)),rownames(plotdat),sep="")
		if(typeplot==1) barplot(t(plotdat[order,]),beside=TRUE,col=col1(3),ylim=ylimm)
		if(typeplot!=1) barplot.se(mnDat=plotdat[order,],se.dat=plotdat.se[order,],line.lwd=1)
		axis(2)
		abline(h=0)
	}
	
	mtext(paste(type,control_rules[contrule]),side=3,line=1,outer=T)
		
}
compare.plot_MCMC<-function(fl.list=c("_1_2","_5_2"),sims=1:100,Fbiomass=2,typeplot=1,ref=1,navgyr=5,type="Rec",order=c(1,2,3,4),contrule=1,mods=c("ECHOG","CCCMA","MIROC"),nspp=3,coluse=col1(3),ltyy1=1,plot.path="/Users/kkari/Dropbox/CEATTLE-master/ceattle-master"){
	# root<-paste(strsplit(fl,split="_")[[1]][1:2],collapse="_",sep="")
	# flname1<-paste("~/Dropbox/msm-master/",root,"/projections/",fl,"/results",sep="")
	ncontrast<-length(fl.list)
	nmods<-length(mods)
	fl.use<-tmp<-list()
	i<-m<-1
	s<-1;n<-1
	snames<-c("plk","pcod","atf")
	summarized<-array(NA,c(ncontrast,2,nspp,nmods))
	dimnames(summarized)[[1]]<-fl.list
	dimnames(summarized)[[2]]<-c("single_sp","multi_sp")
	dimnames(summarized)[[3]]<-snames
	dimnames(summarized)[[4]]<-mods
	summarized.se<-summarized
	for(i in 1:ncontrast){
		fl.use[[i]]<-paste(c("ceattle_0/projections/ceattle_0","ceattle_2/projections/ceattle_2"),fl.list[i],sep="")
		tmppath<-(file.path(plot.path,fl.use[[i]],"results/Future_report.rep"))
		tmp<-list()
		
		# tmp[[1]]<-read.csv(tmppath[1],sep="")
		# tmp[[2]]<-read.csv(tmppath[2],sep="")
		eval(parse(text=paste(c("tmp[[1]]<-","tmp[[2]]<-"),paste(type,paste(c(0,2),fl.list[i],sep=""),sep="_"),sep="")))
		names(tmp)<-c("ceattle_0","ceattle_2")
		
		# tmp<-data.frame(read.csv(file.path(flname1,"Future_report.rep"),sep="",header=TRUE))
		m<-1

		plot.dat.all<-plot.dat.all.se<-list()
		for(m in 1:2){
			control_rules<-""
			# n_cr<-length(control_rules)
			dat<-tmp[[m]][[Fbiomass]]
			scenarios<-unique(dat$proj_model)
			#sims<-unique(dat$simulation)
			# ref_scn<-which(scenarios==ref)
			# ltyyuse<-rep(ltyy1,max(scenarios))
			
			# Fc<-(which(names(dat)=="F_rate")+1):(which(names(dat)=="objective_fun")-1)
			# Bc<-(which(names(dat)=="SSB.SSB0")+1):(grep("catch.biomass",names(dat))-1)
			# Rc<-(grep("recruits.numbers",names(dat))+1):dim(dat)[2]
			# Cc<-(grep("catch.biomass",names(dat))+1):(grep("recruits.numbers",names(dat))-1)
			# lab<-dat[,which(names(dat)=="SSB.SSB0")]
			# fr<-which(lab=="SSB")
			# fr0<-which(lab=="SSB0")
			# dat0<-dat[fr0,]
			# dat<-dat[fr,]
			# rec<-dat[,Rc]
			# Frate<-dat[,Fc]
			# SSB<-dat[,Bc]
			# catch<-dat[,Cc]
			# ltyy<-ltyyuse
			# itr<-dim(dat0[dat0$fut_simulation==scenarios[1]&dat0$species==1,Rc])[1]
			nyrs<-dim(dat[-(1:3)])[2]
			 years<-as.numeric(names(dat[-(1:3)]))
			 nc<-nyrs+-(navgyr-1):0			
			 cr<-1
			plot.dat.tmp<-plot.dat.tmp.se<-list()
			# if(type=="SSB") ccuse<-Bc
			# if(type=="SSB0") ccuse<-Bc
			# if(type=="Catch") ccuse<-Cc
			# if(type=="Frate") ccuse<-Fc
			# if(type=="Rec") ccuse<-Rc

			for (cr in 1){
				plot.dat.se<-plot.dat<-matrix(0,3,length(scenarios))
				for (s in 1:3){
					#ylimm<-1.1*max(na.omit(dat[dat$species==s,Rc]))
					# for(ii in 1:itr){
						tmp.dat<-matrix(0,length(scenarios),1)
						j<-0
					for(n in scenarios){

						j<-j+1
						sub.null<-dat[dat$proj_model==1&dat$species==s,][,-(1:3)][sims,]
						sub.sub<-dat[dat$proj_model==n&dat$species==s,][,-(1:3)][sims,]
						sub.yr<-as.numeric(names(sub.sub))
						
						plot.dat[s,n]<-mean.na(100*(sub.sub[,nc]-sub.null[,nc])/sub.null[,nc])
						plot.dat.se[s,n]<-se.na(100*(sub.sub[,nc]-sub.null[,nc])/sub.null[,nc]) #(sd.na(as.numeric(sub.sub[,nc]))/sqrt(dim(sub.sub)[1]*length(nc)))
						#if(n==ref) plot(sub.yr,as.numeric(sub.sub),type="l")
						#lines(sub.yr,as.numeric(sub.sub),type="l")
					}
					# for(n in scenarios){
					# 	plot.dat[s,n]<-mean.na(100*(tmp.dat[n,]-tmp.dat[ref,])/tmp.dat[ref,])
					# 	plot.dat.se[s,n]<-se.na(100*(tmp.dat[n,]-tmp.dat[ref,])/tmp.dat[ref,])
					# }
					# }
				}
				rownames(plot.dat.se)<-rownames(plot.dat)<-snames
				plot.dat.tmp[[cr]]<-plot.dat
				plot.dat.tmp.se[[cr]]<-plot.dat.se
			}
			names(plot.dat.tmp.se)<-names(plot.dat.tmp)<-control_rules
			plot.dat.all[[m]]<-plot.dat.tmp
			plot.dat.all.se[[m]]<-plot.dat.tmp.se
		}

		names(plot.dat.all)<-c("single_sp","multi_sp")
		cr<-contrule;m<-1;summarized[i,m,,]<-plot.dat.all[[m]][[cr]][,-1] #100*(plot.dat.all[[m]][[cr]][,-1]-plot.dat.all[[m]][[cr]][,1])/plot.dat.all[[m]][[cr]][,1]
		cr<-contrule;m<-2;summarized[i,m,,]<-plot.dat.all[[m]][[cr]][,-1] #100*(plot.dat.all[[m]][[cr]][,-1]-plot.dat.all[[m]][[cr]][,1])/plot.dat.all[[m]][[cr]][,1]

		cr<-contrule;m<-1;summarized.se[i,m,,]<-plot.dat.all.se[[m]][[cr]][,-1] #100*(plot.dat.all[[m]][[cr]][,-1]-plot.dat.all[[m]][[cr]][,1])/plot.dat.all[[m]][[cr]][,1]
		cr<-contrule;m<-2;summarized.se[i,m,,]<-plot.dat.all.se[[m]][[cr]][,-1] #100*(plot.dat.all[[m]][[cr]][,-1]-plot.dat.all[[m]][[cr]][,1])/plot.dat.all[[m]][[cr]][,1]
		if(i==1){
			tmpt.se<-tmpt<-list()
			for(s in 1:3){
				tmpt[[s]]<-summarized[i,1:2,s,]
				tmpt.se[[s]]<-summarized[i,1:2,s,]
				rownames(tmpt.se[[s]])<-rownames(tmpt[[s]])<-paste(snames[s],c("_0","_2"),fl.list[i],sep="")
			}
		}else{
			for(s in 1:3){
				ttt.tmp<-summarized[i,1:2,s,]
				ttt.tmp.se<-summarized.se[i,1:2,s,]
				
				rownames(ttt.tmp)<-rownames(ttt.tmp.se)<-paste(paste(snames[s],c("_0","_2"),sep=""),fl.list[i],sep="")
				tmpt[[s]]<-rbind(tmpt[[s]],ttt.tmp)
				tmpt.se[[s]]<-rbind(tmpt.se[[s]],ttt.tmp.se)
			}
		}
	}
	ylimm<-c(floor(min.na(unlist(tmpt))),ceiling(max.na(unlist(tmpt))))
	quartz(width=5,height=6)
	par(mfrow=c(3,1))
	par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
	par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
	par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)
	for(s in 1:3){
		plotdat<-rbind(summarized[,1,s,],summarized[,2,s,])
		plotdat.se<-rbind(summarized.se[,1,s,],summarized.se[,2,s,])
		rownames(plotdat)<-rownames(plotdat.se)<-paste(c(rep(0,ncontrast),rep(2,ncontrast)),rownames(plotdat),sep="")
		if(typeplot==1) barplot(t(plotdat[order,]),beside=TRUE,col=col1(3),ylim=ylimm)
		if(typeplot!=1) barplot.se(mnDat=plotdat[order,],se.dat=plotdat.se[order,],line.lwd=1)
		axis(2)
		abline(h=0)
	}
	
	mtext(paste(type,control_rules[contrule]),side=3,line=1,outer=T)
		
}
ny<-5
cr<-2
compare.plot(fl.list=c("_2_2","_5_2"),type="Rec",navgyr=ny)
compare.plot(fl.list=c("_3_2","_5_2"),type="Catch",navgyr=ny)
compare.plot(fl.list=c("_2_3","_5_3"),type="Catch",navgyr=ny)

compare.plot(fl.list=c("_1_2","_5_2"),type="Catch",navgyr=1)
compare.plot_MCMC(fl.list=c("_1_2","_5_2"),type="Catch",navgyr=1)


compare.plot(fl.list=c("_1_3","_4_3"),type="Rec",navgyr=ny,typeplot=2)
compare.plot(fl.list=c("_1_3","_4_3"),type="Catch",navgyr=ny)
compare.plot(fl.list=c("_1_3","_4_3"),type="SSB",navgyr=ny)


compare.plot(fl.list=c("_1_7","_5_7"),type="Rec",navgyr=ny)
compare.plot(fl.list=c("_1_7","_5_7"),type="Catch",navgyr=ny)

compare.plot(fl.list=c("_1_3","_5_3"),type="Rec",navgyr=ny,contrule=cr)
compare.plot(fl.list=c("_1_3","_5_3"),type="Catch",navgyr=ny,contrule=cr)
compare.plot(fl.list=c("_3_3","_5_3"),type="SSB",navgyr=ny,contrule=1)
compare.plot(type="SSB0",navgyr=10)
compare.plot(navgyr=1,contrule=2)
compare.plot(type="Catch",navgyr=5)
compare.plot(type="Catch",navgyr=1,contrule=2)

compare.plot(type="Catch",contrule=2)

compare.plot(fl.list=c("_1_3","_2_3","_3_3","_4_3","_5_3"),order=c(1:10))
compare.plot(fl.list=c("_1_7","_2_7","_3_7","_4_7","_5_7"),order=c(1:10),type="Catch")
compare.plot(fl.list=c("_1_2","_2_2","_3_2","_4_2","_5_2"),order=c(1:10),type="Rec")

	quartz(width=7,height=6)
	par(mfrow=c(3,1))
	par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
	par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
	par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)
	for(s in 1:3){
		barplot(t(cbind(tt1_0[,s],tt2_0[,s],tt1_2[,s],tt2_2[,s])),beside=TRUE,col=col1(4))
		abline(h=0)
	}


								if(n==scenarios[1]&i==1){


					plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),col=coluse[1])
					mtext(snames[s],side=3,line=.5)
					if(s==1)
						mtext("Recruits",side=2, outer=FALSE,line=2)
				}else{
					lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[1],lty=ltyy[n])
				}
			}
			for(n in scenarios){
				sub.sub<-dat[dat$fut_simulation==n&dat$species==s,Rc][i,]
				lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[i+1],lty=ltyy[n])
			}
		}
			
	}
	for (s in 1:3){
		ylimm<-1.1*max(na.omit(dat0[dat0$species==s,Bc]))
		for(i in 1:itr){
			for(n in scenarios){
				sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s,Bc][i,]
				sub.yr<-as.numeric(substr(names(sub.sub),2,5))
				if(n==scenarios[1]&i==1){
					plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),col=coluse[1],lty=ltyy[n])
					if(s==1)
					mtext("SSB",side=2, outer=FALSE,line=2)
					}else{
					lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[1],lty=ltyy[n])
				}
			}
			for(n in scenarios){
				sub.sub<-dat[dat$fut_simulation==n&dat$species==s,Bc][i,]
				lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[i+1],lty=ltyy[n])
				
			}
		}	
	}
	for (s in 1:3){
		ylimm<-1.1*max(na.omit(dat[dat$species==s,Cc]))
		for(i in 1:itr){
			for(n in scenarios){
				sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s,Cc][i,]
				sub.yr<-as.numeric(substr(names(sub.sub),2,5))
				if(n==scenarios[1]&i==1){
					plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),col=coluse[1],lty=ltyy[n])
					if(s==1)
					mtext("Catch",side=2, outer=FALSE,line=2)
					}else{
					lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[1],lty=ltyy[n])
				}
			}
			for(n in scenarios){
				sub.sub<-dat[dat$fut_simulation==n&dat$species==s,Cc][i,]
				lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[1+i],lty=ltyy[n])
				
			}
		}
		
		
			
	}
	for (s in 1:3){
		ylimm<-1.1*max(.2,max(na.omit(dat[dat$species==s,Fc])))
		for(i in 1:itr){
			for(n in scenarios){
				sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s,Fc][i,]
				sub.yr<-as.numeric(substr(names(sub.sub),2,5))
				if(n==scenarios[1]&i==1){
					plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),col=coluse[1],lty=ltyy[n])
					if(s==1)
					mtext("Frate",side=2, outer=FALSE,line=2)
					}else{
					lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[1],lty=ltyy[n])
				}
			}
			for(n in scenarios){
				sub.sub<-dat[dat$fut_simulation==n&dat$species==s,Fc][i,]
				lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[i+1],lty=ltyy[n])
				
			}
		}	
	}
	mtext(fl,outer=TRUE,side=3,line=2)
}
fluse<- "MCMC_ceattle_2_2_2"
fluse<- paste(unlist(strsplit(path,split="/"))[(grep("ceattle-master",unlist(strsplit(path,split="/")))+1):(grep("newest",unlist(strsplit(path,split="/")))-1)],sep="",collapse="/")
read.n.plot(3,fl=fluse)
read.n.plot(3,fl="ceattle_0/projections/MCMC_ceattle_0_0_2")
read.n.plot(3,fl="ceattle_0/projections/MCMC_ceattle_0_1_3")
read.n.plot(fl="ceattle_2/projections/MCMC_ceattle_2_0_2")

read.n.plot2(fl="ceattle_2/projections/ceattle_2_3_1")
read.n.plot2(fl="ceattle_0/projections/ceattle_0_3_1")


read.n.plot2(fl="ceattle_2/projections/ceattle_2_5_2")
read.n.plot2(fl="ceattle_0/projections/ceattle_0_0_8")
graphics.off()
read.n.plot2(fl="ceattle_2/projections/ceattle_2_1_9")
read.n.plot2(fl="ceattle_0/projections/ceattle_0_1_2")

graphics.off()
read.n.plot2(fl="ceattle_0/projections/ceattle_0_0_4")
read.n.plot2(fl="ceattle_2/projections/ceattle_2_0_2")

read.n.plot2(fl="ceattle_0/projections/ceattle_0_5_3")
read.n.plot2(fl="ceattle_2/projections/ceattle_2_5_3")
graphics.off()
read.n.plot2(fl="ceattle_0/projections/ceattle_0_3_3")
read.n.plot2(fl="ceattle_2/projections/ceattle_2_3_3")

read.n.plot2(fl="ceattle_0/projections/ceattle_0_1_2")
read.n.plot2(fl="ceattle_0/projections/ceattle_0_2_2")
sub.plot<-function(type="SSB",subc=Rc,On=1,s,ltyy=1:2){

		ylimm<-1.1*max(max(na.omit(dat[dat$species==s,subc])),max(na.omit(dat0[dat0$species==s,subc])))
		for(i in 1:itr){
			for(n in scenarios){
				sub.sub<-dat0[dat0$fut_simulation==n&dat0$species==s,subc]
				sub.yr<-as.numeric(substr(names(sub.sub),2,5))
				if(n==1&i==1){
					if(On==0) plot(sub.yr,as.numeric(sub.sub),type="l",ylim=c(0,ylimm),col=coluse[nsim],lty=ltyy[2])
					if(On==1) lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[nsim],lty=ltyy[2])
					mtext(snames[s],side=3,line=.5)
					if(s==1)
						mtext(type,side=2, outer=FALSE,line=2)
				}else{
					lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[nsim],lty=ltyy[2])
				}
			}
		}
		for(n in scenarios){
				sub.sub<-dat[dat$fut_simulation==n&dat$species==s,subc]
				lines(sub.yr,as.numeric(sub.sub),type="l",col=coluse[nsim],lty=ltyy[1])
		}	

}

graphics.off()
read.n.plotMCMC<-function(nsim=1,coluse=col1(length(simulations)),ltyy1=1){
	# root<-paste(strsplit(fl,split="_")[[1]][1:2],collapse="_",sep="")
	# flname1<-paste("~/Dropbox/msm-master/",root,"/projections/",fl,"/results",sep="")
	fl<- unlist(strsplit(path,split="/"))[grep("MCMC",unlist(strsplit(path,split="/")))]
	snames<-c("Pollock","P. cod","arrowtooth")
	# tmp<-data.frame(read.csv(file.path(flname1,"Future_report.rep"),sep="",header=TRUE))
	quartz(width=7,height=6)
	par(mfrow=c(4,3))
	par(mar=c(2,2,0,0)) # margins of graph: (bottom,left, top, right)
	par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
	par(oma=c(2,3.5,3.5,2))# outer margins of graph: (bottom,left, top, right)
	txt<-c("SSB","Rec","Catch","Frate")
	subc.c<-list(Bc,Rc,Cc,Fc)
	for(l in 1:4){
		for(s in 1:3){
			switch_on<-0
			for(nsim in simulations){

				tmp<-read.csv(Fut_rep_files[nsim],sep="")
				scenarios<-unique(tmp$fut_simulation)
				ltyyuse<-rep(ltyy1,length(scenarios))
				dat<-tmp
				itr<-11

				Fc<-(which(names(dat)=="F_rate")+1):(which(names(dat)=="objective_fun")-1)
				Bc<-(which(names(dat)=="SSB.SSB0")+1):(grep("catch.biomass",names(dat))-1)
				Rc<-(grep("recruits.numbers",names(dat))+1):dim(dat)[2]
				Cc<-(grep("catch.biomass",names(dat))+1):(grep("recruits.numbers",names(dat))-1)
				lab<-dat[,which(names(dat)=="SSB.SSB0")]
				fr<-which(lab=="SSB")
				fr0<-which(lab=="SSB0")
				dat0<-dat[fr0,]
				dat<-dat[fr,]
				rec<-dat[,Rc]
				Frate<-dat[,Fc]
				SSB<-dat[,Bc]
				catch<-dat[,Cc]
				#ltyy<-ltyyuse

				sub.plot(type=txt[l],subc=subc.c[[l]],On=switch_on,s)
				switch_on<-1

			}
		}
	}
	mtext(fl,outer=TRUE,side=1,line=2)
}
read.n.plot(1)
path
dev.new()
path_0<-"/Users/kkari/Dropbox/CEATTLE-master/ceattle-master/ceattle_0/projections"
	path_2<-"/Users/kkari/Dropbox/CEATTLE-master/ceattle-master/ceattle_2/projections"
			
plot_future_rec<-function(fl="2_5_2",yy="Rec",miny=0.1,minx=2013,logg=T,xx="year",path=path_2){
	tmppath<-file.path(path,paste("ceattle_",fl,"/results/ceattle_fut_recruitment.rep",sep=""))
	tt<-read.csv(tmppath,sep="")
	head(tt)
	nspp<-length(unique(tt$spp))
	par(mfrow=c(nspp,1))
	s<-1;i<-1
	itr<-unique(tt$future_itr)
	nitr<-length(itr)
	for(s in 1:nspp){
		eval(parse(text=paste("ylimm<-c((min.na(tt[tt$spp==s&tt,]$",yy,")),(max.na(tt[tt$spp==s&tt,]$",yy,")))",sep="")))
			eval(parse(text=paste("xlimm<-c((min.na(tt[tt$spp==s&tt,]$",xx,")),(max.na(tt[tt$spp==s&tt,]$",xx,")))",sep="")))
			
			ylimm[1]<-min(ylimm[1],miny)
			xlimm[1]<-min(xlimm[1],minx)
			if(logg==T|logg==TRUE)
					ylimm<-log(ylimm)
		for(i in 1:nitr){
			sub.tt<-tt[tt$spp==s&tt$future_itr==i,]
			if(i==1) {
				if(logg==T|logg==TRUE){
					#ylimm<-log(ylimm)
					eval(parse(text=paste("plot(log(",yy,")~",xx,",data=sub.tt,type=\"l\",axes=F,ylim=ylimm,xlim=xlimm)",sep="")))
				}else{
					eval(parse(text=paste("plot(",yy,"~",xx,",data=sub.tt,type=\"l\",axes=F,ylim=ylimm,xlim=xlimm)",sep="")))
				}
				
				axis(1);axis(2)
			}
			if(logg==T|logg==TRUE){
				eval(parse(text=paste("lines(log(",yy,")~",xx,",data=sub.tt)",sep="")))
			}else{
				eval(parse(text=paste("lines(",yy,"~",xx,",data=sub.tt)",sep="")))
			}
			
		}
		
	}
}

plot_future_rec(logg=F)
plot_future_rec(yy="Rec",logg=F,fl="0_5_2",path=path_0)

plot_future_rec(yy="M2of1YrOlds",logg=F,fl="2_5_2")
plot_future_rec(yy="TempC",logg=F)
plot_future_rec(yy="Rec",logg=F,fl="0_1_2",path=path_0)
plot_future_rec(yy="Rec",logg=F,fl="2_1_2",path=path_2)
plot_future_rec(yy="Rec",logg=F,fl="0_4_2",path=path_0)
plot_future_rec(yy="Rec",logg=F,fl="2_4_2",path=path_2)


path.tmp<-"/Users/kkari/Dropbox/CEATTLE-master/ceattle-master/ceattle_2/projections/ceattle_2_5_2/results/ceattle_R_projection.rep"
tt<-read.csv(path.tmp,sep="",header=T)
graphics.off()
scn<-2
CRS<-unique(tt$Control_rule);c<-1
s<-1
sub.tt<-tt[tt$Scenario==scn&tt$Control_rule==CRS[c]&tt$species==s,]
plot(sub.tt$future_year,sub.tt[,7],type="l")

plot(sub.tt$TempC_futUSE.itemp1.i.,sub.tt$R_fut,type="p")







