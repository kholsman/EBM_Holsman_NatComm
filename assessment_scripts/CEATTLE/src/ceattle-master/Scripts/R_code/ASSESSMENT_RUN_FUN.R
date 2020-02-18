#####################################
#ASSESSMENT_RUN_FUN
# R Functions needed to run CEATTLE for the ASSESSMENT
# Kirstin Holsman
# June 2018
#####################################

#-------------------------------------
# get key parms for reporting
#-------------------------------------
  # This function replaces data in a dat file
  getFparms<-function(scen=1,mn=main,
            flname=filenm,modelname=modelnm, 
              rec =1, hvst=3, mode=0,
                hcr=1.7,yr=1,endyr=nyrs,catch){
            
            In<-file.path(mn,paste0(modelname,"_",mode),
                paste0("projections/",flname,"_",mode,"_",rec,"_",hvst,"/results/"))
            #tt1<-read.csv(file=file.path(In,"Future_report.rep"),sep=" ")
            tt1<-read.csv(file=file.path(In,"ceattle_R_projection.rep"),sep=" ")
            N_est<-fsh_sel<-F<-M1<-M2<-N_fut<-W<-list()
            if(mode==0) mm<-ceattle_0
            if(mode==2) mm<-ceattle_2
            fsh_sel[[1]]<-    as.numeric(mm$fsh_sel_1)
            fsh_sel[[2]]<-    as.numeric(mm$fsh_sel_2)
            fsh_sel[[3]]<-  as.numeric(mm$fsh_sel_3)
            # get N estimate from the last year of the estimation period
            N_est[[1]]<-    (mm$N_1)[endyr+1,]
            N_est[[2]]<-    (mm$N_2)[endyr+1,]
            N_est[[3]]<-  (mm$N_3)[endyr+1,]
            
            # get values on the first year of the projection period
            for(s in 1:3){
              subt<-  tt1[tt1$future_year==yr&tt1$Scenario==scen&tt1$Control_rule==hcr&tt1$species==s,]
              F[[s]]<-subt$F
              M1[[s]]<-subt$M1
              M2[[s]]<-subt$M2
              N_fut[[s]]<-subt$N
              W[[s]]<-subt$wt_at_age
           }
            return(list(F=F,M1=M1,M2=M2,N_fut=N_fut,N_est=N_est,W=W,fsh_sel=fsh_sel,endyr=endyr))
          
        }

#-------------------------------------
# getC
#-------------------------------------
  # Get F rate           
getC<-function(Fhat,data){
          s<-data$spIn
          parmIn<-data$parmIn
          N<-parmIn$N_fut[[s]]
          M1<-parmIn$M1[[s]]
          M2<-parmIn$M2[[s]]
          W<-parmIn$W[[s]]
          fsh_sel<-parmIn$fsh_sel[[s]]
          F<-Fhat*fsh_sel
          Z<-M1+M2+F
          
          Chat<-(F/Z)*(1-exp(-Z))*N*W
          return(Chat)
        
 }


#-------------------------------------
# findF
#-------------------------------------
  # Get F rate   
    findF<-function(par,data){
      parmIn<-data$parmIn
      targetC<-data$targetC
      spIn<-data$spIn
      F<-exp(par[1])
      Chat<-sum(getC(F,data=list(parmIn=parmIn,spIn=spIn)))
      return((log(Chat)-log(targetC))^2)
    }

#-------------------------------------
# replace_dat
#-------------------------------------
  # This function replaces data in a dat file
    replace_dat<-function(
              mode=0,
              flin,flout,
              nm="B0_set",
              rplac=tmp0,skip=1,
              new=T){
                  # fl = new filename
                  # nm = value to be replaced
                  # rplac = list of data to overwrite (each list obj = line)
                  # nlines = number of lines
                  # new = if true create
                  
                nlines<-length(rplac)
                tmpin<-list()
                # create duplicate of multispp dat file:
                file.copy(from=flin,to=flout, overwrite=TRUE)
                
                tt<-scan(file=flin,what=character(),sep="\n")   
                nn<-(grep(nm,tt)+skip)+1:nlines
                
                cat(tt[1], file=flout,append=FALSE);cat("\n", file=flout,append=TRUE)
                j<-0
                for( i in 2:length(tt)){
                  if(i%in%nn){ 
                    j<-j+1
                    cat(rplac[[j]], file=flout,append=TRUE);cat("\n", file=flout,append=TRUE)
                  }else{ 
                    cat(tt[i], file=flout,append=TRUE);cat("\n", file=flout,append=TRUE)
                  }
                }
    }

#-------------------------------------
# getBO
#-------------------------------------
	# This function gets the B0 value from simulations
	getB0<-function(mn=main,flname=filenm,modelname=modelnm, rec =1, hvst=3,mode=0,hcr=hcrr,ref_yrs=-9:0,BplusYrs=c(2018,2019)){
        		In<-file.path(mn,paste0(modelname,"_",mode),paste0("projections/",flname,"_",mode,"_",rec,"_",hvst,"/results/"))
        		tt1<-read.csv(file=file.path(In,"Future_report.rep"),sep=" ")
          	cc<-(which(names(tt1)=="SSB.SSB0")+2):(which(names(tt1)=="catch.biomass.")-1)
          	ref_yrs<-length(tt1[1,][cc])+ref_yrs
          	tmpdir<-getwd()
          	setwd(In)
        		load("proj.Rdata") # loads "proj"
        		tmp<-proj$ts
        		meta<-tmp$meta[tmp$meta$control_rule==hcr&tmp$meta$B_target==0,]
        		B0<-tmp$SSB[tmp$meta$control_rule==hcr&tmp$meta$B_target==0,]
        		Bf<-tmp$SSB[tmp$meta$control_rule==hcr&tmp$meta$B_target!=0,]
        		F<-tmp$Frate[tmp$meta$control_rule==hcr&tmp$meta$B_target!=0,]
        		
        		tt<-apply(B0[,ref_yrs],1,mean,na.rm=T)
        		B0_out<-data.frame(
        		        sp=meta$species,Scen=meta$fut_simulation,
        		        targetSSB0=apply(B0[,ref_yrs],1,mean,na.rm=T),
          		      SSB=apply(Bf[,ref_yrs],1,mean,na.rm=T),
          		      model=mods[meta$fut_simulation],
          		      hcr=hcr)
          		for(i in 1:length(BplusYrs)){
          		    # find matching year
          		    eval(parse(text=paste0("B0_out$SSB0_pls",i,"<-B0[,as.numeric(names(B0))==BplusYrs[i]]") ))
          		    eval(parse(text=paste0("B0_out$SSB_pls",i,"<-Bf[,as.numeric(names(Bf))==BplusYrs[i]]") ))
          		    eval(parse(text=paste0("B0_out$F40_pls",i,"<-F[,as.numeric(names(F))==BplusYrs[i]]") ))
          		}
          		setwd(tmpdir)
          		return(B0_out)
    }


#-------------------------------------
# replace_ctl
#-------------------------------------
	# This function reads a control file and replaces objects within that file
	# nm must match name exactly.
    replace_ctl<-function(flin,flout,nm="B0_set",rplac=tmp0){
       
        tt<-scan(file=flin,what=character(),sep="\n")
        # create duplicate of multispp dat file:
        if(flin!=flout)
          file.copy(from=flin,to=flout, overwrite=TRUE)

        nn<-grep(nm,tt)+1
        tmp<-tt[grep(nm,tt)+1]
      	cat(tt[1], file=flout,append=FALSE)
      	cat("\n", file=flout,append=TRUE)
      	for( i in 2:length(tt)){
      	  if(i!=nn){ 
      	    	cat(tt[i], file=flout,append=TRUE)
      		cat("\n", file=flout,append=TRUE)
      	  }else{ 
      	  	cat(rplac, file=flout,append=TRUE)
      		  cat("\n", file=flout,append=TRUE)
      	  }
      	}
  	}

#-------------------------------------
# getF40
#-------------------------------------
	# This subfunction gets the F40 values from a given run
  	#  mn=main;flname=filenm;modelname=modelnm; rec =1; hvst=3; sp=1;mode=0;hcr=hcrset;ref_yrs=77:87
  	getF40<-function(mn=main,flname=filenm,modelname=modelnm, rec =1, hvst=3, sp=1,mode=0,hcr=hcrset,ref_yrs=77:87){
        	In<-file.path(mn,paste0(modelname,"_",mode),paste0("projections/",flname,"_",mode,"_",rec,"_",hvst,
        	"/results/ceattle_R_projection.rep"))
        	tmp<-read.csv(In,header=T,sep=" ")
        	scen<-unique(tmp$Scenario);nsecn<-length(scen)
        	sub<-tmp[tmp$species==sp&tmp$Control_rule==hcr,]
        	tt<-tapply(sub$F,sub$Scenario,unique)
        	F40<-data.frame(sp=sp,Scen=as.numeric(names(tt)),F40=tt,model=mods[scen],hcr=hcr)
        	nyrs_fut<-length(sort(unique(sub$future_year)))
        	# sub<-tmp[tmp$species==sp&tmp$Control_rule==hcr&tmp$future_year%in%ref_yrs,]
        	return(list(F40=F40, dat=tmp,	nyrs_fut=nyrs_fut))
  	}
  	
#-------------------------------------
# F40fun
#-------------------------------------
	# This function gets the F40 values from a given run
  	# depends on getF40()
	F40fun<-function(mn=main,flnameIN=filenm, r =1, h=3, m=0,hcrr=hcrset){
        	projRep<-F40_list<-list()
        	m_0<-getF40(mn=main,flname=flnameIN, rec =r, hvst=h, mode=m,sp=1,hcr=hcrr)
        	tmp<-m_0$F40
        	projRep[["singlespp"]]<-m_0$dat
        	for(s in 2:nspp)
        	  tmp<-rbind(tmp,getF40(mn=main,flname=flnameIN, rec =r, hvst=h, mode=m,sp=s,hcr=hcrr)$F40)
        	# nm<-paste
        	F40_list[["F40_0"]]<-tmp
        	m<-2
        	m_2<-getF40(mn=main,flname=flnameIN, rec =r, hvst=h, mode=m,sp=1,hcr=hcrr)
        	projRep[["multispp"]]<-m_2$dat
        	tmp<-m_2$F40
        	for(s in 2:nspp)
        	  tmp<-rbind(tmp,getF40(mn=main,flname=flnameIN, rec =r, hvst=h, mode=m,sp=s,hcr=hcrr)$F40)
        	F40_list[["F40_2"]]<-tmp
        	return(list(F40_list=F40_list,projRep=projRep))
   	}

#-------------------------------------
# compareF
#-------------------------------------
	# This is a function that summarizes F rates and plots them to compare F rates 
	compareF<-function(
		sptxt=c("pollock","cod","arrowtooth"),
		coll=colorRampPalette(colors()[c(72,83)] )(4),
		F40_a=F40_1_3,F40_b=F40_5_3){

	        F40_smry<-list()
	        par(mfrow=c(3,1))
	        for(s in 1:nspp){
	            tt<-t(data.frame(
	              single_mn=F40_a$F40_list[[1]]$F40[F40_a$F40_list[[1]]$sp==s],
	              single_full=F40_b$F40_list[[1]]$F40[F40_b$F40_list[[1]]$sp==s],
	              multisp_mn=F40_a$F40_list[[2]]$F40[F40_a$F40_list[[2]]$sp==s],
	              multisp_full=F40_b$F40_list[[2]]$F40[F40_b$F40_list[[2]]$sp==s]))
	            colnames(tt)<-F40_a$F40_list[[1]]$model[F40_a$F40_list[[1]]$sp==s]
	            if(s==1) barplot(tt,main=sptxt[s],beside=T,las=2,cex.names=.6,names=colnames(tt),col=coll,
	            legend=T,args.legend=list(box.lty = 0,horiz = T,x="top"))
	            if(s!=1) barplot(tt,main=sptxt[s],beside=T,las=2,cex.names=.6,names=colnames(tt),col=coll)
	            F40_smry[[s]]<-tt
	        }
    		return(F40_smry)
    }

#-------------------------------------
# make_FabcDat
#-------------------------------------
	# This creates the Fabc.dat file for CEATTLE
	make_FabcDat<-function(m=0,F40_In=F40_1_3,fn=file.path(f40fn,"F40In_0_1_3")){
   		F40_In<-F40_In$F40_list
   		if(m==0) F40_In<-F40_In[[1]]
   		if(m==2) F40_In<-F40_In[[2]]
  
  	 	if(file.exists(fn)){
  			#skip
  		}else{
  			file.create(fn)
  		}
		cat("# data file for setting F40 and F25",file=fn,append=F,sep="\n")
		cat("#Fset_yrs",file=fn,append=T,sep="\n")
		cat(nyrs_fut,file=fn,append=T,sep="\n")
		cat("# ----- F40 -------------------------------- ",file=fn,append=T,sep="\n")
		for(itemp in unique(F40_In$Scen)){
			cat(paste("#=========== scen",itemp,"==================="),file=fn,append=T,sep="\n")
			for (sp in unique(F40_In$sp)){
				cat(rep(F40_In$F40[F40_In$sp==sp&F40_In$Scen==itemp],nyrs_fut),file=fn,append=T,sep=" ")
				cat("\n",file=fn,append=T,sep="")
			}
		}
		cat("# ----- F35 -------------------------------- ",file=fn,append=T,sep="\n")
		for(itemp in unique(F40_In$Scen)){
			cat(paste("#=========== scen",itemp,"==================="),file=fn,append=T,sep="\n")
			for (sp in unique(F40_In$sp)){
	 			cat(rep(F40_In$F40[F40_In$sp==sp&F40_In$Scen==itemp],nyrs_fut),file=fn,append=T,sep=" ")
	 			cat("\n",file=fn,append=T,sep="")
			}
		}
		cat(12345,file=fn,append=T,sep="\n")
  }

#-------------------------------------
# getsub
#-------------------------------------
	# This find the subset of F40 files
    getsub<-function(tt=F40_1_3[[2]][[1]],scn=1,ftyr=nyrs_fut,cr=hcrset,sp=1){
          rr<-which(tt$species%in%sp&tt$Scenario%in%scn&tt$future_year%in%ftyr&tt$Control_rule%in%cr)
          return(tt[rr,])
    }
   

#-------------------------------------
# tier3
#-------------------------------------
	# DEFUNCT? This find calculates the tier 3 sloping HcR- is missing 20% cutoff
        tier3<-function(Fin,Bratio,alpha=0.05,cbeta=.2){
          #cbeta : lower limit for EBFM , e.g, B20% in groundfish
          maxFabc<-Bratio*0 
          if(any(Bratio>1)) maxFabc[Bratio>1]<-Fin[Bratio>1]
          if(any(alpha<Bratio&Bratio<=1)) maxFabc[alpha<Bratio&Bratio<=1]<-Fin[alpha<Bratio&Bratio<=1]*((Bratio[alpha<Bratio&Bratio<=1]-alpha)/(1-alpha))
          if(any(Bratio<alpha)) maxFabc[Bratio<alpha]<-0
          if(any(Bratio<=cbeta)) maxFabc[Bratio<=cbeta]<-0

          return(maxFabc)
        }



