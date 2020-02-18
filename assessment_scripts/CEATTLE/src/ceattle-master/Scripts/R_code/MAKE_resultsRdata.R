########################################################
# MAKE_resultsRdata.R
# Main script for summarizing CEATTLE outputs into a data file
# Kirstin Holsman
# kirstin.holsman@noaa.gov
# Last Updated: May 2016
# 
########################################################
# rm(list=ls());setwd("/Users/kholsman/GitHub/CEATTLE/runs/assmnt_2018_2")

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
tt<-getwd()
print("CURRENT DIRECTORY =")

print(tt)
tmp<-read_estrep(fn=file.path(tt,"results/ceattle_R2_est.rep"))

nspp<-as.numeric(tmp$nspp)
nyrs<-as.numeric(tmp$nyrs)
styr<-as.numeric(tmp$styr)

tmp<-assign.rec(target=tmp,fn=file.path(tt,"results/ceattle_est.std"))
tmp$Type<-as.numeric(strsplit(tt,split="ceattle_")[[1]][2])



# get mean F mort
    nyrs<-dim(tmp$F_1)[1]
    F_avg<-matrix(0,3,nyrs)

    for(s in 1:nspp)
      {
        eval(parse(text=paste("Fage<-tmp$F_",s,sep="")))
        eval(parse(text=paste("Fsel<-tmp$fsh_sel_",s,sep="")))
        nages<-dim(Fage)[2]
        for(y in 1:nyrs)
          F_avg[s,y]<-(Fage[y,]/Fsel[1,])[nages]
         rm(Fage);rm(Fsel)
  
      } 
      tmp$F_avg<-F_avg

save(tmp,file=file.path(tt,"results/CEATTLE_results.Rdata"))

q("no")



read_estrepV2<-function(fn,printt=TRUE){
	tt<-read.csv(fn,header=FALSE,colClasses="character")
	vals<-grep("#",tt[[1]])
	nval<-length(vals)
	tt[[1]][vals]

}

