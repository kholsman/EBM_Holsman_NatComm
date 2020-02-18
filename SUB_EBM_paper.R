## ------------------------------------------------
## K. Holsman 
## Feb 2020
## Kirstin.holsman@noaa.gov
##
## SUB_EBM_paper.R
## This code gathers projections from the CEATTLE multispecies assessment 
## runs and reshapes them for analysis and plotting
##
## ------------------------------------------------

  col1<-colorRampPalette(colors()[c(280,320)])
  col2<-colorRampPalette(colors()[c(70,491)])
  col2<-colorRampPalette(colors()[c(114,491)])
  col3<-colorRampPalette(c("yellow","red"))
  
  if(!require(rootSolve)){ install.packages(rootSolve)}else{library(rootSolve)}
  
  # download and unzip the latest ceattle runs:
  if(!length(dir(paste0("data/runs/",fldr,"_0")))>0){
    download.file("https://figshare.com/s/d9c35dbe0880f4169041",paste0("data/runs/",fldr,"_0.zip"))
    system (paste0("cd data/runs; unzip ",fldr,"_0.zip"))
  }
  if(!length(dir(paste0("data/runs/",fldr,"_2")))>0){
    download.file("https://figshare.com/s/d9c35dbe0880f4169041",paste0("data/runs/",fldr,"_2.zip"))
    system (paste0("cd data/runs; unzip ",fldr,"_2.zip"))
  }
    
            # actual .dat files for CEATTLE multispecies assessment:
            retroFL       <-  "data/retro_data2018_long_ext_bcs.dat"
            futFL         <-  "data/proj_data2018_long_ext_bcs.dat"

            refrun    <-  19
            scn       <-  2
            h         <-  13
            rfset     <-  c(19,59)
            rset      <-  c(1,5)[2]
            
            if(update.simlist){
              simlist0  <-  dir(paste0("data/runs/",fldr,"_0/projections/summaryFigs"))
              simlist2  <-  dir(paste0("data/runs/",fldr,"_2/projections/summaryFigs"))
              save(simlist0,file=paste0("data/runs/",fldr,"_0/projections/simlist0.Rdata"))
              save(simlist2,file=paste0("data/runs/",fldr,"_2/projections/simlist2.Rdata"))
            }else{
              load(paste0("data/runs/",fldr,"_0/projections/simlist0.Rdata"))
              load(paste0("data/runs/",fldr,"_2/projections/simlist2.Rdata"))
            }
            
            mclist0   <-  simlist0[grep("_mc",simlist0)]
            mclist2   <-  simlist2[grep("_mc",simlist2)]
            datlist0  <-  datlist2  <-  ""
            for(i in 1:length(mclist0))
                mclist0[i]   <-  getnm(mclist0[i])
            for(i in 1:length(mclist2))
                mclist2[i]   <-  getnm(mclist2[i])
            mclist0   <-   unique(mclist0)
            mclist2   <-   unique(mclist2)
            empty0    <-   data.frame(nm=simlist0,empty=FALSE)
            empty2    <-   data.frame(nm=simlist2,empty=FALSE)


            #_______________________________________
            # read in all the runs & create simlist:
            #_______________________________________

            # skip this step (not needed for paper)
           
            if(status) cat("read in main dat single spp projections\n")
            mode      <-  0

            for(i in 1:length(simlist0)){
                  tmpnm   <-  substr(simlist0[i],14,nchar(simlist0[i])-4)
                  tmpflnm <-  paste0("data/runs/",fldr,"_0/projections/",
                                  tmpnm,"/results/ceattle_R_projection.rep")
                  if(file.exists(tmpflnm)){
                        ttt <-  read.csv(tmpflnm,sep=" ")
                        eval(parse(text=paste0("dat",strsplit(tmpnm,split=fldr)[[1]][2],"  <-  ttt") ))
                        datlist0<-c(datlist0,paste0("dat",strsplit(tmpnm,split=fldr)[[1]][2]))
                        empty0$empty[i]  <-  FALSE  
                  }else{
                        empty0$empty[i] <-  TRUE  
                  }
                  if(status) progress(100*round(i/length(simlist0),2))
            }
            
            mode  <-  2
            if(status) cat("read in main dat multi spp projections\n")
            for(i in 1:length(simlist2)){
                        tmpnm   <-  substr(simlist2[i],14,nchar(simlist2[i])-4)
                        tmpflnm <-  paste0("data/runs/",fldr,"_2/projections/",
                                  tmpnm,"/results/ceattle_R_projection.rep")
                  if(file.exists(tmpflnm)){
                        ttt <-  read.csv(tmpflnm,sep=" ")
                        eval(parse(text=paste0("dat",strsplit(tmpnm,split=fldr)[[1]][2],"  <-  ttt") ))
                        datlist2<-c(datlist2,paste0("dat",strsplit(tmpnm,split=fldr)[[1]][2]))
                        empty2$empty[i]  <-  FALSE
                  }else{
                        empty2$empty[i]  <-  TRUE  
                  }
                  if(status) progress(100*round(i/length(simlist2),2))
            }

            simlist        <-  data.frame(simlist=as.character(ls()[grep("dat_",ls())]))
            simlist
            simlist$note   <-  ""
            simlist$note[grep("19",simlist[,1])]  <-  "based on B0 assuming no climate effects on persistence"
            simlist$note[grep("59",simlist[,1])]  <-  "based B0 assuming climate effects on persistence"

            #_______________________________________
            #USE climate niave reference point for BO; but climate specific projections:
            #_______________________________________

            dat_0_5_12    <-  dat_019_CENaivecf_0_5_12
            dat_2_5_12    <-  dat_219_CENaivecf_2_5_12

            dat_0_5_13    <-  dat_2MT_019_CENaivecf1_0_5_13
            dat_2_5_13    <-  dat_2MT_219_CENaivecf1_2_5_13

            #_______________________________________
            # Load ROMSNPZ data
            #_______________________________________

            dat_retro     <-  "# BT_retro"
            dat_fut       <-  "# BT_future"
            covn          <-  1
            covuse        <-  1
            c             <-  9  # future scenario

            #  "###########################################################"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
            # [1]  "#  | mn_Hind"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
            # [2]  "#  | MIROC_A1B"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
            # [3]  "#  | ECHOG_A1B"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
            # [4]  "#  | CCCMA_A1B"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
            # [5]  "#  | GFDL_rcp45"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
            # [6]  "#  | GFDL_rcp85"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
            # [7]  "#  | GFDL_rcp85_bio"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
            # [8]  "#  | MIROC_rcp45"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
            # [9]  "#  | MIROC_rcp85"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
            # [10] "#  | CESM_rcp45"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
            # [11] "#  | CESM_rcp85"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
            # [12] "#  | CESM_rcp85_bio"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
            #  "###########################################################"  


            # retro. data:
            #------------------------------------
            if(status) cat("reading retro. data")  
            tt        <-  scan(file=retroFL,what=character(),sep="#",skip=1, nlines=1)
            covars    <-  unlist(strsplit(tt,split=" ")); covars  <-  covars[covars!=""]

            if(covuse==0){
                  retro   <-  dat_retro
                  fut     <-  dat_fut
            }else{
                  retro   <-  paste0("#  ",covars[covn])
                  fut     <-  paste0("# ",covars[covn])
            }
            
            tt        <-  scan(file=retroFL,what=character(),sep="\n",skip=2); tt[grep("#",tt)]
            nn        <-  grep(retro,tt)+1
            tmp       <-  tt[grep(retro,tt)+1];    tmp  <-  as.numeric(strsplit(tmp,split=" ")[[1]])
            ny        <-  as.numeric(tt[grep("#nyrs",tt)+1])
            yrs       <-  tt[grep("#Retro_years",tt)+1];    yrs  <-  as.numeric(strsplit(yrs,split=" ")[[1]])
            tmp_hind  <-  tmp

            # future data:
            #------------------------------------
            if(status) cat("reading future data")  
            tt                 <-  scan(file=futFL,what=character(),sep="\n",skip=2)
            nn                 <-  grep(fut,tt)[1]+1
            nyrs_fut           <-  as.numeric(tt[grep("#nyrs_fut",tt)+1])
            n_fut_itr          <-  as.numeric(tt[grep("#n_fut_itr",tt)+1])
            yrs_fut            <-  tt[grep("#fut_years",tt)+1];    
            yrs_fut            <-  as.numeric(strsplit(yrs_fut,split=" ")[[1]])
            tmp                <-  tt[grep(fut,tt)[1]+1:n_fut_itr]
            tmp_fut            <-  matrix(NA,n_fut_itr,nyrs_fut)
            for(i in 1:n_fut_itr)
            tmp_fut[i,]        <-  as.numeric(strsplit(tmp[i],split=" ")[[1]])
            colnames(tmp_fut)  <-  yrs_fut

            BT_dat    <-  data.frame(t=c(yrs,yrs_fut),temp=c(tmp_hind,tmp_fut[c,]))
            BT_dat$t  <-  strptime(paste(BT_dat$t,"01-01",sep="-"),format="%Y-%m-%d")

            # make full matrix of data for bottom Temperature
            simnames  <-  tt[grep("mn_Hind",tt)+(1:n_fut_itr)-1]
            simnames  <-  unlist(strsplit(simnames,"#  | "))
            simnames  <-  simnames[-which(simnames%in%c("","|"))]
            simnames[simnames=="mn_Hind"]  <-  "persistence"

            allDat  <-  data.frame(matrix(NA,dim(BT_dat)[1],1+n_fut_itr))
            colnames(allDat)  <-  c("t",simnames)
            i  <-  1
            allDat[,1:2]  <-  cbind(c(yrs,yrs_fut),c(tmp_hind,tmp_fut[i,]))
            for(i in 2:n_fut_itr)
            allDat[,i+1]  <-  c(tmp_hind,tmp_fut[i,])

            # Set up plotting stuff:
            #------------------------------------     
            Years         <-  c(yrs,yrs_fut)
            nYrsTot       <-  length(Years)
            A1B_n         <-  grep("A1B",colnames(allDat))
            bio_n         <-  grep("bio",colnames(allDat))
            rcp45_n       <-  grep("rcp45",colnames(allDat))
            rcp85_n       <-  grep("rcp85",colnames(allDat))
            rcp85NoBio_n  <-  setdiff(rcp85_n,bio_n)
            plotList      <-  c(2,rcp45_n,rcp85NoBio_n)
            esnm          <-  list(rcp45_n,rcp85NoBio_n)
            esnm          <-  list(c(rcp45_n,rcp85NoBio_n))
            probbs        <-  c(.1,.25,.5,.75,.9)
            alphaAll      <-  ceiling(rep(100/(length(probbs)/2),length(probbs)/2))

            c1            <-  col2(5)
            c2            <-  col2(6)[seq(2,6,2)]
            c3            <-  col3(6)[seq(2,6,2)]
            collIn        <-  rep(NA,13)
            collIn[1:2]   <-  col2(2)[2]
            collIn[c(A1B_n,bio_n)]  <-  c1
            collIn[rcp45_n]         <-  c2
            collIn[rcp85NoBio_n]    <-  c3
            ltyall                  <-  rep(1,13)  
            ltyall[1:2]             <-  1
            ltyall[c(A1B_n,bio_n)]  <-  1
            ltyall[rcp45_n]         <-  c(1,1,2)
            ltyall[rcp85NoBio_n]    <-  c(1,1,2)
            lwdall                  <-  rep(1,13)  
            lwdall[1:2]             <-  2
            lwdall[c(A1B_n,bio_n)]  <-  1
            lwdall[rcp45_n]         <-  c(1,1,1)
            lwdall[rcp85NoBio_n]    <-  c(1,1,1)

            # Read in projection files
            #------------------------------------ 
            # first get the Fished Biomass values
            if(status) print("read in projection files")
            mclist  <-  c(mclist0,mclist2)

            for(i in 1:length(mclist)){
            ix  <-  mclist[i]
            ix  <-  strsplit(ix,split="dat_")[[1]][2]
              if(i==1){ 
                nitr<-length(grep(ix,simlist[,1]))
              }else{ 
                nitr<-c(nitr,length(grep(ix,simlist[,1])))
              }
            }

            for(i in 1:length(mclist)){
                  ix  <-  mclist[i]
                  ix  <-  strsplit(ix,split="dat_")[[1]][2]
                  if(any(rbind(empty0,empty2)$empty[grep(ix,rbind(empty0,empty2)[,1])])){
                    print(paste("empty folder for ",ix))
                  }else{
                       eval(parse(text=paste0("dat_",ix,"  <-  getMCMC(datin=mclist[i],nitr=nitr[i],agein=1)") ))
                       eval(parse(text=paste0("C_",ix,"    <-  getMCMC(datin=mclist[i],nitr=nitr[i],valin='Catch_total_biom',agein=1)") ))
                       eval(parse(text=paste0("abc_",ix,"  <-  getMCMC(datin=mclist[i],nitr=nitr[i],valin='ABC_total_biom',agein=1)") )) 
                       eval(parse(text=paste0("B_",ix,"    <-  getMCMC(datin=mclist[i],nitr=nitr[i],valin='SSB_total_biom',agein=1)") )) 
                       eval(parse(text=paste0("B0_",ix,"   <-  getMCMC(datin=mclist[i],nitr=nitr[i],valin='SSB0_total_biom',agein=1)") )) 
                       
                       eval(parse(text=paste0("TempC_",ix,"<-  getMCMC(datin=mclist[i],nitr=nitr[i],valin='bottomT_C',agein=1)") )) 
                  }

                  if(status) progress(100*(round(1/length(mclist),2)))
            }

            for(i in 1:length(mclist)){
                  ix  <-  mclist[i]
                  ix  <-  strsplit(ix,split="dat_")[[1]][2]
                  if(any(rbind(empty0,empty2)$empty[grep(ix,rbind(empty0,empty2)[,1])])){
                    print(paste("empty folder for ",ix))
                  }else{
                        
                        eval(parse(text=paste0("W4_",ix,"         <-  getMCMC(datin=mclist[i],nitr=nitr[i],valin='wt_at_age',agein=4)" ))) 
                        eval(parse(text=paste0("F_",ix,"         <-  getMCMC(datin=mclist[i],nitr=nitr[i],valin='F',agein=1)") )) 
                        eval(parse(text=paste0("ABC_",ix,"         <-  getMCMC(datin=mclist[i],nitr=nitr[i],valin='ABC_total_biom',agein=1)") )) 
                        eval(parse(text=paste0("Fabc_",ix,"     <-  getMCMC(datin=mclist[i],nitr=nitr[i],valin='Fabc',agein=1)") )) 
                        eval(parse(text=paste0("F40_",ix,"     <-  getMCMC(datin=mclist[i],nitr=nitr[i],valin='F40',agein=1)") )) 
                        eval(parse(text=paste0("M2_",ix,"         <-  getMCMC(datin=mclist[i],nitr=nitr[i],valin='M2',agein=1)") )) 
                        eval(parse(text=paste0("ration_",ix,"     <-  getMCMC(datin=mclist[i],nitr=nitr[i],valin='Ration_g_fish',agein=4)") )) 
                  }

                  if(status) progress(100*(round(1/length(mclist),2)))
            } 

# get Btarget for each mclist
      for(i in 1:length(mclist)){
                  ix  <-  mclist[i]
                  ix  <-  strsplit(ix,split="dat_")[[1]][2]
                  if(any(rbind(empty0,empty2)$empty[grep(ix,rbind(empty0,empty2)[,1])])){
                    print(paste("empty folder for ",ix))
                  }else{
                        paste("tmpdir<-",fldr,ix)
                  }

                  if(status) progress(100*(round(1/length(mclist),2)))
            } 

            mclist_nomc<-c(
            "dat_0_5_3","dat_0_5_12","dat_0_5_13",
            "dat_2_5_3","dat_2_5_12","dat_2_5_13")

            for(i in 1:length(mclist_nomc)){
                  B_0_5_3   <-  grabDat(datIn=dat_0_5_3,valIn="SSB_total_biom")
                  ix  <-  mclist_nomc[i]
                  ix  <-  strsplit(ix,split="dat_")[[1]][2]
                  eval(parse(text=paste0("B_",ix,"    <-  grabDat(datIn=",mclist_nomc[i],",valIn='SSB_total_biom')") )) 
                  eval(parse(text=paste0("B0_",ix,"    <-  grabDat(datIn=",mclist_nomc[i],",valIn='SSB0_total_biom')") )) 
                  eval(parse(text=paste0("C_",ix,"    <-  grabDat(datIn=",mclist_nomc[i],",valIn='Catch_total_biom')") )) 
                  eval(parse(text=paste0("abc_",ix,"    <-  grabDat(datIn=",mclist_nomc[i],",valIn='ABC_total_biom')") ))
                  eval(parse(text=paste0("F_",ix,"    <-  grabDat(datIn=",mclist_nomc[i],",valIn='F')") ))
            }

            lim  <-  c(-10,-50,-90)
            lim  <-  c(-10)
            sp   <-  1
            esmlist  <-  list(rcp45_n,rcp85NoBio_n)
            nlist    <-  length(esmlist)
            Yrbin    <-  c(2017,2025,2050,2075,2100)
            nbin     <-  length(Yrbin)
            risk     <-  data.frame(matrix(NA,nlist,nbin-1))
            colnames(risk)  <-  paste(paste0("(",Yrbin[-nbin]),paste0(Yrbin[-1],"]"),sep=",")
            rownames(risk)  <-  1:nlist
            rownames(risk)  <-  c("rcp45","rcp85")

            l      <-  1
            ceattle_219_2_5_13  <-  read_INdat(paste0("data/runs/",fldr,"_2/projections/",fldr,"_2MT_219_CENaivecf1_2_5_13")) # KEY RUN
            ceattle_219_2_5_12  <-  read_INdat(paste0("data/runs/",fldr,"_2/projections/",fldr,"_219_CENaivecf_2_5_12"))      # KEY RUN
           
            ceattle_2_1_3  <-  read_INdat(paste0("data/runs/",fldr,"_2/projections/",fldr,"_2_1_3"))
            ceattle_2_5_3  <-  read_INdat(paste0("data/runs/",fldr,"_2/projections/",fldr,"_setB0_2_5_3"))
            ceattle_2_5_3  <-  read_INdat(paste0("data/runs/",fldr,"_2/projections/",fldr,"_setB0_2_5_3"))
           
            limlist<-c(-10,-50,-80)
            modeLIST=c("SSM","MSM")
            for(mmm in modeLIST){
                  for(sp in 1:3){
                        for (l in 1:length(limlist)){
                          
                        if(mmm=="SSM") {
                              datuse<-dat_019_CENaivecf_0_5_12
                              datuseMCMC_B<-B_019_CENaivecf_0_5_12_mc[sp,,,]
                              datuseMCMC_C<-C_019_CENaivecf_0_5_12_mc[sp,,,]
                              print(mmm)
                        }
                        if(mmm=="MSM"){ 
                              datuse<-dat_219_CENaivecf_2_5_12
                              datuseMCMC_B<-B_219_CENaivecf_2_5_12_mc[sp,,,]
                              datuseMCMC_C<-C_219_CENaivecf_2_5_12_mc[sp,,,]
                               print(mmm)
                        }

                           tmp     <- calcRisk(delta=grabDat(datIn=datuse,valIn="Catch_total_biom")[[sp]],limm=limlist[l])
                           tmpB     <- calcRisk(delta=grabDat(datIn=datuse,valIn="SSB_total_biom")[[sp]],limm=limlist[l])
                           tmpC_mc  <- calcRisk_MCMC(delta=datuseMCMC_C,limm=limlist[l])
                           tmpB_mc  <- calcRisk_MCMC(delta=datuseMCMC_B,limm=limlist[l])
                          
                                if(mmm==modeLIST[1]&sp==1&l==1){ 
                                  risk12 <- data.frame(timeframe=rep(names(tmp), each = 2),rcp=c("rcp45","rcp85"),sp=sp,mode=mmm,
                                    riskC=unlist(tmp),riskB=unlist(tmpB), 
                                     riskCmn=as.numeric(tmpC_mc$mnRisk),riskBmn=as.numeric(tmpB_mc$mnRisk), 
                                      riskCsd=as.numeric(tmpC_mc$sdRisk),riskBsd=as.numeric(tmpB_mc$sdRisk), 
                                      riskCcv=as.numeric(tmpC_mc$sdRisk/tmpC_mc$mnRisk),riskBcv=as.numeric(tmpB_mc$sdRisk/tmpB_mc$mnRisk), 
                                    type=paste0(abs(limlist[l]),"% decline"))
                                 

                                }else{
                                  risk12 <- rbind(risk12,
                                    data.frame(timeframe=rep(names(tmp), each = 2),rcp=c("rcp45","rcp85"),sp=sp,mode=mmm,
                                    riskC=unlist(tmp),riskB=unlist(tmpB), 
                                     riskCmn=as.numeric(tmpC_mc$mnRisk),riskBmn=as.numeric(tmpB_mc$mnRisk), 
                                      riskCsd=as.numeric(tmpC_mc$sdRisk),riskBsd=as.numeric(tmpB_mc$sdRisk), 
                                      riskCcv=as.numeric(tmpC_mc$sdRisk/tmpC_mc$mnRisk),riskBcv=as.numeric(tmpB_mc$sdRisk/tmpB_mc$mnRisk), 
                                    type=paste0(abs(limlist[l]),"% decline")))
                                }

                        }
                  }
            }
           
            for(mmm in c("SSM","MSM")){
                  for(sp in 1:3){
                        for (l in 1:length(limlist)){
                                
                              if(mmm=="SSM") {
                                    datuse<-dat_2MT_019_CENaivecf1_0_5_13
                                    datuseMCMC_B<-B_2MT_019_CENaivecf1_0_5_13_mc[sp,,,]
                                    datuseMCMC_C<-C_2MT_019_CENaivecf1_0_5_13_mc[sp,,,]
                              }
                              if(mmm=="MSM"){ 
                                    datuse<-dat_2MT_219_CENaivecf1_2_5_13
                                    datuseMCMC_B<-B_2MT_219_CENaivecf1_2_5_13_mc[sp,,,]
                                    datuseMCMC_C<-C_2MT_219_CENaivecf1_2_5_13_mc[sp,,,]
                              }

                                 tmp     <- calcRisk(delta=grabDat(datIn=datuse,valIn="Catch_total_biom")[[sp]],limm=limlist[l])
                                 tmpB     <- calcRisk(delta=grabDat(datIn=datuse,valIn="SSB_total_biom")[[sp]],limm=limlist[l])
                                 tmpC_mc  <- calcRisk_MCMC(delta=datuseMCMC_C,limm=limlist[l])
                                 tmpB_mc  <- calcRisk_MCMC(delta=datuseMCMC_B,limm=limlist[l])
                                      
                                if(mmm==modeLIST[1]&sp==1&l==1){ 
                                  risk13 <- data.frame(timeframe=rep(names(tmp), each = 2),rcp=c("rcp45","rcp85"),sp=sp,mode=mmm,
                                    riskC=unlist(tmp),riskB=unlist(tmpB), 
                                     riskCmn=as.numeric(tmpC_mc$mnRisk),riskBmn=as.numeric(tmpB_mc$mnRisk), 
                                      riskCsd=as.numeric(tmpC_mc$sdRisk),riskBsd=as.numeric(tmpB_mc$sdRisk), 
                                      riskCcv=as.numeric(tmpC_mc$sdRisk/tmpC_mc$mnRisk),riskBcv=as.numeric(tmpB_mc$sdRisk/tmpB_mc$mnRisk), 
                                    type=paste0(abs(limlist[l]),"% decline"))
                                 

                                }else{
                                  risk13 <- rbind(risk13,
                                    data.frame(timeframe=rep(names(tmp), each = 2),rcp=c("rcp45","rcp85"),sp=sp,mode=mmm,
                                    riskC=unlist(tmp),riskB=unlist(tmpB), 
                                     riskCmn=as.numeric(tmpC_mc$mnRisk),riskBmn=as.numeric(tmpB_mc$mnRisk), 
                                      riskCsd=as.numeric(tmpC_mc$sdRisk),riskBsd=as.numeric(tmpB_mc$sdRisk), 
                                      riskCcv=as.numeric(tmpC_mc$sdRisk/tmpC_mc$mnRisk),riskBcv=as.numeric(tmpB_mc$sdRisk/tmpB_mc$mnRisk), 
                                    type=paste0(abs(limlist[l]),"% decline")))
                                }

                        }
                  }
            }
            


            CCIN0   =  C_019_CENaivecf_0_5_12_mc
            TTIN0   =  TempC_019_CENaivecf_0_5_12_mc
            CCIN130 =  C_2MT_019_CENaivecf1_0_5_13_mc
            TTIN130 =  TempC_2MT_019_CENaivecf1_0_5_13_mc

            BBIN0     =  B_019_CENaivecf_0_5_12_mc
            BBIN130   =  B_2MT_019_CENaivecf1_0_5_13_mc
            
            CCIN   =  C_219_CENaivecf_2_5_12_mc
            TTIN   =  TempC_219_CENaivecf_2_5_12_mc
            CCIN13 =  C_2MT_219_CENaivecf1_2_5_13_mc
            TTIN13 =  TempC_2MT_219_CENaivecf1_2_5_13_mc

            BBIN         <-  B_219_CENaivecf_2_5_12_mc
            BBIN13       <-  B_2MT_219_CENaivecf1_2_5_13_mc
            s            <-  1
            itr          <-  1
            adj          <-  0
            yrs          <-  as.numeric(rownames(C_219_CENaivecf_2_5_12_mc[s,itr,,]))
            hind_yrs     <-  match(1979:2017,yrs)
            fut_yrs      <-  match(2018:max(yrs),yrs)
            ref_yrs      <-  match(yrs,2007:2017)
            sim          <-  rcp45_n
            sim          <-  rcp85NoBio_n  # don't include the bio runs
            rndNIN       <-  6
            boot_nobsIN  <-  1000  # this controls the sensitivity of the anlysis
            nitrIN       <-  1000
            knottINtmp   <-  4 
            methodIN     <-  2

          # Now get thresholds:
            print("Now running threshold analyses... ")
            
            tmpall13_1  <-  threshold(s=1,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=CCIN13,TempIn=TTIN13,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            tmpall13_2  <-  threshold(s=2,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=CCIN13,TempIn=TTIN13,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            tmpall13_3  <-  threshold(s=3,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=CCIN13,TempIn=TTIN13,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)

            tmpall12_1  <-  threshold(s=1,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=CCIN,TempIn=TTIN,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            tmpall12_2  <-  threshold(s=2,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=CCIN,TempIn=TTIN,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            tmpall12_3  <-  threshold(s=3,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=CCIN,TempIn=TTIN,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)

            tmpall13_1_20y  <-  threshold(s=1,smooth_yr=20,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=CCIN13,TempIn=TTIN13,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            tmpall13_2_20y  <-  threshold(s=2,smooth_yr=20,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=CCIN13,TempIn=TTIN13,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            tmpall13_3_20y  <-  threshold(s=3,smooth_yr=20,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=CCIN13,TempIn=TTIN13,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)


            print("threshold part 1 complete... ")
            Btmpall12_1  <-  threshold(s=1,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=BBIN,TempIn=TTIN,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            Btmpall12_2  <-  threshold(s=2,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=BBIN,TempIn=TTIN,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            Btmpall12_3  <-  threshold(s=3,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=BBIN,TempIn=TTIN,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)

            Btmpall13_1  <-  threshold(s=1,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=BBIN13,TempIn=TTIN13,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            Btmpall13_2  <-  threshold(s=2,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=BBIN13,TempIn=TTIN13,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            Btmpall13_3  <-  threshold(s=3,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=BBIN13,TempIn=TTIN13,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)

            print("threshold part 2 complete... ")
            tmpall12_1_0  <-  threshold(s=1,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=CCIN0,TempIn=TTIN0,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            tmpall12_2_0  <-  threshold(s=2,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=CCIN0,TempIn=TTIN0,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            tmpall12_3_0  <-  threshold(s=3,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=CCIN0,TempIn=TTIN0,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)

            tmpall13_1_0  <-  threshold(s=1,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=CCIN130,TempIn=TTIN130,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            tmpall13_2_0  <-  threshold(s=2,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=CCIN130,TempIn=TTIN130,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            tmpall13_3_0  <-  threshold(s=3,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=CCIN130,TempIn=TTIN130,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            
            print("threshold part 3 complete... ")
            Btmpall12_1_0  <-  threshold(s=1,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=BBIN0,TempIn=TTIN0,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            Btmpall12_2_0  <-  threshold(s=2,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=BBIN0,TempIn=TTIN0,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            Btmpall12_3_0  <-  threshold(s=3,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=BBIN0,TempIn=TTIN0,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)

            Btmpall13_1_0  <-  threshold(s=1,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=BBIN130,TempIn=TTIN130,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            Btmpall13_2_0  <-  threshold(s=2,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=BBIN130,TempIn=TTIN130,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            Btmpall13_3_0  <-  threshold(s=3,knottIN=knottINtmp,simul_set=c(5,6,8,9,10,11),Catch=BBIN130,TempIn=TTIN130,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
           
             print("threshold part 4 complete... ")
            tmp45_13  <-  threshold(knottIN=knottINtmp,simul_set=c(5,8,10),Catch=CCIN13,TempIn=TTIN13,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            tmp85_13  <-  threshold(knottIN=knottINtmp,simul_set=c(6,9,11),Catch=CCIN13,TempIn=TTIN13,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            tmp45_12  <-  threshold(knottIN=knottINtmp,simul_set=c(5,8,10),Catch=CCIN,TempIn=TTIN,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
            tmp85_12  <-  threshold(knottIN=knottINtmp,simul_set=c(6,9,11),Catch=CCIN,TempIn=TTIN,boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)

            print("SUB_EBM_paper script complete without errors")



