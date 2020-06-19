## ------------------------------------------------
## K. Holsman 
## June 2020
## Kirstin.holsman@noaa.gov
##
## SUB_EBM_paper.R
## This code gathers projections from the CEATTLE multispecies assessment 
## (run in ADMB) and reshapes them for analysis and plotting
## and runs the risk and threshold analyses
## For information on the assessment model see:
## 
## ------------------------------------------------

  source("R/make.R")       # loads packages, data, setup, etc.

  # ------------------------------------------------
  # Multispecies assessment simulations (run in ADMB)
  #
  # using climate naive reference points for BO
  # but climate specific B40 and projections (climate effects
  # on growth, M2, and recruitment)
  # ------------------------------------------------
    
      # Catch = ABC scenarios
        dat_2_5_12 <- sim_msm%>%filter(recMode==as.character(rset),hMode=="12", is.na(MC_n)==T)
      # Catch = attach(ABC) , i.e. simulated ABC  --> TAC --> Catch
        dat_2_5_13 <- sim_msm%>%filter(recMode==as.character(rset),hMode=="13", is.na(MC_n)==T)
    
      # Random recruitment draws included as MC:
        dat_2_5_3_mc  <- sim_msm%>%filter(recMode==as.character(rset),hMode=="3", MC_n>0)
        dat_2_5_12_mc <- sim_msm%>%filter(recMode==as.character(rset),hMode=="12", MC_n>0)
        dat_2_5_13_mc <- sim_msm%>%filter(recMode==as.character(rset),hMode=="13", MC_n>0)
        
        dat2 <- as_tibble(dat_2_5_12_mc)%>%filter(age==6,Scenario%in%c(1,9))
        preview(datIN=dat2,var="ABC_total_biom")
        
  # ------------------------------------------------
  # Risk Evaluations:Find the risk of collapse and decline in catch for each simulation:
  # ------------------------------------------------
  
       # preallocate risk df:
       nbin            <-  length(Yrbin)
       risk            <-  data.frame(matrix(NA,length(esmlist),nbin-1))
       colnames(risk)  <-  paste(paste0("(",Yrbin[-nbin]),paste0(Yrbin[-1],"]"),sep=",")
       rownames(risk)  <-  1:length(esmlist)
       rownames(risk)  <-  c("rcp45","rcp85")
       mmm             <-  "MSM"
       
       #for(mmm in modeLIST){
       cat("first run risk eval for no cap simulations:")
       for (l in 1:length(limlist)){
           cat(paste0("\n running risk of ",-1*limlist[l],"% decline..."))
           dat2_mc      <- dat_2_5_12_mc%>%filter(age==6,Scenario%in%Scenario_set, 
                                                  hModev2=="H12_219_CENaivecf")%>%mutate(Year = start_yr + future_year-1)
           dat2         <- dat_2_5_12%>% filter(age==6,Scenario%in%Scenario_set, 
                                                hModev2=="H12_219_CENaivecf")%>%mutate(Year = start_yr + future_year-1)
           tmpOUT       <- calcRisk(dat2IN =dat2, dat2IN_mc=dat2_mc,limitIN = limlist[l]) 
           tmpOUT$type  <- paste0(-1*limlist[l],"% decline")
           tmpOUT$mode  <- mmm
           
           
           #if(mmm==modeLIST[1]&l==1){ 
           if(l==1){ 
             risk12 <- tmpOUT
           }else{
             risk12 <- rbind(risk12,tmpOUT)
             
           }
         }
       cat("\n Now simulations with the cap:")
       for (l in 1:length(limlist)){
           cat(paste0("\n running risk of ",-1*limlist[l],"% decline..."))
           dat2_mc      <- dat_2_5_13_mc%>%filter(age==6,Scenario%in%Scenario_set, 
                                                  hModev2=="H13_2MT_219_CENaivecf1")%>%mutate(Year = start_yr + future_year-1)
           dat2         <- dat_2_5_13%>% filter(age==6,Scenario%in%Scenario_set, 
                                                hModev2=="H13_2MT_219_CENaivecf1")%>%mutate(Year = start_yr + future_year-1)
           
           tmpOUT       <- calcRisk(dat2IN =dat2, dat2IN_mc=dat2_mc,limitIN = limlist[l]) 
           tmpOUT$type   <- paste0(-1*limlist[l],"% decline")
           tmpOUT$mode   <- mmm
           
           if(l==1){ 
             risk13 <- tmpOUT
           }else{
             risk13 <- rbind(risk13,tmpOUT)
           }
         }
       #}
       
   # ------------------------------------------------
   # Threshold analyses:Evaluate tipping points and thresholds of deltaC and temperature:
   # ------------------------------------------------
      
      cat("\n Now running threshold analyses... ")
                
      tmpMC_13      <- dat_2_5_13_mc                  %>%
        filter(age==6,Scenario%in%Scenario_set, 
               hModev2=="H13_2MT_219_CENaivecf1")  %>%
        select(names(dat_2_5_13),sp = species)     %>%
        mutate(Year = start_yr + future_year-1)
      
      tmpMC      <- dat_2_5_12_mc                  %>%
        filter(age==6,Scenario%in%Scenario_set, 
               hModev2=="H12_219_CENaivecf")  %>%
        select(names(dat_2_5_12),sp = species)     %>%
        mutate(Year = start_yr + future_year-1)
    
    
      # First for delta Catch:
      # ---------------------------------------------
      tmpd <-  calcDelta(datIN     = tmpMC_13,
                limm      = -10,
                delta_var_nm = "Catch_total_biom" ) 
      
      tmpall13_1  <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      tmpall13_2  <-  threshold(datIN = tmpd%>%filter(sp==2),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      tmpall13_3  <-  threshold(datIN = tmpd%>%filter(sp==3),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
     
      tmp45_13    <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(5,8,10),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      tmp85_13    <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(6,9,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      
      tmpall13_1_20y  <-  threshold(datIN = tmpd%>%filter(sp==1),smooth_yr=20,knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      tmpall13_2_20y  <-  threshold(datIN = tmpd%>%filter(sp==2),smooth_yr=20,knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      tmpall13_3_20y  <-  threshold(datIN = tmpd%>%filter(sp==3),smooth_yr=20,knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
    
      tmpd <-  calcDelta(datIN     = tmpMC_12,
                         limm      = -10,
                         delta_var_nm = "Catch_total_biom" ) 
      
      tmpall12_1  <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      tmpall12_2  <-  threshold(datIN = tmpd%>%filter(sp==2),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      tmpall12_3  <-  threshold(datIN = tmpd%>%filter(sp==3),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      
      tmp45_12    <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(5,8,10),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      tmp85_12    <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(6,9,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      
      cat("\n threshold part 1 complete... ")
      
      # Then for delta Biomass:
      # ---------------------------------------------
      
      tmpd <-  calcDelta(datIN     = tmpMC_13,
                         limm      = -10,
                         delta_var_nm = "SSB_total_biom" ) 
      
      Btmpall13_1  <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      Btmpall13_2  <-  threshold(datIN = tmpd%>%filter(sp==2),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      Btmpall13_3  <-  threshold(datIN = tmpd%>%filter(sp==3),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
    
      tmpd_12 <-  calcDelta(datIN     = tmpMC,
                         limm      = -10,
                         delta_var_nm = "SSB_total_biom" )   
      
      Btmpall12_1  <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      Btmpall12_2  <-  threshold(datIN = tmpd%>%filter(sp==2),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      Btmpall12_3  <-  threshold(datIN = tmpd%>%filter(sp==3),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
                
    print("\n threshold analysis complete... ")
   
print("SUB_EBM_paper script complete without errors")



