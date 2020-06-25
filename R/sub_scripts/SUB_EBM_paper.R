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
  cat("\n running SUB_EMB_paper.R....")
  tmp_ls <- ls()
  # ------------------------------------------------
  # Multispecies assessment simulations (run in ADMB)
  #
  # using climate naive reference points for BO
  # but climate specific B40 and projections (climate effects
  # on growth, M2, and recruitment)
  # ------------------------------------------------
    
      # Catch = F40 (hcr =1.8) scenarios
        # dat_2_5_3 <- sim_msm%>%filter(recMode==as.character(rset),hMode=="3", is.na(MC_n)==T)
        # dat_0_5_3 <- sim_ssm%>%filter(recMode==as.character(rset),hMode=="3", is.na(MC_n)==T)
      # Catch = ABC scenarios
        dat_2_5_12 <- sim_msm%>%filter(recMode==as.character(rset),hMode=="12", is.na(MC_n)==T)
      
      # Catch = attach(ABC) , i.e. simulated ABC  --> TAC --> Catch
        dat_2_5_13 <- sim_msm%>%filter(recMode==as.character(rset),hMode=="13", is.na(MC_n)==T)
    
      # Random recruitment draws included as MC:
        #dat_2_5_3_mc  <- sim_msm%>%filter(recMode==as.character(rset),hMode=="3", MC_n>0)
        dat_2_5_12_mc <- sim_msm%>%filter(recMode==as.character(rset),hMode=="12", MC_n>0)
        dat_2_5_13_mc <- sim_msm%>%filter(recMode==as.character(rset),hMode=="13", MC_n>0)
        
        preview(datIN=as_tibble(dat_2_5_12_mc)%>%filter(age==6,Scenario%in%c(1,9)),var="ABC_total_biom")
     
   if(update.outputs){
     # save(list=c("dat_2_5_3"
     # ),file=file.path(out_dir,"2_5_3_nohcr_simulations.Rdata")) 
     # save(list=c("dat_0_5_3"
     # ),file=file.path(out_dir,"0_5_3_nohcr_simulations.Rdata")) 
     # 
    save(list=c("dat_2_5_12",
                 "dat_2_5_12_mc"
                 ),file=file.path(out_dir,"multispp_nocap_simulations.Rdata"))   
    save(list=c("dat_2_5_13",
                 "dat_2_5_13_mc"
     ),file=file.path(out_dir,"multispp_cap_simulations.Rdata"))  
   }
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
       if(update.outputs)
         save(list=c("risk12","risk13"),file=file.path(out_dir,"risk.Rdata"))  
       
       
   # ------------------------------------------------
   # Threshold analyses:Evaluate tipping points and thresholds of deltaC and temperature:
   # ------------------------------------------------
      
      cat("\n Now running threshold analyses... ")
                
      tmpMC_13      <- dat_2_5_13_mc                  %>%
        filter(age==6,Scenario%in%Scenario_set, 
               hModev2=="H13_2MT_219_CENaivecf1")  %>%
        select(names(dat_2_5_13),sp = species)     %>%
        mutate(Year = start_yr + future_year-1)
      
      tmpMC_12      <- dat_2_5_12_mc                  %>%
        filter(age==6,Scenario%in%Scenario_set, 
               hModev2=="H12_219_CENaivecf")  %>%
        select(names(dat_2_5_12),sp = species)     %>%
        mutate(Year = start_yr + future_year-1)
    
    
      # First for delta Catch:
      # ---------------------------------------------
      tmpd <-  calcDelta(datIN     = tmpMC_13,
                limm      = -10,
                delta_var_nm = "Catch_total_biom" ) 
      
      C_thresh_13_1  <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      C_thresh_13_2  <-  threshold(datIN = tmpd%>%filter(sp==2),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      C_thresh_13_3  <-  threshold(datIN = tmpd%>%filter(sp==3),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
     
      C_thresh_45_13    <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(5,8,10),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      C_thresh_85_13    <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(6,9,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      
      C_thresh_13_1_20y  <-  threshold(datIN = tmpd%>%filter(sp==1),smooth_yr=20,knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      C_thresh_13_2_20y  <-  threshold(datIN = tmpd%>%filter(sp==2),smooth_yr=20,knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      C_thresh_13_3_20y  <-  threshold(datIN = tmpd%>%filter(sp==3),smooth_yr=20,knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      (rm(tmpd))
      tmpd <-  calcDelta(datIN     = tmpMC_12,
                         limm      = -10,
                         delta_var_nm = "Catch_total_biom" ) 
      
      C_thresh_12_1  <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      C_thresh_12_2  <-  threshold(datIN = tmpd%>%filter(sp==2),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      C_thresh_12_3  <-  threshold(datIN = tmpd%>%filter(sp==3),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      
      C_thresh_45_12    <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(5,8,10),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      C_thresh_85_12    <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(6,9,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      
      cat("\n threshold part 1 complete... ")
      
      # Then for delta Biomass:
      # ---------------------------------------------
      (rm(tmpd))
      tmpd <-  calcDelta(datIN     = tmpMC_13,
                         limm      = -10,
                         delta_var_nm = "SSB_total_biom" ) 
      
      B_thresh_13_1  <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      B_thresh_13_2  <-  threshold(datIN = tmpd%>%filter(sp==2),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      B_thresh_13_3  <-  threshold(datIN = tmpd%>%filter(sp==3),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      (rm(tmpd))
      tmpd <-  calcDelta(datIN     = tmpMC_12,
                         limm      = -10,
                         delta_var_nm = "SSB_total_biom" )   
      
      B_thresh_12_1  <-  threshold(datIN = tmpd%>%filter(sp==1),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      B_thresh_12_2  <-  threshold(datIN = tmpd%>%filter(sp==2),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
      B_thresh_12_3  <-  threshold(datIN = tmpd%>%filter(sp==3),knotsIN=t_knots,simul_set=c(5,6,8,9,10,11),boot_nobs=boot_nobsIN,rndN=rndNIN,method=methodIN,boot_n=nitrIN)
                
    print("\n threshold analysis complete... ")
    if(update.outputs){
      
      save(list=c(
        "C_thresh_13_1",
        "C_thresh_13_2",
        "C_thresh_13_3",
        "C_thresh_45_13",
        "C_thresh_85_13",
        "C_thresh_13_1_20y",
        "C_thresh_13_2_20y",
        "C_thresh_13_3_20y",
        "C_thresh_12_1",
        "C_thresh_12_2",
        "C_thresh_12_3",
        "C_thresh_45_12",
        "C_thresh_85_12"),file=file.path(out_dir,"Catch_thresholds.Rdata"))
        
      save(list=c(
        "B_thresh_13_1",
        "B_thresh_13_2",
        "B_thresh_13_3",
        "B_thresh_12_1",
        "B_thresh_12_2",
        "B_thresh_12_3"
      ),file=file.path(out_dir,"Biomass_thresholds.Rdata"))
     
    }
   
print("SUB_EBM_paper script complete without errors")




