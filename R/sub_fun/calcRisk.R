#calcRiskV2.R

calcRisk<-function(dat2IN,dat2IN_mc,limitIN = limlist[l]){
  cat("calculating risk, may take a few mins.")
  # get subset for each delta calculation:
  datuse_B      <- dat2IN    %>% select("MC_n",sp=species,"Scenario","Year","bottomT_C","SSB_total_biom")
  datuse_C      <- dat2IN    %>% select("MC_n",sp=species,"Scenario","Year","bottomT_C","Catch_total_biom")
  datuseMCMC_B  <- dat2IN_mc %>% select("MC_n",sp=species,"Scenario","Year","bottomT_C","SSB_total_biom")
  datuseMCMC_C  <- dat2IN_mc %>% select("MC_n",sp=species,"Scenario","Year","bottomT_C","Catch_total_biom")
  
  # Now calculate risk:
  tmpC <- calcDelta(datIN     = datuse_C,
                    limm      = limitIN,
                    delta_var_nm = "Catch_total_biom" )      %>%
    group_by(  YrBin, rcp, sp, delta_var_nm, belowLim)       %>%
    select(    YrBin, rcp, sp, delta_var_nm, belowLim)       %>%
    summarize( A = sum(belowLim), B = length(belowLim) )  %>%
    group_by(  YrBin, rcp, sp )                           %>% 
    summarise( sumA = sum(A), sumB = sum(B) )             %>%
    mutate(    riskC = 100 * round( sumA/sumB ,4) )       %>%
    select(    YrBin, rcp, sp, riskC)
  
  tmpB <- calcDelta(datIN     = datuse_B,
                    limm      = limitIN,
                    delta_var_nm = "SSB_total_biom" )        %>%
    group_by(  YrBin, rcp, sp, delta_var_nm, belowLim)       %>%
    select(    YrBin, rcp, sp, delta_var_nm, belowLim)       %>%
    summarize( A = sum(belowLim), B = length(belowLim) )  %>%
    group_by(  YrBin, rcp, sp )                           %>% 
    summarise( sumA = sum(A), sumB = sum(B) )             %>%
    mutate(    riskB = 100 * round( sumA/sumB ,4) )       %>%
    select(    YrBin, rcp, sp, riskB)
  cat("..")
  tmpC_mc <- calcDelta(datIN     = datuseMCMC_C,
                       limm      = limitIN,
                       delta_var_nm = "Catch_total_biom" )   %>%
    group_by(  MC_n, YrBin, rcp, sp, delta_var_nm, belowLim) %>%
    select(    MC_n, YrBin, rcp, sp, delta_var_nm, belowLim) %>%
    summarize( A = sum(belowLim), B = length(belowLim) )  %>%
    group_by(  MC_n, YrBin, rcp, sp)                      %>% 
    summarise( sumA = sum(A), sumB = sum(B) )             %>%
    mutate(    risk = 100* round (sumA /sumB,4) )         %>%
    select(    MC_n, YrBin, rcp, sp, risk)                %>%
    group_by(  YrBin, rcp, sp)                            %>% 
    summarise( riskCmn = mean(risk), riskCsd = sd(risk))  %>%
    mutate(    riskCcv = riskCsd/riskCmn )                %>%
    select(    YrBin, rcp, sp, riskCmn, riskCsd, riskCcv)
  cat("..")
  tmpB_mc <- calcDelta(datIN     = datuseMCMC_B,
                       limm      = limitIN,
                       delta_var_nm = "SSB_total_biom" )     %>%
    group_by(  MC_n, YrBin, rcp, sp, delta_var_nm, belowLim) %>%
    select(    MC_n, YrBin, rcp, sp, delta_var_nm, belowLim) %>%
    summarize( A = sum(belowLim), B = length(belowLim) )  %>%
    group_by(  MC_n, YrBin, rcp, sp)                      %>% 
    summarise( sumA = sum(A), sumB = sum(B) )             %>%
    mutate(    risk = 100* round (sumA /sumB,4) )         %>%
    select(    MC_n, YrBin, rcp, sp, risk)                %>%
    group_by(  YrBin, rcp, sp)                            %>% 
    summarise( riskBmn = mean(risk), riskBsd = sd(risk))  %>%
    mutate(    riskBcv = riskBsd/riskBmn )                %>%
    select(    YrBin, rcp, sp, riskBmn, riskBsd, riskBcv)
  cat("..")
  tmp           <- merge(x=data.frame(tmpC),y=data.frame(tmpB),by=c("YrBin","rcp","sp"))
  tmp_mc        <- merge(x=data.frame(tmpC_mc),y=data.frame(tmpB_mc),by=c("YrBin","rcp","sp"))
  out           <- merge(x=data.frame(tmp),y=data.frame(tmp_mc),by=c("YrBin","rcp","sp"))
  cat("Complete")
  return(out)
}