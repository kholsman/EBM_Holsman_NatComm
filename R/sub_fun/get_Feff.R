
get_Feff<-function(
  N_tmp=c(100,90,70,30)*100,
  wt_fut=.4*(1:4)^.2,
  fsh_sel=c(0,.2,.3,.5),
  TACin=400,M1=c(.3,.2,.2,.2),
  M2_fut=c(.8,.1,0,0),nitr=10 ){
  
  dd   = 10
  cc   = TACin
  btmp = sum(fsh_sel*N_tmp*wt_fut) #temp fishable biomass
  ftmp = 0
  
  if(btmp>0.1) ftmp = 1.2*TACin/btmp
  Fatmp = ftmp * fsh_sel
  Z_tmp = Fatmp + M1+ M2_fut
  S_tmp = exp(-Z_tmp)
  
  for (i in 1:nitr){
    if(btmp >  0.1)     ftmp = ftmp*exp((TACin-cc) / btmp)
    if(btmp <= 0.1)     ftmp = 0
    if(ftmp >  2.)      ftmp = 2. 
    Fatmp = ftmp * fsh_sel
    Z_tmp = Fatmp + M1 + M2_fut 
    S_tmp = exp( -Z_tmp )
    
    cc = sum((Fatmp/Z_tmp)*(1.-S_tmp)*N_tmp*wt_fut)
   
    dd = cc / (TACin - 1)
    dd = TACin-cc
  }
  return(ftmp)
}

if(1==10){
  
  N1    <- c(100,90,70,30)*100
  wt1   <- .4*(1:4)^.2
  sel   <- c(0,.2,.3,.5)
  M1IN  <- c(.1,.2,.2,.2)
  M2IN  <- c(.8,.1,0,0)
  Ftest <- .491
  ZIn   <- Ftest*sel + M1IN+ M2IN
  SIn   <- exp(-ZIn)
  tacIN <- sum( ( (Ftest*sel)/ZIn ) * (1-SIn) * N1*wt1) #400
  
  get_Feff(TACin   = tacIN,
           N_tmp   = N1,
           wt_fut  = wt1,
           fsh_sel = sel,
           M1      = M1IN,
           M2_fut  = M2IN,
           nitr    = 30)

}
