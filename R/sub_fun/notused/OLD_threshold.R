OLDthreshold<-function(x=seq(-3,10,.1),
                       Catch=C_019_CENaivecf_0_5_12_mc,
                       TempIn=TempC_019_CENaivecf_0_5_12_mc,
                       subyr=fut_yrs,
                       simul_set=c(6,9,11),
                       adj1=adj,
                       rndN=2,
                       rndN2=6,
                       s=1,
                       boot_n=1000,
                       boot_nobs=24600,#500,
                       probIN=c(.025,.5,.975),
                       knottIN=4){
  # get detla C
  # From Large et al. "The shape of the relationship between a response and pressure is captured in the smoothing function s(X). 
  # Values of the pressure variable that influence the response in a particular direction can be enumerated by 
  # recognizing qualities of the shape of the smoothing function. The first derivative sˆ’(X)of s(X)indicates 
  # regions where a pressure variable causes a negative [sˆ’(X) , 0] or positive [sˆ’(X) . 0] response to an 
  # ecological indicator. Further, the second derivative sˆ”(X) denotes regions where ˆs’(X) changes sign and 
  # a threshold is crossed [0 , sˆ”(X) . 0]. To measure the uncertainty surrounding both sˆ’(X)and sˆ”(X), 
  # we estimated the first and second derivatives using finite differ- ences for each bootstrap replicated 
  # smoothing term sbr(X). Both ˆsi’(X)and sˆi”(X) were sorted into ascending order and the value of the 
  # 2.5% and 97.5% quantiles of sˆi’(X)and sˆi”(X)were considered the 95% CI for the first and second derivative 
  # of the smoothing function (Buckland, 1984). A significant trend sˆ’(X) or threshold sˆ”(X) was identified 
  # when the 95% CI crossed zero for either derivative (Fewster et al., 2000; Lindegren et al., 2012). "
  # from Samhouri et al. 21
  #and red dotted arrow indicates the best estimate of the location of the threshold (i.e., where the second 
  #  derivative is most difference from zero within the threshold range). See
  require(dplyr)
  require( quantmod )
  deltaC<-NA
  TempC<-NA
  for(simul in simul_set){
    deltaC<-c(deltaC, as.vector((Catch[s,,subyr,simul]-Catch[s,,subyr,1])/Catch[s,,subyr,1]))
    TempC<-c(TempC,as.vector(TempIn[s,,subyr,simul]))
  }
  deltaC<-deltaC[-1]
  TempC<-TempC[-1]
  
  # Fit gam
  require(mgcv)
  GAM<-gam(deltaC~s(TempC,k=knottIN,bs="tp"))
  hat<-predict(GAM,se.fit=TRUE,newdata=data.frame(TempC=x))
  dd<-data.frame(deltaC,TempC,round(TempC,2))
  dd$num<-1:length(dd[,1])
  Deriv1<-Deriv2<-hatFit<-hatse<-matrix(NA,boot_n,length(x))
  gmlist<-list()
  # now bootstrap for error:
  for(int in 1:boot_n){
    #We measured uncertainty surrounding each GAM by using a
    #naive bootstrap with random sampling and replacement. 
    #For each indicator–pressure combination, bootstrap replicates 
    #(br ¼ i ... 1000) were selected from the raw data and each 
    #bri was fitted with a GAM. 
    
    # For pressure–state relationships identified as
    # nonlinear, we defined the location of the threshold as the inflection point, that is, 
    # the value of the pres- sure where the second derivative changed sign (Fewster et al. 2000, 
    #   Bestelmeyer et al. 2011, Sam- houri et al. 2012, Large et al. 2013). For these 
    # anal- yses, we calculated the 95% CI of the smoothing function itself, along with 
    # its second derivative, via bootstrapping of the residuals in order to allow for autocorrelation. This
    
    nobs<-length(dd$num)
    if(boot_nobs>nobs) boot_nobs<-nobs
    bootd<-sample_n(dd,boot_nobs,replace = TRUE)
    tmpgam<-gam(deltaC~s(TempC,k=knottIN,bs="tp"),data=bootd)
    gmlist[[int]]<-tmpgam
    # Deriv1[int,]<-tmpd$fd_d1
    # Deriv2[int,]<-tmpd$fd_d2
    hatFit[int,]<-predict(tmpgam,se.fit=TRUE,newdata=data.frame(TempC=x))$fit
    hatse[int,]<-predict(tmpgam,se.fit=TRUE,newdata=data.frame(TempC=x))$se
  }
  
  nobs<-length(x)
  
  dx<-(x[2:nobs]-x[1:(nobs-1)])
  mxdx<-matrix(dx,boot_n,nobs-1)
  
  # get dys for qntl
  mcdy<-(hatFit[,2:nobs]-hatFit[,1:(nobs-1)])/(mxdx)
  nobs2<-length(mcdy[1,])
  mcdydy<-(mcdy[,2:nobs2]-mcdy[,1:(nobs2-1)])/(mxdx[,2:nobs2])
  
  # get 95%CI
  qnt<-apply(hatFit,2,quantile,probs=probIN)
  qnt_mcdy<-apply(mcdy,2,quantile,probs=probIN)
  qnt_mcdydy<-apply(mcdydy,2,quantile,probs=probIN)
  
  # get dy of mean gam fit
  mn_dy<-(hat$fit[2:nobs]-hat$fit[1:(nobs-1)])/(dx)
  mn_dydy<-(mn_dy[2:nobs2]-mn_dy[1:(nobs2-1)])/(dx[-1])
  
  hat_qnt<-data.frame(tmp=x,
                      up=hat$fit+qnt[1,]-qnt[2,],
                      mn=hat$fit,
                      dwn=hat$fit+qnt[3,]-qnt[2,])
  
  
  df1_qnt<-data.frame(tmp=x[-length(x)]+abs(x[-length(x)]-x[-1])/2,
                      up=mn_dy+qnt_mcdy[1,]-qnt_mcdy[2,],
                      mn=mn_dy,
                      dwn=mn_dy+qnt_mcdy[3,]-qnt_mcdy[2,])
  
  df2_qnt<-data.frame(tmp=x1[-length(x1)]+abs(x1[-length(x1)]-x1[-1])/2,
                      up=mn_dydy+qnt_mcdydy[1,]-qnt_mcdydy[2,],
                      mn=mn_dydy,
                      dwn=mn_dydy+qnt_mcdydy[3,]-qnt_mcdydy[2,])
  
  getdelta<-function(xx,rnd=rndN2){
    nn<-length(xx)
    xx<-round(xx,rndN2)
    delta<-rep(NA,nn)
    updn <- c(0, diff(sign(xx)))
    #updn[xx==0]<-0
    ix <- which(updn != 0)
    #(xx[ix] + xx[ix-1])/2
    sign(updn)[ix]
    delta[ix]<-1
    return(list(delta=delta,ix=ix,updn=updn,xx=xx))
  }
  
  phases<-phases2<-NA
  nn<-length(df2_qnt$up)
  dwn_phases<-up_phases<-df2_qnt$up*0
  phases2<-NA*dwn_phases
  if(length(getdelta(xx=df2_qnt$up)$ix)>0)
    for(p in 1:length(getdelta(xx=df2_qnt$up)$ix))
      up_phases[getdelta(xx=df2_qnt$up)$ix[p]:nn]<-up_phases[getdelta(xx=df2_qnt$up)$ix[p]:nn]+1
  #up_phases[up_phases==0]<-NA
  if(length(getdelta(xx=df2_qnt$dwn)$ix)>0)
    for(p in 1:length(getdelta(xx=df2_qnt$dwn)$ix))
      dwn_phases[getdelta(xx=df2_qnt$dwn)$ix[p]:nn]<-dwn_phases[getdelta(xx=df2_qnt$dwn)$ix[p]:nn]+1
  #dwn_phases[dwn_phases==0]<-NA
  phases2[sign(getdelta(xx=df2_qnt$up)$xx)==sign(getdelta(xx=df2_qnt$dwn)$xx)]<-1
  phases2<-phases2* apply( cbind(up_phases,dwn_phases),1,max)
  plot(df2_qnt$tmp,df2_qnt$up, ylim=c(-.25,.25),type="l")
  lines(df2_qnt$tmp,df2_qnt$dwn, ylim=c(-.25,.25),type="l");abline(h=0)
  points(df2_qnt$tmp,df2_qnt$dwn,col=rainbow(10)[up_phases],pch=phases2)
  points(df2_qnt$tmp,df2_qnt$up,col=rainbow(10)[dwn_phases],pch=phases2)
  
  nn<-length(df1_qnt$up)
  dwn_phases<-up_phases<-df1_qnt$up*0
  phases<-NA*dwn_phases
  if(length(getdelta(xx=df1_qnt$up)$ix)>0)
    for(p in 1:length(getdelta(xx=df1_qnt$up)$ix))
      up_phases[getdelta(xx=df1_qnt$up)$ix[p]:nn]<-up_phases[getdelta(xx=df1_qnt$up)$ix[p]:nn]+1
  
  if(length(getdelta(xx=df1_qnt$dwn)$ix)>0)
    for(p in 1:length(getdelta(xx=df1_qnt$dwn)$ix))
      dwn_phases[(getdelta(xx=df1_qnt$dwn)$ix[p]):nn]<-dwn_phases[(getdelta(xx=df1_qnt$dwn)$ix[p]):nn]+1
  phases[sign(df1_qnt$up)==sign(df1_qnt$dwn)]<-(up_phases+dwn_phases)[sign(df1_qnt$up)==sign(df1_qnt$dwn)]
  phases[phases==0]<-NA
  
  
  signif<-rep(NA,nobs)
  aa<-which(round(as.numeric(df1_qnt$up),rndN)<0&round(as.numeric(df1_qnt$dwn),rndN)<0)
  bb<-which(round(as.numeric(df1_qnt$up),rndN)>0&round(as.numeric(df1_qnt$dwn),rndN)>0)
  if(length(aa)>0) signif[aa]<-hat_qnt$mn[aa+1]
  if(length(bb)>0) signif[bb]<-hat_qnt$mn[bb+1]
  
  thrsh_max1<-thrsh_max2<-NA
  
  if(length(phases)>0){
    mm<-rep(NA,length(phases))
    mm[phases>0]<-1
    signif<-signif*c(NA,mm)
    if(max(phases,na.rm=T)>0){
      ph<-data.frame(phase=unique(phases),thrsh=NA,ix=NA,absval=NA)
      ph<-ph[is.na(ph$phase)==FALSE,]
      jj<-0
      mxvec<-apply( abs(data.frame(dwn=df1_qnt$dwn,
                                   up=df1_qnt$up)),1,max)
      nums<-1:length(mxvec)
      
      for(pp in as.numeric(ph$phase)){
        jj<-jj+1
        ph$absval[jj]<-mxvec[ which(phases==pp)][which(mxvec[ which(phases==pp)]==max(mxvec[ which(phases==pp)]))]
        ph$ix[jj]<-nums[which(phases==pp)][which(mxvec[ which(phases==pp)]==max(mxvec[ which(phases==pp)]))]
        ph$thrsh[jj]<-df1_qnt$tmp[which(phases==pp)][which(mxvec[ which(phases==pp)]==max(mxvec[ which(phases==pp)]))]
        
      }
      thrsh_max1<-ph
    }
    
  }
  
  sigthresh<-rep(NA,nobs)
  aa<-which(round(as.numeric(df2_qnt$up),rndN)<0&round(as.numeric(df2_qnt$dwn),rndN)<0)
  bb<-which(round(as.numeric(df2_qnt$up),rndN)>0&round(as.numeric(df2_qnt$dwn),rndN)>0)
  if(length(aa)>0) sigthresh[aa]<-hat_qnt$mn[aa+1]
  if(length(bb)>0) sigthresh[bb]<-hat_qnt$mn[bb+1]
  
  if(length(phases2)>0){
    mm<-rep(NA,length(phases2))
    mm[phases2>0]<-1
    sigthresh<-signif*c(NA,NA,mm)
    
    if(max(phases2,na.rm=T)>0){
      ph<-data.frame(phase=unique(phases2),thrsh=NA,ix=NA,absval=NA)
      ph<-ph[is.na(ph$phase)==FALSE,]
      jj<-0
      mxvec<-apply( abs(data.frame(dwn=df2_qnt$dwn,
                                   up=df2_qnt$up)),1,max)
      nums<-1:length(mxvec)
      
      for(pp in as.numeric(ph$phase)){
        jj<-jj+1
        ph$absval[jj]<-mxvec[ which(phases2==pp)][which(mxvec[ which(phases2==pp)]==max(mxvec[ which(phases2==pp)]))]
        ph$ix[jj]<-nums[which(phases2==pp)][which(mxvec[ which(phases2==pp)]==max(mxvec[ which(phases2==pp)]))]
        ph$thrsh[jj]<-df2_qnt$tmp[which(phases2==pp)][which(mxvec[ which(phases2==pp)]==max(mxvec[ which(phases2==pp)]))]
        
      }
      thrsh_max2<-ph
    }
    
  }
  
  return(list(deltaC=deltaC,TempC=TempC,
              hat=hat_qnt,fdif1=df1_qnt,
              fdif2=df2_qnt,
              signif=signif,sigthresh=sigthresh,
              thrsh_max1=thrsh_max1,
              thrsh_max2=thrsh_max2,GAM=GAM))
}