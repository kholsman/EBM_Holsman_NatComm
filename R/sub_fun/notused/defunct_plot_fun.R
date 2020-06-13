# unused older functions

#-------------------------------------
# plot_aclimTS
#-------------------------------------
#' PLot ACLIM covariates
#'
#' @param dat is a data.frame with t=date and dat=data to plot
#' @param plotSet is the columns of dat that you want to plot (col=1 is the date, 2:ncol are options)
#' @param lwdd vector of the line widths; length of the projection simulation set + 1 for hindcast (dim of dat)
#' @param ltyy vector of the line types; length of the projection simulation set + 1 for hindcast (dim of dat)
#' @param ylab y axis label
#' @param xlab x axis label
#' @param title title for the graph
#' @param coll vector colors for each line; length of the projection simulation set + 1 for hindcast (dim of dat)
#' @param esnmCol colors for each ensemble polygon
#' @param esnmSet is a list with the set for each ensemble groups
#' @param alpha vector of transparencies of the ensemble mean; corresponds to number quantiles
#' @param add0line Add a horizontal line at 0?
#' @keywords Temperature, plot, data, ACLIM
#' @export 
#' @examples
plot_aclimTS<-function(
  dat,
  plotSet=list(c(2,6,8)),
  h=5,
  w=5,
  lwdd=rep(2,13),
  ltyy=rep(1,13),
  coll=col2(10),
  ylab="",
  xlab="Year", 
  title="",
  alpha = c(20,40), 
  esnmSet=list(c(6,9,11),c(7,10,12)), 
  esnmCol=col1(2),
  projLine=2017,
  prob=c(.01,.25,.50,.75,.99),
  add0line=FALSE){
  sp<-1
  
  yr<-dat[,1]
  ylim1<-c(min(dat[,-1]),max(dat[,-1]))
  plot(yr,dat[,plotSet[[1]][1]],type="l",ylim=ylim1,col="white",ylab=ylab,xlab=xlab,axes=F)
  abline(v=projLine,lwd=2,lty=2,col="gray")
  axis(1);axis(1,c(1500,2500))
  stp<-abs(ylim1[1]/10)
  if(stp==0) stp<-abs(ylim1[2]/30)
  yy<-pretty(seq(ylim1[1],ylim1[2],stp))
  
  axis(2,las=2,at=yy);axis(2,c(-1000,1000))
  axis(4,las=2,at=yy,lab=rep("",length(yy)))
  axis(4,c(-1000,1000))
  axis(3,c(1500,2500))
  if(add0line) abline(h=0)
  
  # get quantiles:
  npoly<-length(esnmSet)
  poly<-list()
  for(i in 1:npoly)
    poly[[i]]<-apply(dat[,esnmSet[[i]]],1,quantile,probs=prob)
  nq<-(length(prob)-1)/2  # number of quantile layers
  
  # add ensm polygons
  for(q in 1:nq){
    for(c in 1:npoly){
      xx<-c(yr,rev(yr))
      r1<-q
      r2<-length(prob)-q+1
      yy<-c(poly[[c]][r1,],rev(poly[[c]][r2,]))
      polygon(xx,yy, col=makeTransparent(esnmCol[c],alpha=alpha[q]),border=FALSE)
    }
  }
  
  # add each projection line
  rr<-which(dat[,1]<=projLine)
  lines(dat[rr,1],dat[rr,2],lty=ltyy[1],lwd=lwdd[1],col=coll[1])
  rr<-which(dat[,1]<projLine)
  nset<-length(plotSet)
  for(s in 1:nset)
    for(c in plotSet[[s]])
      lines(dat[-rr,1],dat[-rr,c],lty=ltyy[c],lwd=lwdd[c],col=coll[c])
}

#-------------------------------------
# plot_aclimCEATTLE_MCMC
#-------------------------------------
#' PLot ACLIM covariates
#'
#' @param dat is a data.frame with t=date and dat=data to plot
#' @param plotSet is the columns of dat that you want to plot (col=1 is the date, 2:ncol are options)
#' @param lwdd vector of the line widths; length of the projection simulation set + 1 for hindcast (dim of dat)
#' @param ltyy vector of the line types; length of the projection simulation set + 1 for hindcast (dim of dat)
#' @param ylab y axis label
#' @param xlab x axis label
#' @param title title for the graph
#' @param coll vector colors for each line; length of the projection simulation set + 1 for hindcast (dim of dat)
#' @param esnmCol colors for each ensemble polygon
#' @param esnmSet is a list with the set for each ensemble groups
#' @param alpha vector of transparencies of the ensemble mean; corresponds to number quantiles
#' @param add0line Add a horizontal line at 0?
#' @param addNew Add as a new plot?
#' @param legnd add lengend?
#' @keywords Temperature, plot, data, ACLIM
#' @export 
#' @examples
plot_aclimCEATTLEMCMC<-function(
  dat=dat1,
  sp=2,
  plotSet=list(c(2,6,8)),
  h=5,
  w=5,
  lwdd=rep(2,13),
  ltyy=rep(1,13),
  coll=col2(10),
  ydiv=1e6,
  ylab="",
  xlab="Year", 
  title="",
  alpha = c(20,40), 
  esnmSet=list(c(6,9,11),c(7,10,12)), 
  esnmCol=col1(2),
  projLine=2017,
  prob=c(.01,.25,.50,.75,.99),
  add0line=FALSE,
  addNew=TRUE,alphaLine=255,
  ylim1=NA){
  
  mn1<-apply(dat[sp,,,],2:3,mean)
  sd1<-apply(dat[sp,,,],2:3,sd)
  yr<-as.numeric(rownames(mn1))
  nset<-length(esnmSet)  # number of contrasts
  
  # get quantiles:
  qnt1<-array(0, c(nset,length(esnmSet[[1]]),length(yr),length(prob)))
  for(i in 1:nset){
    for(jj in 1:length(esnmSet[[i]]))
      qnt1[i,jj,,]<- t(apply(dat[sp,,,esnmSet[[i]][jj]],2,quantile,probs=prob))
  }
  
  if(addNew){
    if(is.na(ylim1[1])) ylim1=c(0,max(mn1))
    plot(yr,mn1[,plotSet[[1]][1]],type="l",ylim=ylim1,col="white",ylab=ylab,xlab=xlab,axes=F)
    abline(v=projLine,lwd=2,lty=2,col="gray")
    axis(1);axis(1,c(1500,2500))
    stp<-abs(ylim1[1]/10)
    if(stp==0) stp<-abs(ylim1[2]/30)
    yy<-pretty(seq(ylim1[1],ylim1[2],stp))
    axis(2,las=2,at=yy,labels=yy/ydiv);axis(2,c(-1e10,1e10))
    axis(4,las=2,at=yy,labels=rep("",length(yy)))
    axis(4,c(-1e10,1e10))
    axis(3,c(1500,2500))
    if(add0line) abline(h=0)
  }
  
  # get quantiles:
  npoly<-length(esnmSet)
  nq<-(length(prob)-1)/2  # number of quantile layers
  
  # add ensm polygons
  for(r in 1:nset){
    nscen<-length(esnmSet[[r]])
    for(q in 1:nq){
      for(c in 1:nscen){
        xx<-c(yr,rev(yr))
        r1<-q
        r2<-length(prob)-q+1
        yy<-c(qnt1[r,c,,r1],rev(qnt1[r,c,,r2]))
        polygon(xx,yy, col=makeTransparent(esnmCol[r],alpha=alpha[q]),border=FALSE)
      }
    }
  }
  for(r in 1:nset){
    q<-nq+1
    for(c in 1:nscen){
      xx<-c(yr,rev(yr))
      r1<-q
      r2<-length(prob)-q+1
      yy<-c(qnt1[r,c,,r1],rev(qnt1[r,c,,r2]))
      lines(yr,qnt1[r,c,,r1], col=makeTransparent(esnmCol[r],alpha=240))
    }
  }
  # # add each projection line
  # rr<-which(dat[[sp]][,1]<=projLine)
  # lines(dat[[sp]][rr,1],dat[[sp]][rr,2],lty=ltyy[1],lwd=lwdd[1],col=coll[1])
  # rr<-which(dat[[sp]][,1]<projLine)
  # nset<-length(plotSet)
  # for(s in 1:nset)
  #   for(c in plotSet[[s]])
  #     lines(dat[[sp]][-rr,1],dat[[sp]][-rr,c],lty=ltyy[c],lwd=lwdd[c],col=makeTransparent(coll[c],alphaLine))
  # 
}  

#-------------------------------------
# plot_aclimCEATTLE
#-------------------------------------
#' PLot ACLIM covariates
#'
#' @param dat is a data.frame with t=date and dat=data to plot
#' @param plotSet is the columns of dat that you want to plot (col=1 is the date, 2:ncol are options)
#' @param lwdd vector of the line widths; length of the projection simulation set + 1 for hindcast (dim of dat)
#' @param ltyy vector of the line types; length of the projection simulation set + 1 for hindcast (dim of dat)
#' @param ylab y axis label
#' @param xlab x axis label
#' @param title title for the graph
#' @param coll vector colors for each line; length of the projection simulation set + 1 for hindcast (dim of dat)
#' @param esnmCol colors for each ensemble polygon
#' @param esnmSet is a list with the set for each ensemble groups
#' @param alpha vector of transparencies of the ensemble mean; corresponds to number quantiles
#' @param add0line Add a horizontal line at 0?
#' @param addNew Add as a new plot?
#' @param legnd add lengend?
#' @keywords Temperature, plot, data, ACLIM
#' @export 
#' @examples
plot_aclimCEATTLE<-function(
  dat=B_2_5_3,
  sp=2,
  plotSet=list(c(2,6,8)),
  h=5,
  w=5,
  lwdd=rep(2,13),
  ltyy=rep(1,13),
  coll=col2(10),
  ydiv=1e6,
  ylab="",
  xlab="Year", 
  title="",
  alpha = c(20,40), 
  esnmSet=list(c(6,9,11),c(7,10,12)), 
  esnmCol=col1(2),
  projLine=2017,
  prob=c(.01,.25,.50,.75,.99),
  add0line=FALSE,
  addNew=TRUE,alphaLine=255,
  ylim1=NA){
  
  yr<-dat[[sp]][,1]
  if(addNew){
    if(is.na(ylim1[1])) ylim1=c(0,max(dat[[sp]][,-1]))
    plot(yr,dat[[sp]][,plotSet[[1]][1]],type="l",ylim=ylim1,col="white",ylab=ylab,xlab=xlab,axes=F)
    abline(v=projLine,lwd=2,lty=2,col="gray")
    axis(1);axis(1,c(1500,2500))
    stp<-abs(ylim1[1]/10)
    if(stp==0) stp<-abs(ylim1[2]/30)
    yy<-pretty(seq(ylim1[1],ylim1[2],stp))
    axis(2,las=2,at=yy,labels=yy/ydiv);axis(2,c(-1e10,1e10))
    axis(4,las=2,at=yy,labels=rep("",length(yy)))
    axis(4,c(-1e10,1e10))
    axis(3,c(1500,2500))
    if(add0line) abline(h=0)
  }
  
  # get quantiles:
  npoly<-length(esnmSet)
  poly<-list()
  for(i in 1:npoly)
    poly[[i]]<-apply(dat[[sp]][,esnmSet[[i]]],1,quantile,probs=prob)
  nq<-(length(prob)-1)/2  # number of quantile layers
  
  # add ensm polygons
  for(q in 1:nq){
    for(c in 1:npoly){
      xx<-c(yr,rev(yr))
      r1<-q
      r2<-length(prob)-q+1
      yy<-c(poly[[c]][r1,],rev(poly[[c]][r2,]))
      polygon(xx,yy, col=makeTransparent(esnmCol[c],alpha=alpha[q]),border=FALSE)
    }
  }
  
  # add each projection line
  rr<-which(dat[[sp]][,1]<=projLine)
  lines(dat[[sp]][rr,1],dat[[sp]][rr,2],lty=ltyy[1],lwd=lwdd[1],col=coll[1])
  rr<-which(dat[[sp]][,1]<projLine)
  nset<-length(plotSet)
  for(s in 1:nset)
    for(c in plotSet[[s]])
      lines(dat[[sp]][-rr,1],dat[[sp]][-rr,c],lty=ltyy[c],lwd=lwdd[c],col=makeTransparent(coll[c],alphaLine))
  
}

#-------------------------------------
# plot_cumlCatch
#-------------------------------------
#' PLot cumulative catch from each of the scenarios
#'
#' @param dat is a data.frame with t=date and dat=data to plot
#' @param plotSet is the columns of dat that you want to plot (col=1 is the date, 2:ncol are options)
#' @param lwdd vector of the line widths; length of the projection simulation set + 1 for hindcast (dim of dat)
#' @param ltyy vector of the line types; length of the projection simulation set + 1 for hindcast (dim of dat)
#' @param ylab y axis label
#' @param xlab x axis label
#' @param title title for the graph
#' @param coll vector colors for each line; length of the projection simulation set + 1 for hindcast (dim of dat)
#' @param esnmCol colors for each ensemble polygon
#' @param esnmSet is a list with the set for each ensemble groups
#' @param alpha vector of transparencies of the ensemble mean; corresponds to number quantiles
#' @param add0line Add a horizontal line at 0?
#' @param addNew Add as a new plot?
#' @keywords Temperature, plot, data, ACLIM
#' @export 
#' @examples
plotCumL<-function(
  datlist=list(C_0_5_3,C_0_5_12,C_0_5_13),
  cuml=TRUE,
  sp=2,
  plotSet=list(c(2,6,8)),
  h=5,
  w=5,
  lwdd=rep(2,13),
  ltyy=rep(1,13),
  coll=col2(10),
  ylab="",
  xlab="Year", 
  title="",
  alpha = c(20,40), 
  esnmSet=list(c(6,9,11),c(7,10,12)), 
  esnmCol=col1(2),
  projLine=2017,
  prob=c(.25,.50,.75),
  add0line=FALSE,
  addNew=TRUE,alphaLine=255,
  ylim1=NA){
  
  yr<-datlist[[1]][[sp]][,1]
  rr<-which(yr>0)
  neww<-0
  for(k in 1:length(datlist)){
    neww<-neww+1
    # for each comparitive simulation
    dat<-datlist[[k]][[sp]]
    if(cuml)
      dat[,-1]<-apply(dat[rr,],2,cumsum)[,-1]
    
    if(addNew&neww==1){
      if(is.na(ylim1[1])) ylim1=c(0,max(dat[,-1]))
      plot(yr,dat[,plotSet[[1]][1]],type="l",ylim=ylim1,col="white",ylab=ylab,xlab=xlab,axes=F)
      abline(v=projLine,lwd=2,lty=2,col="gray")
      axis(1);axis(1,c(1500,2500))
      stp<-abs(ylim1[1]/10)
      if(stp==0) stp<-abs(ylim1[2]/30)
      yy<-pretty(seq(ylim1[1],ylim1[2],stp))
      axis(2,las=2,at=yy);axis(2,c(-1e10,1e10))
      axis(4,las=2,at=yy,lab=rep("",length(yy)))
      axis(4,c(-1e10,1e10))
      axis(3,c(1500,2500))
      if(add0line) abline(h=0)
    }
    
    # get quantiles:
    npoly<-length(esnmSet)
    poly<-list()
    for(i in 1:npoly)
      poly[[i]]<-apply(dat[,esnmSet[[i]]],1,quantile,probs=prob)
    nq<-(length(prob)-1)/2  # number of quantile layers
    
    # add ensm polygons
    for(q in 1:nq){
      for(c in 1:npoly){
        xx<-c(yr,rev(yr))
        r1<-q
        r2<-length(prob)-q+1
        yy<-c(poly[[c]][r1,],rev(poly[[c]][r2,]))
        polygon(xx,yy, col=makeTransparent(esnmCol[c],alpha=alpha[q]),border=FALSE)
      }
    }
    
    # add each projection line
    rr<-which(dat[,1]<=projLine)
    lines(dat[rr,1],dat[rr,2],lty=ltyy[1],lwd=lwdd[1],col=coll[1])
    rr<-which(dat[,1]<projLine)
    nset<-length(plotSet)
    for(s in 1:nset)
      for(c in plotSet[[s]])
        lines(dat[-rr,1],dat[-rr,c],lty=ltyy[c],lwd=lwdd[c],col=makeTransparent(coll[c],alphaLine))
  }
}



deriv2<-function(gam_mod,simdat=x){
  # finite difference approach to derivatives following
  # example from ?predict.gam
  DF1=gam_mod$model
  eps <- 1e-7
  # new data for prediction
  newDF <- with(DF1, data.frame(TempC = simdat))
  
  # prediction of smoothed estimates at each unique year value
  # with standard error    
  B <- predict.gam(gam_mod,  newDF, type="response", se.fit=TRUE)
  # lines(scale(newDF$S_driver),scale(B$fit),pch=16,lwd=2)
  X0 <- predict(gam_mod, newDF, type = 'lpmatrix')
  newDFeps_p <- newDF + eps
  X1 <- predict(gam_mod, newDFeps_p, type = 'lpmatrix')
  X1sim<-simulate(gam_mod,nsim=200,newdata=newDFeps_p)
  # finite difference approximation of first derivative
  # the design matrix
  Xp <- (X0 - X1) / eps
  
  # first derivative
  fd_d1 <- -1*Xp %*% coef(gam_mod)
  D1_dat<-data.frame(TempC=newDF$TempC,y=fd_d1)[order(newDF$TempC),]
  # plot(D1_dat[,1],D1_dat[,2],type="l",lwd=2, xlim=c(-3,3),ylim=c(-2,2))
  
  # second derivative
  newDFeps_m <- newDF - eps
  X_1 <- predict(gam_mod, newDFeps_m, type = 'lpmatrix')
  # design matrix for second derivative
  Xpp <- (X1 + X_1 - 2*X0)  / eps^2
  # second derivative
  fd_d2 <- Xpp %*% coef(gam_mod)
  
  D2_dat<-data.frame(TempC=newDF$TempC,y=fd_d2)[order(newDF$TempC),]
  # plot(D2_dat[,1],D2_dat[,2],type="l",lwd=2, xlim=c(-3,3),ylim=c(-2,2))
  return(list(newDF=newDF,fd_d1=as.numeric(fd_d1),fd_d2=as.numeric(fd_d2)))
}




#-------------------------------------