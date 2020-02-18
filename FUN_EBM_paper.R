###########################################################
## plots FUNCTION for EBM paper
## Kirstin Holsman 
## June 2018
###########################################################
  col1<-colorRampPalette(colors()[c(280,320)])
  col2<-colorRampPalette(colors()[c(70,491)])
  col2<-colorRampPalette(colors()[c(114,491)])
  col3<-colorRampPalette(c("yellow","red"))
  #col4<-colorRampPalette(c(colIN1(6),"maroon"))
  
  plt<-c("Zissou1","Darjeeling1","Darjeeling2","FantasticFox1")
  wes<-colorRampPalette(c(wes_palette(n=5, name=plt[1])[1:5]))
  wes(6)
  col2<-colorRampPalette(c(wes(7)[c(3,1)],col2(3)[3]))
  col3<-colorRampPalette(c(wes(7)[4:7]))


if(!require(extrafont)){ install.packages(extrafont)}else{library(extrafont)}
theme_kir_EBM <- function(...) {
    theme_kir(base_family="Helvetica",
              plot_title_family="Helvetica-Bold",
              subtitle_family="Helvetica",
              panel_face="bold",
              caption_family="Helvetica",
              plot_title_just ="r",
              axis_title_size = 12,
              ...)
}
theme_kir_EBM <- function(...) {
    theme_kir(base_family="ArialNarrow",
              plot_title_family="ArialNarrow-Bold",
              subtitle_family="ArialNarrow",
              panel_face="bold",
              caption_family="ArialNarrow",
              plot_title_just ="r",
              axis_title_size = 12,
              ...)
}
  #-------------------------------------
  #  makeTransparent
  #-------------------------------------
    # This function makes any color transparent
    makeTransparent<-function(someColor, alpha=100)
    {
      newColor<-col2rgb(someColor)
      apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                                  blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
    }
  
  #-------------------------------------
  #  getDat
  #-------------------------------------
  # This function extracts any value from the dat_2_5_x files
    getDat<-function(dat,scn=1,sp=1,age=1,val="B"){
      subDat<-dat[dat$Scenario==scn&dat$age==age&dat$species==sp,]
      xx<-subDat[,"future_year"]
      yy<-subDat[,val]
      return(yy)
    }
  
  #-------------------------------------
  #  grabDat
  #-------------------------------------
  # This function extracts any value from the dat_2_5_x files 
    grabDat<-function(datIn=dat_2_5_3,valIn="B",age=1){
      outtmp<-list()
      for(spIn in 1:3){
        tmp<-data.frame(matrix(0,nYrsTot,length(simnames)+1))
        colnames(tmp)<-c("Year",simnames)
        tmp[,1]<-Years
        for(ss in 1:length(simnames))
          tmp[,ss+1]<-getDat(dat=datIn,scn=ss,val=valIn,sp=spIn,age=age)
        outtmp[[spIn]]<-tmp
      }
      return(outtmp)
    }
    


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
  # threshold
  #-------------------------------------
  #' Get threshold for gam relationship based on Large et al. 2014
  #'
  #' @param dat is a data.frame with t=date and dat=data to plot
  #' @param x is the predictor variable of the gam fitted (newdata)
  #' @param Catch is the y variable (data)
  #' @param TempIn is the y variable (data)
  #' @param subyr is the subset years for the GAM
  #' @param simul_set is the subset of simulations to evaluate
  #' @param adj1 is the first adjustment parm
  #' @param rndN is the sign digits for the first 
  #' @param rndN2 is the sign digits for the second
  #' @param s is species number
  #' @param boot_n 
  #' @param boot_nobs 
  #' @param probIN probabilties for the quantile function
  #' @param knottIN number of knots for the GAM
  #' @keywords Temperature, plot, data, ACLIM
  #' @export 
  #' @examples 

# plot(datt$t,datt$MIROC_rcp85,col="red",type="l")
#   lines(datt$t,datt$MIROC_rcp45,col="red",type="l")
#   lines(as.numeric(names(TempC_019_CENaivecf_0_5_12_mc[1,1,,"MIROC_rcp85"])),TempC_019_CENaivecf_0_5_12_mc[1,1,,"MIROC_rcp85"],type=
#     "l")
#   lines(as.numeric(names(TempC_019_CENaivecf_0_5_12_mc[1,1,,"MIROC_rcp45"])),TempC_019_CENaivecf_0_5_12_mc[1,1,,"MIROC_rcp45"],type=
#     "l")
getBtarget<-function(fldrIN,nm="B0_set"){
      
      tt<-scan(file=fldrIN,what=character(),sep="\n")
      tmp<-tt[grep(nm,tt)+1]
      tmp<-as.numeric(strsplit(tmp,split=" ")[[1]])
      return(tmp)
}


threshold<-function(
  x=seq(-3,10,.1),
  hind=NULL,
  Catch=C_019_CENaivecf_0_5_12_mc,
  TempIn=TempC_019_CENaivecf_0_5_12_mc,
  subyr=fut_yrs,
  simul_set=c(6,9,11),
  adj1=adj,
  rndN=6,
  rndN2=6,
  method=1,
  s=1,
  smooth_yr=1,
  boot_n=1000,
  boot_nobs=24600,#500,
  probIN=c(.025,.5,.975),
  sdmult=1.95,
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
    deltaC<-TempC_raw<-TempC<-NA
    if(!is.null(hind[1])){
      refTS <- C_019_CENaivecf_0_5_12_mc[s,,,1]*0+ mean(C_019_CENaivecf_0_5_12_mc[s,1,subyr,1])
      deltaC<-c(deltaC, as.vector( (Catch[s,hind,subyr,1]-refTS[hind,subyr])/refTS[hind,subyr] ))
      tmpC<-TempIn[s,hind,subyr,1]
      tmpC2<-tmpC2smooth<-tmpC*NA
      tmpC2<-as.numeric(tmpC)
      tmpC2smooth<-as.numeric(ma2(x=as.numeric(tmpC2),n=smooth_yr))
      TempC<-c(TempC,as.vector(tmpC2smooth))
      TempC_raw<-c(TempC_raw,as.vector(TempIn[s,1,subyr,1]))
    }else{
      for(simul in simul_set){
        deltaC<-c(deltaC, as.vector((Catch[s,,subyr,simul]-Catch[s,,subyr,1])/Catch[s,,subyr,1]))
        tmpC<-TempIn[s,,subyr,simul]
        tmpC2<-tmpC2smooth<-tmpC*NA
        for(ii in 1:dim(tmpC)[1]){
          tmpC2[ii,]<-as.numeric(tmpC[ii,])
          tmpC2smooth[ii,]<-as.numeric(ma2(x=as.numeric(tmpC2[ii,]),n=smooth_yr))
        }
        TempC<-c(TempC,as.vector(tmpC2smooth))
        TempC_raw<-c(TempC_raw,as.vector(TempIn[s,,subyr,simul]))
      }
    }
     
    deltaC<-deltaC[-1]
    TempC<-TempC[-1]
    TempC_raw<-TempC_raw[-1]
    
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

      # get bootstraped sub-sample
      nobs<-length(dd$num)
      if(boot_nobs>nobs) boot_nobs<-nobs
      bootd<-sample_n(dd,boot_nobs,replace = TRUE)
      tmpgam<-gam(deltaC~s(TempC,k=knottIN,bs="tp"),data=bootd)
      tmpd<-deriv2(tmpgam,simdat=x)

      gmlist[[int]]<-tmpgam
      Deriv1[int,]<-tmpd$fd_d1
      Deriv2[int,]<-tmpd$fd_d2
      hatFit[int,]<-predict(tmpgam,se.fit=TRUE,newdata=data.frame(TempC=x))$fit
      hatse[int,]<-predict(tmpgam,se.fit=TRUE,newdata=data.frame(TempC=x))$se
    }
    # apply quantiles to bootstrap replicates
    D1_se  <-apply(Deriv1,2,quantile,probs=probIN)
    D2_se  <-apply(Deriv2,2,quantile,probs=probIN)
    qnt    <-apply(hatFit,2,quantile,probs=probIN)
    qntse  <-apply(hatse,2,quantile,probs=probIN)
    
    # first to the gam using 1-3 methods
    nobs<-length(x)
    if(method==1)
      hat_qnt<-data.frame(tmp=x,
                        up=hat$fit+qnt[3,]-qnt[2,],
                        mn=hat$fit,
                        dwn=hat$fit+qnt[1,]-qnt[2,])
    if(method==2)
     hat_qnt<-data.frame(tmp=x,
                        up=hat$fit+sdmult*qntse[2,],
                        mn=hat$fit,
                        dwn=hat$fit-sdmult*qntse[2,])

    if(method==3)
    hat_qnt<-data.frame(tmp=x,
                        up=hat$fit+sdmult*hat$se,
                        mn=hat$fit,
                        dwn=hat$fit-sdmult*hat$se)
    hat_qnt$smoothed_mn<- predict(loess(mn ~ tmp, data=hat_qnt, span=0.25)) 
    hat_qnt$smoothed_dwn<- predict(loess(dwn ~ tmp, data=hat_qnt, span=0.25)) 
    hat_qnt$smoothed_up<- predict(loess(up ~ tmp, data=hat_qnt, span=0.25)) 
    # first derivative quantiles
    df1_qnt<-data.frame(tmp=x,
                        up=D1_se[3,],
                        mn=D1_se[2,],
                        dwn=D1_se[1,])

    # second derivative quantiles
    df2_qnt<-data.frame(tmp=x,
                        up=D2_se[3,],
                        mn=D2_se[2,],
                        dwn=D2_se[1,])
    
    # get difference in signs
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

    require(rootSolve)
    # determine peaks and valleys:
    # 25% smoothing span
    df1_qnt$smoothed_mn<- predict(loess(mn ~ tmp, data=df1_qnt, span=0.25)) 
    df1_qnt$smoothed_dwn<- predict(loess(dwn ~ tmp, data=df1_qnt, span=0.25)) 
    df1_qnt$smoothed_up<- predict(loess(up ~ tmp, data=df1_qnt, span=0.25)) 

    pks1<-sort(c(findPeaks(df1_qnt$smoothed_mn),findPeaks(-df1_qnt$smoothed_mn)))
    signif1<-which(!between(0, df1_qnt$dwn, df1_qnt$up, incbounds=TRUE))
    thrsh1<-intersect(which(!between(0, df1_qnt$dwn, df1_qnt$up, incbounds=TRUE)),pks1)
    df1_qnt$tmp[thrsh1]

    # 25% smoothing span
    df2_qnt$smoothed_mn<- predict(loess(mn ~ tmp, data=df2_qnt, span=0.25)) 
    df2_qnt$smoothed_dwn<- predict(loess(dwn ~ tmp, data=df2_qnt, span=0.25)) 
    df2_qnt$smoothed_up<- predict(loess(up ~ tmp, data=df2_qnt, span=0.25)) 
    pks2<-sort(c(findPeaks(df2_qnt$smoothed_mn),findPeaks(-df2_qnt$smoothed_mn)))
    pks2_up<-sort(c(findPeaks(df2_qnt$smoothed_up),findPeaks(-df2_qnt$smoothed_up)))
    pks2_dwn<-sort(c(findPeaks(df2_qnt$smoothed_dwn),findPeaks(-df2_qnt$smoothed_dwn)))
    
    signif2<-which(!between(0, df2_qnt$dwn, df2_qnt$up, incbounds=TRUE))

   
    thrsh2<-intersect(signif2,pks2)
    #  thrsh2_up<-intersect(signif2,pks2_up)
    #   thrsh2_dwn<-intersect(signif2,pks2_dwn)
    df2_qnt$tmp[thrsh2]

    return(list(deltaC=deltaC,TempC=TempC,TempC_raw=TempC,
                hat=hat_qnt,fdif1=df1_qnt,
                fdif2=df2_qnt,
                signif1=signif1,
                signif2=signif2,
                ix_pks=pks2,
                thrsh_max1=thrsh2,
                thrsh_x=df2_qnt$tmp[thrsh2],
                thrsh_y=df2_qnt$mn[thrsh2],
                #thrsh_x=tmpall13_1$hat$tmp[thrsh2],
                #thrsh_y=tmpall13_1$hat$mn[thrsh2],
                GAM=GAM))
}
  
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
  
    
  # 
  # Read in datat
  # 
  read_INdat<-function(dat_path=dat_path,fl="",SaveIt=TRUE,coluse=c("orange",col1(6)),ltyy1=1,alpha1=50,plot.path=plot_file){
    dat<-read.csv(file.path(dat_path,"results/Future_report.rep"),sep=" ")
    snames<-c("Pollock","P. cod","arrowtooth")
    
    tmp<-dat
    scenarios<-unique(tmp$fut_simulation)
    # print(scenarios)
    ltyyuse<-rep(ltyy1,max(scenarios))
    # dat<-tmp
    tmpDat<-list()
    tmpDat$Alldat<-dat
    Fc<-(which(names(dat)=="F_rate")+2):(which(names(dat)=="objective_fun")-1)
    Bc<-(which(names(dat)=="SSB.SSB0")+2):(grep("catch.biomass",names(dat))-1)
    Rc<-(grep("recruits.numbers",names(dat))+2):dim(dat)[2]
    Cc<-(grep("catch.biomass",names(dat))+2):(grep("recruits.numbers",names(dat))-1)
    lab<-dat[,which(names(dat)=="SSB.SSB0")]
    fr<-tmpDat$fr<-which(lab=="SSB")
    fr0<-tmpDat$fr0<-which(lab=="SSB0")
    tmpDat$dat0<-dat0<-dat[fr0,]
    tmpDat$dat<-dat<-dat[fr,]
    tmpDat$rec<-dat[,Rc]
    tmpDat$Frate<-dat[,Fc]
    tmpDat$SSB<-dat[,Bc]
    tmpDat$SSB0<-dat0[,Bc]
    tmpDat$catch<-dat[,Cc]
    ltyy<-ltyyuse
    tmpDat$species<-dat0$species
    tmpDat$sim<-dat0$fut_simulation
    tmpDat$itr<-dim(dat0[dat0$fut_simulation==scenarios[1]&dat0$species==1,Rc])[1]
    tmpDat$year<-as.numeric(substr(names(dat[,Rc]),2,5))
    return(tmpDat)
  }
  
  
  
  calcRisk<-function(delta=grabDat(datIn=dat_019_CENaivecf_0_5_12,
    valIn="Catch_total_biom")[[sp]],limm=lim[l],
    Yrbin=c(2017,2025,2050,2075,2100),esmlist=list(rcp45_n,rcp85NoBio_n),mn=FALSE){
    #delta<-tmpC<-C_2_5_12[[sp]]
    nbin<-length(Yrbin) 
    nlist<-length(esmlist)
    risk<-data.frame(matrix(NA,nlist,nbin-1))
    colnames(risk)<-paste(paste0("(",Yrbin[-nbin]),paste0(Yrbin[-1],"]"),sep=",")
    rownames(risk)<-1:nlist
    rownames(risk)<-c("rcp45","rcp85")
    tmpC<-delta
    for(esm in 1:nlist){
      cc<-esmlist[[esm]]
      for(y in 1:(nbin-1)){
        rr<-which(tmpC$Year>Yrbin[y]&tmpC$Year<=Yrbin[y+1] )
        delta[,-1]<- round((tmpC[,-1]- tmpC$persistence)/ tmpC$persistence,5)*100
        risk[esm,y]<-100*round(length(which(unlist(delta[rr,cc])<=(limm)))/length(unlist(delta[rr,cc])),4)
        if(mn) risk[esm,y]<-round(mean(unlist(delta[rr,cc])),2)
        #risk[esm,y]<-100*round(length(which(unlist(delta[rr,cc])<=(limm)))/length(unlist(delta[rr,cc])),4)
      }
    }
    return(risk)
  }

inv.logit<-function(x){exp(x)/(1+exp(x))}
logit<-function(x){log(x/(1-x))}

calcRisk_MCMC<-function(delta=dat_019_CENaivecf_0_5_12_mc[sp,,,],limm=-10,logit=F,
    Yrbin=c(2017,2025,2050,2075,2100),esmlist=list(rcp45_n,rcp85NoBio_n)){
    #delta<-tmpC<-C_2_5_12[[sp]]
    nbin<-length(Yrbin) 
    nitr<-dim(delta)[1]
    Years<-as.numeric(rownames(delta[1,,]))
    nlist<-length(esmlist)
    mnrisk<-matrix(NA,nlist,nbin-1)

    risk<-array(NA,c(nlist,nitr,nbin-1))
    dimnames(risk)<-list(c("rcp45","rcp85"),paste("itr",1:nitr),
      paste(paste0("(",Yrbin[-nbin]),paste0(Yrbin[-1],"]"),sep=","))
    
    colnames(mnrisk)<-paste(paste0("(",Yrbin[-nbin]),paste0(Yrbin[-1],"]"),sep=",")
    rownames(mnrisk)<-1:nlist
    rownames(mnrisk)<-c("rcp45","rcp85")
    sdrisk<-mnrisk
    
    for(esm in 1:nlist){
      cc<-esmlist[[esm]]
      for(ii in 1:nitr){
        tmpC<-delta[ii,,]
        for(y in 1:(nbin-1)){
          rr<-which(Years>Yrbin[y]&Years<=Yrbin[y+1] )
          tmpp<- round((tmpC[rr,cc]- tmpC[rr,1])/ tmpC[rr,1],5)*100
          risk[esm,ii,y]<-100*round(length(which(unlist(tmpp)<=(limm)))/length(unlist(tmpp)),4)
        }  #risk[esm,y]<-100*round(length(which(unlist(delta[rr,cc])<=(limm)))/length(unlist(delta[rr,cc])),4)
      }
      mnrisk[esm,]<-(apply((risk[esm,,]),2,mean,na.rm=T))
      sdrisk[esm,]<-(apply((risk[esm,,]),2,sd,na.rm=T))
      if(logit) mnrisk[esm,]<-logit(apply(inv.logit(risk[esm,,]),2,mean,na.rm=T))
      if(logit) sdrisk[esm,]<-logit(apply(inv.logit(risk[esm,,]),2,sd,na.rm=T))
    }
    return(list(mcmcRisk=risk,mnRisk=mnrisk,sdRisk=sdrisk))
  }
  
  
# plotRisk<-function(figname="Figures/PlotRisk1_SSM.jpg",lim=-10,sp=1,newplot=TRUE
#     ,modeIN="MSM",typeIN=c("10% decline","50% decline"),spIN=1:3,
#     ltyy=c(1,3),ylimm=c(0,100),coll=rep(rev(col2(4)),pchh=c(3,16,3,1),update.figsIN=TRUE){
#           graphics.off()
#           quartz(h=8,w=5)
#           par(mar=c(1,1,1,1)) # margins of graph: (bottom,left, top, right)
#           par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
#           par(oma=c(2,2,1,1))# outer margins of graph: (bottom,left, top, right)
#           par(mfrow=c(length(spIN),2))
#           layout(cbind(c(1,2,3),c(4,5,6)))
#           layout.show()
       
#         for(s in spIN){ 
#           for(i in 1:length(typeIN)){
#             tmp12<-as_tibble(risk12)%>%filter(sp%in%s,mode%in%modeIN,rcp=="rcp45",type%in%typeIN[i])
#             tmp13<-as_tibble(risk13)%>%filter(sp%in%s,mode%in%modeIN,rcp=="rcp45",type%in%typeIN[i])

#             if(i ==1) plot(tmp12$riskC,tmp12$riskB,ylim=ylimm,xlim=ylimm,pch=pchh[(i*2)-1],col=coll)
#             points(tmp12$riskC,tmp12$riskB,ylim=ylimm,xlim=ylimm,pch=pchh[(i*2)-1],col=coll)
#             points(tmp13$riskC,tmp13$riskB,ylim=ylimm,xlim=ylimm,pch=pchh[i*2],col=coll)   

#             arrows(x0=as.numeric(tmp12$riskC),x1=as.numeric(tmp13$riskC),
#               y0=as.numeric(tmp12$riskB),y1=as.numeric(tmp13$riskB),length = 0.1,lwd=3,col=coll,lty=ltyy[i])
#           }
#         }
#          legend("bottomright",
#             col=c(coll,rep("black",2)),
#             lwd=2,
#             pch=c(rep(-1,,pchh),
#             c(tmp12$timeframe,"No cap", "2 MT cap"),box.lty=0)
#           mtext(side=3,font=2,"a) RCP 4.5",outer=F,line=-1.1,adj=.01)
         
#         for(s in spIN){ 
#           for(i in 1:length(typeIN)){
#             tmp12<-as_tibble(risk12)%>%filter(sp%in%s,mode%in%modeIN,rcp=="rcp45",type%in%typeIN[i])
#             tmp13<-as_tibble(risk13)%>%filter(sp%in%s,mode%in%modeIN,rcp=="rcp45",type%in%typeIN[i])

#             if(i ==1) plot(tmp12$riskC,tmp12$riskB,ylim=ylimm,xlim=ylimm,pch=pchh[(i*2)-1],col=coll)
#             points(tmp12$riskC,tmp12$riskB,ylim=ylimm,xlim=ylimm,pch=pchh[(i*2)-1],col=coll)
#             points(tmp13$riskC,tmp13$riskB,ylim=ylimm,xlim=ylimm,pch=pchh[i*2],col=coll)   

#             arrows(x0=as.numeric(tmp12$riskC),x1=as.numeric(tmp13$riskC),
#               y0=as.numeric(tmp12$riskB),y1=as.numeric(tmp13$riskB),length = 0.1,lwd=3,col=coll,lty=ltyy[i])
#           }

#           legend("bottomright",col=c(coll,rep("black",2)),lwd=c(rep(2,length(xrisk1[1,])),-1,-1),pch=c(rep(-1,length(risk1[1,])),3,16),c(colnames(risk1),"No cap", "2 MT cap"),box.lty=0)
#           mtext(side=3,font=2,"a) RCP 4.5",outer=F,line=-1.1,adj=.01)
#         }
          
#           mtext(side=2,font=2,paste("Risk of ",lim,"% Biomass decline"),outer=T,line=1)
#           mtext(side=1,font=2,paste("Risk of ",lim,"% Catch decline"),outer=T,line=1)
#           mtext(side=3,font=2,"b) RCP 8.5",outer=F,line=-1.1,adj=.01)
#           if(update.figsIN) quartz.save(file=figname,type="jpg",dpi=500)  
#     }  


  getMCMC<-function(datin="dat_019_CENaivecf_0_5_12_mc",
                    nspp=3,
                    nitr=20,
                    agein=1,
                    valin='SSB_total_biom'){
    eval(parse(text=paste0("tmp<-grabDat(datIn=",datin,1,",valIn=valin,age=agein)")))
    nrc             <-dim(tmp[[1]][,-1])
    tmpmc           <-array(NA,c(nspp,nitr,nrc[1],nrc[2]))
    s               <-1
    dimnames(tmpmc) <-list(paste0("sp",1:nspp),
                           paste0("itr",1:nitr),
                           tmp[[s]][,1],
                           colnames(tmp[[s]][,-1]))
    rm(tmp)
    for(itr in 1:nitr) {
      eval(parse(text=paste0("tmp<-grabDat(datIn=",datin,itr,",valIn=valin)")))
      for(s in 1:3){
        tmpmc[s,itr,,]<-as.matrix(tmp[[s]][,-1])
      }
    }
    return(tmpmc)
  }
  
  # Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

  
  # threshold_old<-function(x=tmp,hatin=hat_45_12,adj1=adj,nitr=10000,probIN=c(.25,.5,.75)){
  #   nobs<-length(hat)
  #   mchat<-matrix(NA,nitr,nobs)
  #   for(i in 1:nobs)
  #     mchat[,i]<-(rnorm(nitr,mean=hatin$fit[i],sd=hatin$se[i]))
  #   hat<-(hatin$fit)
  #   
  #   # dx 
  #   dy<-(hat[2:nobs]-hat[1:(nobs-1)])
  #   dx<-(x[2:nobs]-x[1:(nobs-1)])
  #   fdif<-dy/dx
  #   mxdx<-matrix(dx,nitr,nobs)
  #   mcdy<-(mchat[,2:nobs]-mchat[,1:(nobs-1)])/(mxdx[,2:nobs])
  #   
  #   #dxdx 
  #   nobs2<-length(fdif)
  #   fdif2<-(fdif[2:nobs2]-fdif[1:(nobs2-1)])/dx[2:nobs2]
  #   mcdydy<-(mcdy[,2:nobs2]-mcdy[,1:(nobs2-1)])/(mxdx[,2:nobs2])
  #   
  #   qnt_hat<-apply(mchat,2,quantile,probs=probIN)
  #   qnt_fdif<-apply(mcdy,2,quantile,probs=probIN)
  #   qnt_fdif2<-apply(mcdydy,2,quantile,probs=probIN)
  #   
  #   signif<-rep(NA,nobs)
  #   aa<- which(qnt_fdif[3,]<0&which(qnt_fdif[1,]<0))
  #   bb<-which(qnt_fdif[3,]>0&which(qnt_fdif[1,]>0))
  #   if(length(aa)>0) signif[aa]<-hat[aa+1]
  #   if(length(bb)>0) signif[bb]<-hat[bb+1]
  #   
  #   sigthresh<-rep(NA,nobs)
  #   aa<- which(qnt_fdif2[3,]<0&which(qnt_fdif2[1,]<0))
  #   bb<-which(qnt_fdif2[3,]>0&which(qnt_fdif2[1,]>0))
  #   if(length(aa)>0) sigthresh[aa]<-hat[aa+1]
  #   if(length(bb)>0) sigthresh[bb]<-hat[bb+1]
  #   
  #   return(list(hat=data.frame(hat=hat,t(qnt_hat)),
  #               fdif1=data.frame(fdif1=fdif,t(qnt_fdif)),
  #               fdif2=data.frame(fdif2=fdif2,t(qnt_fdif2)),
  #               signif=signif,sigthresh=sigthresh))
  # }
  # 