#' plot cumulative years above threshold:
plot_figS6<-function(){
  dev.new(height=3.25*1.3,width=4.5*1.3)
  head(TempC_019_CENaivecf_0_5_3_mc[1,1,,])
  above         <-  TempC_019_CENaivecf_0_5_3_mc[1,1,,]>2.16
  tt1           <-  apply(above,2,cumlyr,sumyr=5)
  tt            <-  apply(tt1,2,cumsum)/length(above[,1])
  yrs2          <-  as.numeric(rownames(above))
  findthrsh     <-  function(x,thrsh=.25,yrsIN=yrs2){
    yrsIN[as.numeric(x)>thrsh][1]
  }
  
  yrt           <-  apply(tt,2,findthrsh)
  plot(yrs2,tt)
  yrt[rcp85NoBio_n-1]
  yrt[rcp45_n-1]
  nscen         <-  length(c(rcp45_n,rcp85NoBio_n))
  plot(yrs2,tt[,2],ylim=c(0,nscen*1.2),xlim=c(1965,2100),type="l",col=NA,axes=F,ylab="",xlab="")
  axis(1)
  firstY<-rep(2017,nscen);names(firstY)<-colnames(tt1)[c(rcp45_n,rcp85NoBio_n)-1]
  
  for(i in 1:nscen){
    cc              <-  (c(rcp45_n,rcp85NoBio_n)-1)[i]
    ll              <-  NA*tt1[,cc]
    ll[tt1[,cc]]    <-  i
    suby            <-  yrs2[tt1[,cc]]
    if(any(suby>2017))
      firstY[i]    <-  suby[suby>2017][1]
    points(yrs2,ll,pch=16,cex=1.5,col=makeTransparent((wes(6))[i],alpha=250))
  }
  
  text(rep(1980,nscen),1:nscen,colnames(tt1)[c(rcp45_n,rcp85NoBio_n)-1],cex=.8,col=(wes(6))[1:nscen])
  mean(firstY[1:3],na.rm=T)
  mean(firstY[4:6],na.rm=T)
}