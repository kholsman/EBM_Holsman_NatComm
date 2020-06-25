#' plot cumulative years above threshold:
plot_figS6<-function(
  datIN   =   dat_2_5_12_mc,
  spIN    =   1,
  Hin     =   "H12_219_CENaivecf",
  plotSet = list("RCP 4.5" = c(rcp45_n),"RCP 8.5" = c(rcp85NoBio_n)),
  thrsh   = 2.2,
  thrsh_pIN = 0.25,
  coll    = coll_use[-1],
  var     = "bottomT_C"){
  
 eval(parse(text = paste0("sub <- datIN%>%filter(species==spIN,hModev2==Hin, MC_n==1,age==6,Scenario%in%unlist(plotSet))%>%
                           mutate(Year = start_yr + future_year-1,above = FALSE,val=",var,")")))
  sub$above[sub$val>thrsh] <- TRUE
  sub$scenario  <- factor(Scenarios[sub$Scenario],levels=Scenarios[unlist(plotSet)])
  
  sub_m         <-  reshape2::acast(sub%>%select(Year,scenario,val), Year~scenario, mean,na.rm=T)
  ny            <-  dim(sub_m)[1]
  nscn          <-  dim(sub_m)[2]
  above         <-  sub_m>thrsh
  tt1           <-  apply( above,2,cumlyr,sumyr=5)
  tt            <-  apply(tt1,2,cumsum)/length(above[,1])
  yrs2          <-  as.numeric(rownames(above))
  findthrsh     <-  function(x,thrsh_p=thrsh_pIN,yrsIN=yrs2){
    yrsIN[as.numeric(x)>thrsh_p][1]
  }
  
  yrt           <-  apply(tt,2,findthrsh,thrsh_p=thrsh_pIN)
  
  nscen         <-  length(c(rcp45_n,rcp85NoBio_n))
  plot(yrs2,tt[,2],ylim=c(0,nscen*1.2),xlim=c(1965,2100),type="l",col=NA,axes=F,ylab="",xlab="")
  firstY        <- rep(2017,nscen)
  names(firstY) <- colnames(tt1)
  
  for(i in 1:nscen){
    cc              <-  i
    ll              <-  NA*tt1[,i]
    ll[tt1[,i]]     <-  i
    suby            <-  yrs2[tt1[,i]]
    if(any(suby>2017))
      firstY[i]    <-  suby[suby>2017][1]
    points(yrs2,ll,pch=16,cex=1.5,col=makeTransparent(coll[i],alpha=250))
  }
  
  text(rep(1980,nscen),1:nscen,colnames(tt1),cex=.8,col=coll[1:nscen])
  axis(1)
  mean(firstY[1:3],na.rm=T)
  mean(firstY[4:6],na.rm=T)
}
