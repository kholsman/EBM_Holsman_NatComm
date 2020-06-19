#' 
#' preview.R
#'
preview <- function(datIN = dat2, var = "ABC_total_biom"){
    unique(datIN$hModev2)
    datIN$legend<-""
    for(i in 1:dim(run_def)[1]){
      tt <- grepl(run_def[i,]$`Run title`,datIN$hModev2,fixed=T)
      if(any(tt)){
        datIN[which(tt),]$legend <- run_def[i,]$Legend
      }
    }
    datIN$Scenario <- as.factor(Scenarios[datIN$Scenario])
    datIN$spp      <- factor(names(sppINFO)[datIN$species],levels=names(sppINFO))
    
    if(length(unique(datIN$MC_n))!=1){
      eval(parse(text=paste0("ggplot(data=datIN) +geom_line(aes(x=future_year,y=",var,
          ",color=factor(MC_n),linetype=Scenario),alpha=.4) +
        facet_grid(spp~.,scales='free_y')+
                             theme_minimal()+ theme(legend.position='none')")))
      
    }else{
      eval(parse(text=paste0("ggplot(data=datIN) +
        geom_line(aes(x=future_year,y=",var,",color=Scenario)) +
        facet_grid(spp~.,scales='free_y')+
        theme_minimal()")))
    }
    
}