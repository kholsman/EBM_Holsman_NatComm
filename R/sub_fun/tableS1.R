tableS1 <- function(){
  
  tmp12       <- risk12
  tmp13       <- risk13
  tmp12$hcr   <- "No cap"
  tmp13$hcr   <- "2 MT cap"
  tabledat    <- rbind(tmp12,tmp13)
  
  
  tbl1    <- tabledat%>%
    arrange(rcp, .by_group = TRUE)%>%
    group_by(rcp,sp,YrBin)%>%
    filter(hcr=="No cap")%>%
    arrange(sp, .by_group = TRUE)%>% group_by(rcp,sp,YrBin)%>%
    arrange(YrBin, .by_group = TRUE)%>% group_by(rcp,sp,YrBin)%>%
    arrange(type, .by_group = TRUE)%>% group_by(rcp,sp,YrBin)%>%
    select(rcp,sp,YrBin,type,NoCapRC = riskC,NoCapRCmn = riskCmn,NoCapRCsd = riskCsd,hcr)
   
  tbl2    <- tabledat%>%
    arrange(rcp, .by_group = TRUE)%>%
    group_by(rcp,sp,YrBin)%>%
    filter(hcr=="2 MT cap")%>%
    arrange(sp, .by_group = TRUE)%>% group_by(rcp,sp,YrBin)%>%
    arrange(YrBin, .by_group = TRUE)%>% group_by(rcp,sp,YrBin)%>%
    arrange(type, .by_group = TRUE)%>% group_by(rcp,sp,YrBin)%>%
    select(rcp,sp,YrBin,type,CapRC = riskC,CapRCmn = riskCmn,CapRCsd = riskCsd,hcr)
  
  tbl <- merge(tbl1,tbl2, by=c("rcp","sp","YrBin","type"))
  tbl <- tbl%>%mutate(deltaRC   = CapRC   - NoCapRC ,
                      deltaRCmn = CapRCmn - NoCapRCmn,
                      type2     = substr(type,1,3))
  tbl$delaRC   <- (round(tbl$deltaRC,1))
  tbl$NoCapRC  <- paste0(as.character(round(tbl$NoCapRC,1))," (",as.character(round(tbl$NoCapRCsd,1)),")")
  tbl$CapRC    <- paste0(as.character(round(tbl$CapRC,1))," (",as.character(round(tbl$CapRCsd,1)),")")
  
  tbl <- tbl%>%
    group_by(sp,YrBin,rcp,type2,NoCapRC,CapRC,deltaRC)%>%
    select(sp,YrBin,rcp,type2,NoCapRC,CapRC,deltaRC)
  
  tbl_dat_long <- reshape2::melt(data.frame(tbl), id=c("sp", "YrBin","rcp","type2"))
  
  first_k      <- function(x,char=T) if(char) as.character(x[1]) else x[1]
  tbl_dat      <- reshape2::dcast(tbl_dat_long, rcp+sp + YrBin ~ type2+variable, first_k, margins="treatment")
  
  out <- 
    kable(tbl_dat[,-(1:2)], caption = "Table 1") %>%
    kable_styling(full_width = F,font=10) %>%
    #collapse_rows(columns = 1:2, valign = "top")%>%
    pack_rows("a) RCP 4.5", 1, 12) %>%
    pack_rows("b) RCP 8.5", 13, 24) %>%
    pack_rows("walleye pollock", 1, 4) %>%
    pack_rows("Pacific cod", 5, 8) %>%
    pack_rows("arrowtooth flounder", 9, 12)%>%
    pack_rows("walleye pollock", 13, 16) %>%
    pack_rows("Pacific cod", 17, 20) %>%
    pack_rows("arrowtooth flounder", 21, 24)
  
  return(out) 
}