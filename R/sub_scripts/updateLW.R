# updateLW.R

#_____________________________________
#  update LWA regressions:
#_____________________________________

if(update.LWdatat==1)
  createLWA_glms(
    data.path1=data.in,
    data.pathout1=data.out,
    LWDATA="LWA_ALL",
    flname=LWname,
    flsubname=LWsubname)

## Step 1. LW regression # W<-a*L^b
load(file = file.path(data.out,LWname))
load(file = file.path(data.out,LWsubname))

names(LW.glm)    		
spp              <-  names(LW.glm)[names(LW.glm)!="qrydate"]
nn               <-  length(spp)
LW_SmryTable     <-  data.frame(matrix(-999,nn,6))
LW_SmryTable[,1] <-  spp

for(i in 1:nn){
  LW_SmryTable[i,2]  <-  coef(LW.glm[[i+1]])[1]
  LW_SmryTable[i,3]  <-  exp(coef(LW.glm[[i+1]])[2])
  LW_SmryTable[i,4]  <-  summary(LW.glm[[i+1]])[8]
  LW_SmryTable[i,5]  <-  (LW.glm[[i+1]])$df.residual
}

LW_SmryTable[,6]       <-  "W(grams)~a*(L(^b)"
LW_SmryTable[,7]       <-  LW.glm[["qrydate"]]
colnames(LW_SmryTable) <-  c("Species","a","b","adj.r2","df","formula","qrydate")
save(LW_SmryTable,file = file.path(data.out,"LW_SmryTable.Rdata"))

rm(spp)
print("completed LW table")