d=read.table("abc.dat",header=T)
 
 plot(d$yr,d$wpa,ylim=c(0,2.6e6))
?ts:
  names(d)
plot(d$wpc[1:32]-d$wpc[2:33])
# compute differences (change) in catch from one year to next
pcc1diff= (d$pcc[1:32]-d$pcc[2:33])
wpc1diff= (d$wpc[1:32]-d$wpc[2:33])
# compute differences (% change) in catch from one year to next
pcc1diffp= (d$pcc[1:32]-d$pcc[2:33])/mean(d$pcc)
wpc1diffp= (d$wpc[1:32]-d$wpc[2:33])/mean(d$wpc)
?hist
hist((wpc1diff),col="lightblue",breaks=12,xlab="Change in catch (t)",axt=F)
hist((pcc1diff),col="lightblue",breaks=12,xlab="Change in catch (t)",xlim=c(-4e5,4e5))
hist((wpc1diffp),col="lightblue",breaks=12,xlab="Change in catch (t)",xlim=c(-.4,.4),main="Pollock")
hist((pcc1diffp),col="lightblue",breaks=12,xlab="Change in catch (t)",xlim=c(-.4,.4),main="Pacific cod")
# combined difference (for fitting)
Tc1diffp=as.vector(rbind(wpc1diffp,pcc1diffp))
hist((Tc1diffp),col="lightblue",breaks=12,xlab="Change in catch (t)",xlim=c(-.4,.4),main="Pollock and Pacific cod")
lines(density(Tc1diffp),lwd=2,col=2,lty=2)
# Notes: need to come up w/ a catch given ABC and last year's catch
# change in negative direction slightly more extreme than positive
summary((Tc1diffp))
names(d)
summary(wpc1diffp[wpc1diffp>=0])
summary(pcc1diffp[pcc1diffp>=0])
summary(Tc1diffp[Tc1diffp>=0])
summary(wpc1diffp[wpc1diffp<=0])
summary(pcc1diffp[pcc1diffp<=0])
summary(tc_neg)

wp_pos=(wpc1diffp[wpc1diffp>=0])
pc_pos=(pcc1diffp[pcc1diffp>=0])
tc_pos=(Tc1diffp[Tc1diffp>=0])
wp_neg=(wpc1diffp[wpc1diffp<=0])
pc_neg=(pcc1diffp[pcc1diffp<=0])
tc_neg=(Tc1diffp[Tc1diffp<=0])
cbind(pc_pos,wp_pos)
length(wp_pos)
require(lattice)
boxplot(cbind(wp_pos,pc_pos,tc_pos,wp_neg,pc_neg,tc_neg))
abline(h=0)
abline(h=-.1,lty=2)

hist(Tc1diffp[Tc1diffp<=0],breaks=8,col="lightblue",main="Decreases in catch",xlab="Change relative to previous year")
hist(Tc1diffp[Tc1diffp>=0],breaks=8,col="lightblue",main="Increases in catch",xlab="Change relative to previous year")

    (density(wpc1diff),ylim=c(0,1.5e-5),lwd=2,col=1)
lines(density(pcc1diff),lwd=2,col=2,lty=2)

plot(p(pcc1diffp),lwd=2,col=1)
plot(density(wpc1diffp),lwd=2,col=1)
lines(density(pcc1diffp),lwd=2,col=2,lty=2)

plot(wpc1diff,pcc1diff)
summary(wpc1diff)
plot(density(wpc1diff))
plot(density(pcc1diff))

hist(density(wpc1diff))

names(d)

lm1=lm(pcc~pca+wpa,data=d)
 summary(lm(pcc~s(pca)+s(pct)+s(wpa),data=d))
pairs(d[,2:7],pch=19,cex=1.2)
library(mgcv)
g1=(gam(pcc~s(pca)+s(wpt),data=d))
summary(g1)

summary(lm1)
plot(g1)

