# All raw data ####
rm(list=ls())
library(reshape2)
library(ggplot2)
library(blme)
library(gtools)

# the csv is extracted 
# http://www.fangraphs.com/leaders.aspx?pos=all&stats=fld&lg=all&qual=0&type=3&season=2013&month=0&season1=2012&ind=0&team=0&rost=0&age=0&filter=&players=0
# I additionaly added the columns Def and UZR/150
dat=read.csv("InsideEdgeFielding2012-2013.csv",stringsAsFactors=FALSE)
names(dat)<-c("Name","Team","Pos","Inn","n0","n1","y1","n2","y2",
              "n3","y3","n4","y4","n5","y5","playerid","Def","UZR.150")
dat[is.na(dat)]=0
for (col in c(7,9,11,13,15)) {
  prop=as.numeric(substring(dat[,col],1,nchar(dat[,col])-1))
  prop[is.na(prop)] <- 0
  prop=prop/100
  dat[,col]=round(prop*dat[,col-1],0)
}
dat$Successes=rowSums(dat[,c(7,9,11,13,15)])
dat$Failures=rowSums(dat[,c(7,9,11,13,15)-1])-dat$Successes
dat$Def=dat$Def/dat$Inn*150 # convert to yearly stat

datall=dat

# get plays per inning
with(subset(datall,Inn>100),aggregate((Successes+Failures)/Inn,by=list(Pos),FUN=median))
# ggplot(datall,aes(x=(Successes+Failures)/Inn,colour=Pos))+stat_ecdf()

# plot simple ####
dat=subset(datall,Pos=="SS")
dat=subset(dat,Successes>0 & Failures>0)

dat.m1=melt(dat[,c(1,6,8,10,12,14)],id=c(1))
dat.m2=melt(dat[,c(1,7,9,11,13,15)],id=1)
dat.m1$Bucket=substring(dat.m1$variable,2)
dat.m2$Bucket=substring(dat.m2$variable,2)
dat.m=merge(dat.m1[,-2],dat.m2[,-2],by=c("Name","Bucket"))
names(dat.m)[3:4]=c("n","y")
dat.m=subset(dat.m,n>0)

model.simple=glm(y/n~Bucket,family=binomial(link=logit),data=dat.m,weights=n)
coefs=coefficients(model.simple)

qplot(-20:75/10,gtools::inv.logit(-20:75/10+coefs[1]),geom="line")+
  geom_segment(aes(x=c(0,coefs[-1]),xend=c(0,coefs[-1]),y=0,yend=gtools::inv.logit(c(0,coefs[-1])+coefs[1])),lty=2)+
  geom_segment(aes(x=-2,xend=c(0,coefs[-1]),y=gtools::inv.logit(c(0,coefs[-1])+coefs[1]),yend=gtools::inv.logit(c(0,coefs[-1])+coefs[1])),lty=2)+
  labs(x=expression(theta),y="Pr(Success)")+
  annotate(geom="text",x=coefs[-1],y=0,label=paste0("gamma[",1:4,"]"),parse=T)+
  annotate(geom="text",x=-2,y=gtools::inv.logit(c(0,coefs[-1])+coefs[1]),label=c("Remote","Unlikely","About Even","Likely","Almost Certain"),vjust=-.3,hjust=0)
ggsave("LogitOrdered.png")

# loop through all positions and save output
# https://stackoverflow.com/questions/9460664/weighted-pearsons-correlation
weighted.cor <- function( x, y, w = rep(1,length(x))) {
  stopifnot(length(x) == dim(y)[2] )
  w <- w / sum(w)
  # Center x and y, using the weighted means
  x <- x - sum(x * w)
  y <- y - sum(y * w)
  # Compute the variance
  vx <- sum(w * x * x)
  vy <- sum(w * y * y)
  # Compute the covariance
  vxy <- sum(y * x * w)
  # Compute the correlation
  vxy / sqrt(vx * vy)
}

positions=c("P","C","1B","2B","3B","SS","LF","CF","RF")
cors=numeric(length(positions))
for (pos in positions) {
  cat(pos,"\n")
  dat=subset(datall,Pos==pos)
  dat=subset(dat,Successes>0 & Failures>0)
  # colSums(dat[,c(7,9,11,13,15)])/colSums(dat[,c(7,9,11,13,15)-1])
  
  dat.m1=melt(dat[,c(1,6,8,10,12,14)],id=c(1))
  dat.m2=melt(dat[,c(1,7,9,11,13,15)],id=1)
  dat.m1$Bucket=substring(dat.m1$variable,2)
  dat.m2$Bucket=substring(dat.m2$variable,2)
  dat.m=merge(dat.m1[,-2],dat.m2[,-2],by=c("Name","Bucket"))
  names(dat.m)[3:4]=c("n","y")
  dat.m=subset(dat.m,n>0)
  
  
  model=bglmer(y/n~1+Bucket+(1 | Name),family=binomial,data=dat.m,weights=n,nAGQ=25)
  # summary(model)
  # dotplot(ranef(model, which = "Name", condVar = TRUE))
  
  coef.table=data.frame(Name=sort(unique(dat.m$Name)),
                        Effect=ranef(model, which = "Name", condVar = TRUE)$Name[[1]],
                        SE=sqrt(as.numeric(attr(ranef(model, condVar = TRUE)$Name,"postVar"))))
  coef.table$Lower=with(coef.table,Effect-1.96*SE)
  coef.table$Upper=with(coef.table,Effect+1.96*SE)
  coef.table=merge(coef.table,dat[,c(1,4)],by="Name")
  coef.table=coef.table[order(coef.table$Effect),]
  
  png(paste0(pos,"list.png"))
  par(mar=c(4, 4, 1, 2) + 0.1)
  with(subset(coef.table,Inn>=ifelse(pos=="P",350,800)),{
    dotchart(Effect,labels=Name,xlim=c(min(Lower),max(Upper)),xlab=expression(theta))
    segments(Lower, 1:length(Lower),Upper, 1:length(Upper))
    abline(v=0,lty=2)
  })
  dev.off()
  
  categ.weights=aggregate(dat.m$n,list(dat.m$Bucket),sum)[,2]
  categ.weights=categ.weights/sum(categ.weights)
  
  avg.prob=sum(gtools::inv.logit(c(0,summary(model)$coefficients[-1,1])+summary(model)$coefficients[1,1])*
                 categ.weights)
  
  mean.probs=gtools::inv.logit(outer(coef.table$Effect,rep(1,5))+outer(rep(1,nrow(coef.table)),c(0,summary(model)$coefficients[-1,1])+summary(model)$coefficients[1,1]))
  mean.probs=mean.probs %*% categ.weights
  
  upper.probs=gtools::inv.logit(outer(coef.table$Upper,rep(1,5))+outer(rep(1,nrow(coef.table)),c(0,summary(model)$coefficients[-1,1])+summary(model)$coefficients[1,1]))
  upper.probs=upper.probs %*% categ.weights
  
  lower.probs=gtools::inv.logit(outer(coef.table$Lower,rep(1,5))+outer(rep(1,nrow(coef.table)),c(0,summary(model)$coefficients[-1,1])+summary(model)$coefficients[1,1]))
  lower.probs=lower.probs %*% categ.weights
  
  coef.table=cbind(coef.table,PlayProbDiff=mean.probs-avg.prob,
                   PlayProbDiffLower=lower.probs-avg.prob,
                   PlayProbDiffUpper=upper.probs-avg.prob)
  
  merged.dat=merge(coef.table,dat[,c(1:4,17)])
  cors[pos==positions]=weighted.cor(merged.dat$Effect,merged.dat$UZR.150,merged.dat$Inn)
  
  if (which(pos==positions)==1) {
    full.dat=merged.dat
  } else {
    full.dat=rbind(full.dat,merged.dat)
  }
  if(nrow(dat)!=nrow(merged.dat)) {
    print("nrows don't equal")
  }
}
positions.fac=factor(positions,levels=positions)
qplot(positions.fac[-(1:2)],cors[-(1:2)],ylab="Weighted Correlation with UZR/150",geom="bar",xlab="Position",stat="identity")
ggsave("../../Blog/octopress/source/images/InsideEdge/CorrelationWithUZR.png")

