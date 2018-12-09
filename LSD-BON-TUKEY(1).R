
## LSD
resmat<-NULL
for (i in 1:3){
    for (j in (i+1):4){
    se.d<-sqrt(se[i]^2+se[j]^2)
    m.d<-a[i]-a[j]
    lwd<-m.d-tc*se.d
    upd<-m.d+tc*se.d
    result<-c(i, j, m.d, lwd, upd)
    resmat<-rbind(resmat, result)
  }
}


## Bon
r<-4
nm<-r*(r-1)/2
tcb<-tc<-qt(1-alpha/(2*nm), df)


resmat.b<-NULL
for (i in 1:3){
  for (j in (i+1):4){
    se.d<-sqrt(se[i]^2+se[j]^2)
    m.d<-a[i]-a[j]
    lwd<-m.d-tcb*se.d
    upd<-m.d+tcb*se.d
    result<-c(i, j, m.d, lwd, upd)
    resmat.b<-rbind(resmat.b, result)
  }
}


### Tukey


tct<-qtukey(1-alpha/2, 4, df)



resmat.t<-NULL
for (i in 1:3){
  for (j in (i+1):4){
    se.d<-sqrt(se[i]^2+se[j]^2)
    m.d<-a[i]-a[j]
    lwd<-m.d-tct*se.d
    upd<-m.d+tct*se.d
    result<-c(i, j, m.d, lwd, upd)
    resmat.t<-rbind(resmat.t, result)
  }
}



#### contrast
#install.packages('lsmeans')
library(lsmeans)
cereal.aov<-aov(cases~pkgdes, data=cereal)
levels(cereal$pkgdes)

cereal.aov.lsmeans<-lsmeans(cereal.aov, "pkgdes")
contrast1<-list(T4vsRest=c(-.33, -.33, -.34, 1),
                T1vsT2=c(1, -1, 0, 0))
contrast(cereal.aov.lsmeans, contrast1)



### diagnosis
> resid<-residuals(cereal.aov)
> plot(resid)
> abline(h=0)
> plot(cereal$pkgdes, resid)
> plot(as.numeric(cereal$pkgdes), resid)
> qqnorm(resid)
> qqline(resid)
> plot(as.numeric(cereal$pkgdes), resid)


### constant variance test

bartlett.test(cases~pkgdes, data=cereal)

library(car)
leveneTest(cases~pkgdes, data=cereal)



#### Two way ANOVA


bread<-read.table('http://www.stat.purdue.edu/~boli/stat512/datasets/CH19TA07.DAT', header=F, as.is=T)
names(bread)<-c("sales", "height", "width", "id")

bread$height<-as.factor(bread$height)
bread$width<-as.factor(bread$width)

bread.aov<-aov(sales~height+width+(height*width), data=bread)
summary(bread.aov)
aggregate(bread$sales, by=list(bread$height), 'mean')
aggregate(bread$sales, by=list(bread$height), 'sd')
aggregate(bread$sales, by=list(bread$width), 'mean')
aggregate(bread$sales, by=list(bread$width), 'sd')
aggregate(bread$sales, by=list(bread$height, bread$width), 'sd')
aggregate(bread$sales, by=list(bread$height, bread$width), 'mean')

muhat<-mean(bread$sales)
aggregate(bread$sales, by=list(bread$height), 'mean')->heightmean
alpha<-heightmean[,2]-muhat
aggregate(bread$sales, by=list(bread$width), 'mean')->widthmean
beta<-widthmean[,2]-muhat
aggregate(bread$sales, by=list(bread$height, bread$width), 'mean')->cellmean
celldev<-cellmean[,3]-muhat

rowmean<-heightmean[,2]
colmean<-widthmean[,2]
abmean<-cellmean[,3]

ab<-abmean-rep(rowmean, 2)-rep(colmean, rep(3, 2))+muhat
summary(bread.aov)
bread.aov.m<-aov(sales~height+width, data=bread)
summary(bread.aov.m)
plot(bread$height, bread$sales)
plot(as.numeric(bread$height), bread$sales)
lines(c(1, 2, 3), rowmean, col=2)
plot(as.numeric(bread$width), bread$sales)
lines(c(1, 2), colmean, col=2)


#### Random effect models
interview<-read.table('http://www.stat.purdue.edu/~boli/stat512/datasets/CH24TA01.DAT', header=F, as.is=T)
names(interview)<-c('rating', 'officer', 'id')
head(interview)
as.factor(interview$officer)->interview$officer
as.factor(interview$id)->interview$id

library(lattice)
xyplot(rating~officer, data=interview)

library(lme4)
interview.lme<-lmer(rating~1+(1 | officer), data=interview)
summary(interview.lme)


#### two way random effect model
efficiency<-read.table('http://www.stat.purdue.edu/~boli/stat512/datasets/CH24PR15.DAT', header=F, as.is=T)
names(efficiency)<-c("mpg", "driver", "car", "id")
head(efficiency)

dc<-as.numeric(efficiency$driver)*10+as.numeric(efficiency$car)
xyplot(efficiency$mpg~dc)

efficiency$driver<-as.factor(efficiency$driver)
efficiency$car<-as.factor(efficiency$car)

efficiency.aov<-aov(mpg~driver*car, data=efficiency)
anova(efficiency.aov)
library('lme4')
efficiency.lmer<-lmer(mpg~1+(1 | driver)+(1 | car)+(1 | driver:car), data=efficiency)
summary(efficiency.lmer)

aggregate(fitted(efficiency.lmer), list(dc), 'mean')
aggregate(efficiency$mpg, list(dc), 'mean')
mean(efficiency$mpg)


### mixed effect model
service<-read.table('http://www.stat.purdue.edu/~boli/stat512/datasets/CH19PR16.DAT', header=F, as.is=T)
names(service)<-c('time', 'tech', 'make', 'k')
head(service)
service$tech<-as.factor(service$tech)
service$make<-as.factor(service$make)

mt<-as.numeric(service$make)*10+as.numeric(service$tech)

service.aov<-aov(time~make*tech, data=service)
anova(service.aov)
## MSB is not always larger than MSE from fixed effect model
service.lmer<-lmer(time~1+make+(1 | tech)+(1 | make:tech), data=service)
summary(service.lmer)
