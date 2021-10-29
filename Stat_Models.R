
#Reading the CSV into R 
library(readr)
wines=read.csv2(file="winequality-red.csv", head=TRUE, sep=";", dec=".")
str(wines)

#Box Plots
boxplot(wines$fixed.acidity,main="Fixed Acidity")
boxplot(wines$volatile.acidity,main="Volatile Acidity")
boxplot(wines$citric.acid,main="Citric Acid ")
boxplot(wines$residual.sugar,main="Residual Sugar")
boxplot(wines$chlorides,main="Chlorides ")
boxplot(wines$free.sulfur.dioxide,main="Free Sulfur Dioxide")
boxplot(wines$total.sulfur.dioxide,main="Total Sulfur Dioxide")
boxplot(wines$density,main="Density ")
boxplot(wines$pH,main="pH")
boxplot(wines$sulphates,main="Sulphates")
boxplot(wines$alcohol,main="Alcohol")
boxplot(wines$quality,main="Quality")

#Summary of Data
summary(wines)
library(Hmisc)
describe(wines)
library(psych)
describe(wines)

#Calculation of confidence intervals and proportions

CI<-function(a){
 LE= mean(a)-2*sd(a)
 print('Lower Limit')
 print(LE)
 UE=mean(a)+2*sd(a)
 print("Upper Limit")
 print(UE)
 p=sum((a>LE & a<UE)==TRUE)/1599
 print("Proportion between two standard deviations interval")
 print(p)
 
 }

for(i in 1:ncol(wines)){
        print(names(wines[i]))
        CI(wines[[i]])   
 
}
        

#Histogram
hist(wines$fixed.acidity,main="Fixed Acidity",freq = FALSE)
lines(density(wines$fixed.acidity), lwd=5, col='blue')

hist(wines$volatile.acidity,main="Volatile Acidity",freq = FALSE)
lines(density(wines$volatile.acidity), lwd=5, col='blue')

hist(wines$citric.acid,main="Citric Acid",freq = FALSE)
lines(density(wines$citric.acid), lwd=5, col='blue')

hist(wines$residual.sugar,main="Residual Sugar",freq = FALSE)
lines(density(wines$residual.sugar), lwd=5, col='blue')

hist(wines$chlorides,main="Chlorides",freq = FALSE)
lines(density(wines$chlorides), lwd=5, col='blue')

hist(wines$free.sulfur.dioxide,main="Free Sulfur Dioxide",freq = FALSE)
lines(density(wines$free.sulfur.dioxide), lwd=5, col='blue')

hist(wines$total.sulfur.dioxide,main="Total Sulfur Dioxide",freq = FALSE)
lines(density(wines$total.sulfur.dioxide), lwd=5, col='blue')

hist(wines$density,main="Density",freq = FALSE)
lines(density(wines$density), lwd=5, col='blue')


hist(wines$pH,main="pH",freq = FALSE)
lines(density(wines$pH), lwd=5, col='blue')

hist(wines$sulphates,main="Sulphates",freq = FALSE)
lines(density(wines$sulphates), lwd=5, col='blue')

hist(wines$alcohol,main="Alcohol",freq = FALSE)
lines(density(wines$alcohol), lwd=5, col='blue')

hist(wines$quality,main="Quality",freq = FALSE)

#Part 

X=mean(wines$density)

sd=sd(wines$density)/sqrt(length(wines$density))
sd

confidence_interval<-function(a,b){
        UL= a+(2*b)
        LL=a-(2*b)
        print (UL)
        print (LL)
                                                         
}
confidence_interval(X,sd)

rm=mean(wines$residual.sugar)
rs=sd(wines$residual.sugar)/sqrt(length(wines$residual.sugar))

confidence_interval(rm,rs)
 
mu.hat.set<-NULL 
for(k in 1:2000){
        sample.bootstrap<-sample(wines$density, size=1599,
                                 replace=T)
        mu.hat<-mean(sample.bootstrap)
        mu.hat.set[k]<-mu.hat
}

mu.hat
sd(mu.hat.set)

wines$density_new<- wines$density*10

hist(mu.hat.set,freq = FALSE)
lines(density(mu.hat.set), lwd=5, col='blue')
quantile(mu.hat.set, probs=c(0.025,0.975))

install.packages("stats4")
library(stats4)

minuslog.lik<-function(mu, sigma){
        log.lik<-0
        for(i in 1:1599){
                log.lik<-log.lik+log(dnorm(wines$density_new[i], mean=mu,
                                           sd=sigma))
        }
        return(-log.lik)
}
est <- mle(minuslog=minuslog.lik,
           start=list(mu=mean(wines$density_new),
                      sigma=sd(wines$density_new)))
summary(est)

mu.hat.set<-NULL 
for(k in 1:2000){
        sample.bootstrap<-sample(wines$residual.sugar, size=1599,
                                 replace=T)
        mu.hat<-mean(sample.bootstrap)
        mu.hat.set[k]<-mu.hat
}

mu.hat
sd(mu.hat.set)

hist(mu.hat.set,freq = FALSE)
lines(density(mu.hat.set), lwd=5, col='blue')
quantile(mu.hat.set, probs=c(0.025,0.975))

wines$log_residual_sugar<-log10(wines$residual.sugar)

hist(wines$log_residual_sugar,main=" Log Residual Sugar",freq = FALSE)
lines(density(wines$log_residual_sugar), lwd=5, col='blue')

minuslog.lik<-function(mu, sigma){
        log.lik<-0
        for(i in 1:1599){
                log.lik = log.lik + log(dlnorm(x=wines$residual.sugar[i], meanlog = mu, sdlog = sigma))
        }
        return(-log.lik)
}
est.lognorm<-mle(minuslog=minuslog.lik, start=list(mu=log(mean(wines$residual.sugar)),
                                                   sigma=log(sd(wines$residual.sugar))))
summary(est.lognorm)

wines$quality<-as.numeric(wines$quality)
summary(wines)
wines$ratings<- ifelse(wines$quality>=7,1,0) 
mean(wines$ratings)
qm=mean(wines$ratings)
qs=sd(wines$ratings)/sqrt(length(wines$ratings))
confidence_interval(qm,qs)

mu.hat.set<-NULL 
for(k in 1:2000){
        sample.bootstrap<-sample(wines$ratings, size=1599,
                                 replace=T)
        mu.hat<-mean(sample.bootstrap)
        mu.hat.set[k]<-mu.hat
}

hist(mu.hat.set,freq = FALSE)
lines(density(mu.hat.set), lwd=5, col='blue')
quantile(mu.hat.set, probs=c(0.025,0.975))

minuslog.lik<-function(mu, sigma){
        log.lik<-0
        for(i in 1:1599){
                log.lik<-log.lik+log(dnorm(wines$ratings[i], mean=mu,
                                           sd=sigma))
        }
        return(-log.lik)
}
est <- mle(minuslog=minuslog.lik,
           start=list(mu=mean(wines$ratings),
                      sigma=sd(wines$ratings)))
summary(est)


