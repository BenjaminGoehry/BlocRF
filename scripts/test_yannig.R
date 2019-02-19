library(rangerts)
library(randomForest)
library(magrittr)
library(yarrr)


rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

mape<-function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=10))
}


###############Import data
#setwd("/Users/yannig/Documents/Enseignement/2017-2018/M2statML/TP/")
#C:\Enseignement\2015-2016\Projet Data Mining\TP\Regression
data0<-read.table("data/data_conso_hebdo0.txt", header=TRUE)
data1<-read.table("data/data_conso_hebdo1.txt", header=TRUE)

data <- rbind(data0, data1)

# N <- floor(nrow(data)/2)
# data0 <- data[1:N,]
# data1 <- data[(N+1):(N+52),]
# 

eq <- Load ~ Load1+ NumWeek + Temp + IPI +Temp1+Time +IPI_CVS+Year+Time
rf0 <- randomForest(eq, ntree=500,data=data0, importance=TRUE)
rf0.fitted <- predict(rf0,newdata=data0)
rf0.forecast <- predict(rf0,newdata=data1)
mape(data0$Load,rf0.fitted)
mape(data1$Load,rf0.forecast)

plot(data1$Load, type='l')
lines(rf0.forecast, col='red')


data0$Time2 <- data0$Time^2
data0$TempTime <- data0$Time*data0$Temp
data0$TempChauf <- pmin(0,data0$Temp-15)
data0$TempChaufTime <- pmin(0,data0$Temp-15)*data0$Time

noel = which(abs(data0$Day - 24) <= 3 & data0$Month == 12)
consoNoel = vector("numeric", length(data0$Time))
consoNoel[noel] = 1
data0$consoNoel <- consoNoel
data0$MonthF <- as.factor(data0$Month)

data1$Time2 <- data1$Time^2
data1$TempTime <- data1$Time*data1$Temp
data1$TempChauf <- pmin(0,data1$Temp-15)
data1$TempChaufTime <- pmin(0,data1$Temp-15)*data1$Time
noel = which(abs(data1$Day - 24) <= 3 & data1$Month == 12)
consoNoel = vector("numeric", length(data1$Time))
consoNoel[noel] = 1
data1$consoNoel <- consoNoel
data1$MonthF <- as.factor(data1$Month)


#eq <- Load ~ Load1+ NumWeek + Temp + IPI +Temp1+Time +IPI_CVS+Year+Time+Time2+TempTime
eq <- Load ~ Load1+ NumWeek + Temp + IPI +Temp1+Time +IPI_CVS+Year+Time+Time2+TempTime+TempChauf+TempChaufTime+consoNoel+MonthF
rf0<-ranger(eq, data=data0, sample.fraction=1, mtry =3, num.trees = 1000, min.node.size=1, importance='permutation', activate.ts=FALSE, replace=T)
ranger0.fitted <- predict(rf0, data=data0)
ranger0.forecast <- predict(rf0, data=data1)
mape(data0$Load,ranger0.fitted$predictions)
mape(data1$Load,ranger0.forecast$predictions)

eps <- data0$Load-ranger0.fitted$predictions

plot(data0$Time, eps, type='l')
plot(data0$Load1, eps, pch=20)
plot(data0$NumWeek, eps, pch=20)




rf1<-ranger(eq, data=data0, sample.fraction=1, mtry =3, num.trees = 1000, min.node.size=1, importance='permutation',
            block.size = 52, bootstrap.ts = "nonoverlapping", replace=T, activate.ts=T)

ranger1.fitted <- predict(rf1, data=data0)
ranger1.forecast <- predict(rf1, data=data1)
mape(data0$Load,ranger1.fitted$predictions)
mape(data1$Load,ranger1.forecast$predictions)


plot(data1$Load, type='l')
lines(ranger0.forecast$predictions, col='blue')
lines(ranger1.forecast$predictions, col='red')



###########

rgIID <- function(eq)
{
  rf0<-ranger(eq, data=data0, sample.fraction=1, mtry =3, num.trees = 1000, min.node.size=1, activate.ts=FALSE
              , replace=T)
  ranger0.fitted <- predict(rf0, data=data0)
  ranger0.forecast <- predict(rf0, data=data1)
  
  res <- list()
  res$rmse.fit <-rmse(data0$Load-ranger0.fitted$predictions) 
  res$rmse.for <-rmse(data1$Load-ranger0.forecast$predictions) 
  res$forecast <- ranger0.forecast$predictions
  return(res)
}

rgDEP <- function(eq, bootstrap.ts="nonoverlapping", block.size=10)
{
  rf1<-ranger(eq, data=data0, sample.fraction=1, mtry =3, num.trees = 1000, min.node.size=1,
              block.size = block.size, bootstrap.ts = bootstrap.ts, replace=T, activate.ts=T)
  
  ranger1.fitted <- predict(rf1, data=data0)
  ranger1.forecast <- predict(rf1, data=data1)
  
  res <- list()
  res$rmse.fit <-rmse(data0$Load-ranger1.fitted$predictions) 
  res$rmse.for <-rmse(data1$Load-ranger1.forecast$predictions) 
  res$forecast <- ranger1.forecast$predictions
  return(res)
}



Nsim <- 100
#eq <- Load ~ Load1+ NumWeek + Temp + IPI +Temp1+Time +IPI_CVS+Year+Time+I(Time^2) +I(Temp*Time)
#eq <- Load ~ Load1+ NumWeek + Temp + IPI +Temp1+Time +IPI_CVS+Year+Time+Time2+TempTime
eq <- Load ~ Load1+ NumWeek + Temp + IPI +Temp1+Time +IPI_CVS+Year+Time+Time2+TempTime+TempChauf+TempChaufTime+consoNoel+MonthF

resIID <- lapply(rep(list(eq), Nsim), rgIID)
resDEP<- lapply(rep(list(eq), Nsim), rgDEP, block.size=52)
resDEP_mb<- lapply(rep(list(eq), Nsim), rgDEP, block.size=52,bootstrap.ts="moving")
resDEP_stationary<- lapply(rep(list(eq), Nsim), rgDEP, block.size=52,bootstrap.ts="stationary")



rmse.fit.iid <- lapply(resIID, function(x){x$rmse.fit})%>%unlist
rmse.for.iid <-lapply(resIID, function(x){x$rmse.for})%>%unlist
#for.iid <- lapply(resIID, function(x){x$forecqst})%>%unlist%>%matrix(,byrow=F, ncol=Nsim)
for.iid <- lapply(resIID, function(x){x$forecast})%>%unlist%>%matrix(,byrow=F, ncol=Nsim)

rmse.fit.dep <- lapply(resDEP, function(x){x$rmse.fit})%>%unlist
rmse.for.dep <-lapply(resDEP, function(x){x$rmse.for})%>%unlist
for.dep <- lapply(resDEP, function(x){x$forecast})%>%unlist%>%matrix(,byrow=F, ncol=Nsim)

rmse.fit.dep_mb <- lapply(resDEP_mb, function(x){x$rmse.fit})%>%unlist
rmse.for.dep_mb<-lapply(resDEP_mb, function(x){x$rmse.for})%>%unlist
for.dep_mb <- lapply(resDEP_mb, function(x){x$forecast})%>%unlist%>%matrix(,byrow=F, ncol=Nsim)

rmse.fit.dep_stationary <- lapply(resDEP_stationary, function(x){x$rmse.fit})%>%unlist
rmse.for.dep_stationary<-lapply(resDEP_stationary, function(x){x$rmse.for})%>%unlist
for.dep_stationary <- lapply(resDEP_stationary, function(x){x$forecast})%>%unlist%>%matrix(,byrow=F, ncol=Nsim)




boxplot(rmse.for.iid/rmse.for.dep)
abline(h=1)
median(rmse.for.iid/rmse.for.dep)



boxplot(cbind(rmse.for.iid,rmse.for.dep,rmse.for.dep_mb,rmse.for.dep_stationary), col=c("royalblue2","orangered2","salmon","green"))

plot(data1$Load, type='l')
matlines(for.dep, col="orangered2")
matlines(for.iid, col="royalblue2")
matlines(for.dep_mb, col="salmon")
matlines(for.dep_stationary, col="green")





















