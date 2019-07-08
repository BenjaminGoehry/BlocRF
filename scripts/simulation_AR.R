rm(list=objects())
library(xgboost)
library(mgcv)
library(forecast)
library(RColorBrewer)
library(magrittr)
library(opera)
library(yarrr)
library(R39Toolbox)
library(rangerts)
library(pdp)
library(mlbench)

source("R/mlbench.friedman1_cor.R")
source("R/fit_forest.R")





plot(t, y)
#plot(delta_t, y)
#a_t <- 2*t
Nsim <- 100
n <- 200
n0 <- 190
t <- c(1:n)
sd <- 1/2
a <- 1/10
w <- 2*pi/20
delta <- w/10
cos1_t <- cos(w*t)
cos2_t <- cos(2*w*t)
cos3_t <- cos(3*w*t)
f <- cos1_t+1/2*cos2_t-1/3*cos3_t
#f <- t
model <- list(ar=0.5)

plot(t,f ,type='l')

eps <- arima.sim(n = n, model=model, sd = sd)
y <- f+eps

plot(t, y, type='l')
lines(t, f, col='red')


param <- list(num.trees=100,  mtry=1)
#param$formula <- y~cos1_t+cos2_t+cos3_t
param$formula <- y~t
param$sample.fraction <- 1

block.size <- c(1, seq(5,40, by=5))

mse_fit_iid_mat <- NULL
mse_fit_moving_mat <- NULL
mse_fit_circular_mat <- NULL
mse_fit_nov_mat <- NULL

mse_forecast_iid_mat <- NULL
mse_forecast_moving_mat <- NULL
mse_forecast_circular_mat <- NULL
mse_forecast_nov_mat <- NULL

mse_forecast_iid_ar_mat <- NULL
mse_forecast_moving_ar_mat <- NULL
mse_forecast_circular_ar_mat <- NULL
mse_forecast_nov_ar_mat <- NULL



for(i in c(1:Nsim))
{
  # eps <- rnorm(n, 0, sd)
  # y <- a*t+eps
  
  eps <- arima.sim(n = n, model=model, sd = sd)
  y <- f+eps
  #y_lag1 <- c(y[1], y[1:(n-1)])
  
  DataSim <- data.frame(y, t, cos1_t, cos2_t, cos3_t, f=f)
  
  #DataSim <- data.frame(y, t, f=a*t)
  DataSim0 <- DataSim[1:n0,]
  DataSim1 <- DataSim[(n0+1):n,]
  param$data <- DataSim0
  
  res_iid <- lapply(block.size , fit_forest_AR,  bootstrap.ts="circular", activate.ts=F)
  res_moving <- lapply(block.size , fit_forest_AR,  bootstrap.ts="moving", activate.ts=T)
  res_circular <- lapply(block.size , fit_forest_AR,  bootstrap.ts="circular", activate.ts=T)
  res_nov <- lapply(block.size , fit_forest_AR,  bootstrap.ts="nonoverlapping", activate.ts=T)
  
  
  mse_fit_iid <- lapply(res_iid, function(x){x$mse_fit})%>%unlist
  mse_fit_moving <- lapply(res_moving, function(x){x$mse_fit})%>%unlist
  mse_fit_circular <- lapply(res_circular, function(x){x$mse_fit})%>%unlist
  mse_fit_nov <- lapply(res_nov, function(x){x$mse_fit})%>%unlist
  
  mse_forecast_iid <- lapply(res_iid, function(x){x$mse_forecast})%>%unlist
  mse_forecast_moving <- lapply(res_moving, function(x){x$mse_forecast})%>%unlist
  mse_forecast_circular <- lapply(res_circular, function(x){x$mse_forecast})%>%unlist
  mse_forecast_nov <- lapply(res_nov, function(x){x$mse_forecast})%>%unlist
  
  mse_forecast_iid_ar <- lapply(res_iid, function(x){x$mse_rf_forecast_ar})%>%unlist
  mse_forecast_moving_ar <- lapply(res_moving, function(x){x$mse_rf_forecast_ar})%>%unlist
  mse_forecast_circular_ar <- lapply(res_circular, function(x){x$mse_rf_forecast_ar})%>%unlist
  mse_forecast_nov_ar <- lapply(res_nov, function(x){x$mse_rf_forecast_ar})%>%unlist
  
  mse_fit_iid_mat <- cbind(mse_fit_iid_mat, mse_fit_iid)
  mse_fit_moving_mat <- cbind(mse_fit_moving_mat, mse_fit_moving)
  mse_fit_circular_mat <- cbind(mse_fit_circular_mat, mse_fit_circular)
  mse_fit_nov_mat <- cbind(mse_fit_nov_mat, mse_fit_nov)
  
  mse_forecast_iid_mat <- cbind(mse_forecast_iid_mat, mse_forecast_iid)
  mse_forecast_moving_mat <- cbind(mse_forecast_moving_mat, mse_forecast_moving)
  mse_forecast_circular_mat <- cbind(mse_forecast_circular_mat, mse_forecast_circular)
  mse_forecast_nov_mat <- cbind(mse_forecast_nov_mat, mse_forecast_nov)
  
  mse_forecast_iid_ar_mat <- cbind(mse_forecast_iid_ar_mat, mse_forecast_iid_ar)
  mse_forecast_moving_ar_mat <- cbind(mse_forecast_moving_ar_mat, mse_forecast_moving_ar)
  mse_forecast_circular_ar_mat <- cbind(mse_forecast_circular_ar_mat, mse_forecast_circular_ar)
  mse_forecast_nov_ar_mat <- cbind(mse_forecast_nov_ar_mat, mse_forecast_nov_ar)
}

##############################################################################################################################
mse_fit_iid_MB <- mse_fit_iid_mat%>%rowMeans
mse_fit_moving_MB <- mse_fit_moving_mat%>%rowMeans
mse_fit_circular_MB <- mse_fit_circular_mat%>%rowMeans
mse_fit_nov_MB <- mse_fit_nov_mat%>%rowMeans

col <- piratepal("basel")
plot(block.size, mse_fit_iid_MB, type='b', pch=20, ylim=range(mse_fit_iid_MB, mse_fit_moving_MB, mse_fit_circular_MB, mse_fit_nov_MB))
lines(block.size, mse_fit_moving_MB, type='b', pch=20, col=col[1])
lines(block.size, mse_fit_circular_MB, type='b', pch=20, col=col[2])
lines(block.size, mse_fit_nov_MB, type='b', pch=20, col=col[3])
legend('topright', col=c('black', col[1:3]), c('iid', 'moving block', 'circular','non-overlapping'), bty='n', lty=1)

##############################################################################################################################
mse_forecast_iid_MB <- mse_forecast_iid_mat%>%rowMeans
mse_forecast_moving_MB <- mse_forecast_moving_mat%>%rowMeans
mse_forecast_circular_MB <- mse_forecast_circular_mat%>%rowMeans
mse_forecast_nov_MB <- mse_forecast_nov_mat%>%rowMeans

col <- piratepal("basel")
plot(block.size, mse_forecast_iid_MB, type='b', pch=20, ylim=range(mse_forecast_iid_MB, mse_forecast_moving_MB, mse_forecast_circular_MB, mse_forecast_nov_MB))
lines(block.size, mse_forecast_moving_MB, type='b', pch=20, col=col[1])
lines(block.size, mse_forecast_circular_MB, type='b', pch=20, col=col[2])
lines(block.size, mse_forecast_nov_MB, type='b', pch=20, col=col[3])
legend('topright', col=c('black', col[1:3]), c('iid', 'moving block', 'circular','non-overlapping'), bty='n', lty=1)

#####################################################################################################################################
mse_forecast_iid_ar_MB <- mse_forecast_iid_ar_mat%>%rowMeans
mse_forecast_moving_ar_MB <- mse_forecast_moving_ar_mat%>%rowMeans
mse_forecast_circular_ar_MB <- mse_forecast_circular_ar_mat%>%rowMeans
mse_forecast_nov_ar_MB <- mse_forecast_nov_ar_mat%>%rowMeans

col <- piratepal("basel")
plot(block.size, mse_forecast_iid_ar_MB, type='b', pch=20, ylim=range(mse_forecast_iid_ar_MB, mse_forecast_moving_ar_MB, mse_forecast_circular_ar_MB, mse_forecast_nov_ar_MB))
lines(block.size, mse_forecast_moving_ar_MB, type='b', pch=20, col=col[1])
lines(block.size, mse_forecast_circular_ar_MB, type='b', pch=20, col=col[2])
lines(block.size, mse_forecast_nov_ar_MB, type='b', pch=20, col=col[3])
legend('topright', col=c('black', col[1:3]), c('iid', 'moving block', 'circular','non-overlapping'), bty='n', lty=1)



res_iid[[1]]$ar_corr


res_circular[[1]]$ar_corr

ar_iid <- lapply(res_iid, function(x){x$ar_corr$coeff[1]})%>%unlist
ar_moving <-lapply(res_moving, function(x){x$ar_corr$coeff[1]})%>%unlist

ar_iid/ar_moving

plot(ar_moving, type='b', pch=20)
lines(ar_iid, type='b', pch=20)

plot(DataSim0$t, DataSim0$y, pch=20, col='grey')
lines(DataSim0$t,res_moving[[3]]$rf_fit$predictions, type='l')
lines(DataSim0$t, DataSim0$f, col='red')


plot(DataSim1$t, DataSim1$y, pch=20, col='grey')
lines(DataSim1$t,res_moving[[3]]$rf_fit.forecast, type='l')
lines(DataSim1$t,res_moving[[3]]$rf_fit_ar.forecast, col='green')
lines(DataSim1$t, DataSim1$f, col='red')


###########################################################################################
#####iid
#set.seed(seed=100)
param$activate.ts <- F
rf_iid <- do.call(ranger, args=param, quote = FALSE, envir = parent.frame())
rf_iid.forecast <- predict(rf_iid, data=DataSim1)$predictions

#####moving block boostrap  
param$block.size <- 10
param$bootstrap.ts <- 'moving'
param$activate.ts <- T
rf_moving <- do.call(ranger, args=param, quote = FALSE, envir = parent.frame())
rf_moving.forecast <- predict(rf_moving, data=DataSim1)$predictions

col <- piratepal("basel")
plot(DataSim0$t, DataSim0$y, pch=20, col='grey')
lines(DataSim0$t, DataSim0$f, col='red')
lines(DataSim0$t, rf_iid$predictions, col='black')
lines(DataSim0$t, rf_moving$predictions, col=col[1])


plot(DataSim1$t, DataSim1$y, pch=20, col='grey', ylim=range(DataSim1$y, DataSim1$f))
lines(DataSim1$t, DataSim1$f, col='red')
lines(DataSim1$t, rf_iid.forecast, col='black')
lines(DataSim1$t, rf_moving.forecast, col=col[1])




plot(DataSim$cos_t,type='l')



#res_moving <- lapply(block.size , fit_forest2,  bootstrap.ts="moving", activate.ts=T)
res_moving <- lapply(block.size , fit_forest2,  bootstrap.ts="circular", activate.ts=T)


mse_fit_moving <- lapply(res_moving, function(x){x$mse_fit})%>%unlist
plot(block.size, mse_fit_moving, type='b', pch=20, ylim=range(mse_fit_moving, mse_fit_iid))
abline(h=mse_fit_iid, lty='dotted')
which.min(mse_fit_moving)

param$block.size <- block.size[which.min(mse_fit_moving)]
param$bootstrap.ts <- 'circular'
param$activate.ts <- T
rf_moving <- do.call(ranger, args=param, quote = FALSE, envir = parent.frame())
rf_moving.forecast <- predict(rf_fit, data=DataSim1)$predictions





col <- piratepal("basel")
plot(DataSim0$t, DataSim0$y, pch=20, col='grey')
lines(DataSim0$t, DataSim0$t*a, col='red')
lines(DataSim0$t, rf_iid$predictions, col='black')
lines(DataSim0$t, rf_moving$predictions, col=col[1])





plot(DataSimt$t, DataSim1$y, pch=20, col='grey')
lines(DataSim1$t, DataSim1$t*a, col='red')
lines(DataSim1$t, rf_iid.forecast, col='black')
lines(DataSim1$t, rf_fit.forecast, col=col[1])








#####circular moving block boostrap  
#set.seed(seed=100)
mse_rf_cir <- lapply(block.size , fit_forest,  bootstrap.ts="circular", activate.ts=T)%>%unlist

#####non-overlapping
#set.seed(seed=100)
mse_rf_nov <- lapply(block.size , fit_forest,  bootstrap.ts="nonoverlapping", activate.ts=T)%>%unlist

col <- piratepal("basel")

plot(DataSim1$t, DataSim1$y, pch=20, col='grey')
lines(DataSim1$t, DataSim1$t*a, col='red')
lines(DataSim1$t, rf_iid.forecast, col='black')
lines(DataSim1$t, rf_fit.forecast, col=col[1])




param$block.size <- 10
param$bootstrap.ts <- 'circular'
param$activate.ts <- T
rf_fit <- do.call(ranger, args=param, quote = FALSE, envir = parent.frame())
rf_fit.forecast <- predict(rf_fit, data=DataSim1)$predictions




plot(block.size, mse_rf_mb, type='l', ylim=range(mse_rf_iid, mse_rf_mb, mse_rf_cir, mse_rf_nov), col=col[1])
lines(block.size, mse_rf_cir, col=col[2])
lines(block.size, mse_rf_nov, col=col[3])
abline(h=mse_rf_iid)
legend('topright', col=c('black', col[1:3]), c('iid', 'moving block', 'circular','non-overlapping'), bty='n', lty=1)

##########################################################################################
##########################################################################################
##########################################################################################



















