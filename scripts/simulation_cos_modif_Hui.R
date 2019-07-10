rm(list=objects())
library(RColorBrewer)
library(magrittr)
library(opera)
library(yarrr)
library(R39Toolbox)
library(rangerts)
library(pdp)
library(mlbench)

source("R/mlbench.friedman1_cor.R")
source("R/fit_forest_Hui.R")

Nsim <- 100
n <- 500
n0 <- 250
t <- c(1:n)
sd <- 1/4
a <- 1/10
w <- 2*pi/20
delta <- w/10
cos05_t <- cos(0.5*w*t)
cos1_t <- cos(w*t)

f <- cos05_t + cos1_t  
t_20 <- rep(c(1:20),  length(t)/20)

eps <- rnorm(n, 0, sd)
# y <- f+eps
y <- f

plot(t, y, type='l')
lines(t, f, col='red')



param <- list(num.trees=100,  mtry=1)
param$formula <- y ~ t_20
param$sample.fraction <- 1


# block.size <- c(1, seq(5,40, by=5))
block.size <- c(1, 5, 10, 20, 40, 50, 60, 80, 100)

mse_fit_iid_mat <- NULL
mse_fit_moving_mat <- NULL
mse_fit_circular_mat <- NULL
mse_fit_nov_mat <- NULL

err_fit_oob_iid_mat <- NULL
err_fit_oob_moving_mat <- NULL
err_fit_oob_circular_mat <- NULL
err_fit_oob_nov_mat <- NULL

mse_forecast_iid_mat <- NULL
mse_forecast_moving_mat <- NULL
mse_forecast_circular_mat <- NULL
mse_forecast_nov_mat <- NULL


for(i in c(1:Nsim)) {
  print(i)
  # eps <- rnorm(n, 0, sd)
  # y <- a*t+eps
  eps <- rnorm(n, 0, sd)
  y <- f+eps
  
  DataSim <- data.frame(y, t, cos05_t, cos1_t, f, t_20)
  
  #DataSim <- data.frame(y, t, f=a*t)
  DataSim0 <- DataSim[1:n0,]
  DataSim1 <- DataSim[(n0+1):n,]
  param$data <- DataSim0
  #param$data.pred <- DataSim1
  
  res_iid <- lapply(block.size , fit_forest2,  bootstrap.ts=NULL, seed = i, param=param, data.pred=DataSim1)
  res_moving <- lapply(block.size , fit_forest2,  bootstrap.ts="moving", seed = i, param=param, data.pred=DataSim1)
  res_circular <- lapply(block.size , fit_forest2,  bootstrap.ts="circular", seed = i, param=param, data.pred=DataSim1)
  res_nov <- lapply(block.size , fit_forest2,  bootstrap.ts="nonoverlapping", seed = i, param=param, data.pred=DataSim1)
  
  mse_fit_iid <- lapply(res_iid, function(x){x$mse_fit})%>%unlist
  mse_fit_moving <- lapply(res_moving, function(x){x$mse_fit})%>%unlist
  mse_fit_circular <- lapply(res_circular, function(x){x$mse_fit})%>%unlist
  mse_fit_nov <- lapply(res_nov, function(x){x$mse_fit})%>%unlist
  
  err_fit_oob_iid <- lapply(res_iid, function(x){x$err_fit_oob})%>%unlist
  err_fit_oob_moving <- lapply(res_moving, function(x){x$err_fit_oob})%>%unlist
  err_fit_oob_circular <- lapply(res_circular, function(x){x$err_fit_oob})%>%unlist
  err_fit_oob_nov <- lapply(res_nov, function(x){x$err_fit_oob})%>%unlist
  
  mse_forecast_iid <- lapply(res_iid, function(x){x$mse_forecast})%>%unlist
  mse_forecast_moving <- lapply(res_moving, function(x){x$mse_forecast})%>%unlist
  mse_forecast_circular <- lapply(res_circular, function(x){x$mse_forecast})%>%unlist
  mse_forecast_nov <- lapply(res_nov, function(x){x$mse_forecast})%>%unlist
  
  mse_fit_iid_mat <- cbind(mse_fit_iid_mat, mse_fit_iid)
  mse_fit_moving_mat <- cbind(mse_fit_moving_mat, mse_fit_moving)
  mse_fit_circular_mat <- cbind(mse_fit_circular_mat, mse_fit_circular)
  mse_fit_nov_mat <- cbind(mse_fit_nov_mat, mse_fit_nov)
  
  err_fit_oob_iid_mat <- cbind(err_fit_oob_iid_mat, err_fit_oob_iid)
  err_fit_oob_moving_mat <- cbind(err_fit_oob_moving_mat, err_fit_oob_moving)
  err_fit_oob_circular_mat <- cbind(err_fit_oob_circular_mat, err_fit_oob_circular)
  err_fit_oob_nov_mat <- cbind(err_fit_oob_nov_mat, err_fit_oob_nov)
  
  mse_forecast_iid_mat <- cbind(mse_forecast_iid_mat, mse_forecast_iid)
  mse_forecast_moving_mat <- cbind(mse_forecast_moving_mat, mse_forecast_moving)
  mse_forecast_circular_mat <- cbind(mse_forecast_circular_mat, mse_forecast_circular)
  mse_forecast_nov_mat <- cbind(mse_forecast_nov_mat, mse_forecast_nov)
}


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

err_fit_oob_iid_MB <- err_fit_oob_iid_mat%>%rowMeans
err_fit_oob_moving_MB <- err_fit_oob_moving_mat%>%rowMeans
err_fit_oob_circular_MB <- err_fit_oob_circular_mat%>%rowMeans
err_fit_oob_nov_MB <- err_fit_oob_nov_mat%>%rowMeans

col <- piratepal("basel")
plot(block.size, err_fit_oob_iid_MB, type='b', pch=20, ylim=range(err_fit_oob_iid_MB, err_fit_oob_moving_MB, err_fit_oob_circular_MB, err_fit_oob_nov_MB))
lines(block.size, err_fit_oob_moving_MB, type='b', pch=20, col=col[1])
lines(block.size, err_fit_oob_circular_MB, type='b', pch=20, col=col[2])
lines(block.size, err_fit_oob_nov_MB, type='b', pch=20, col=col[3])
legend('topright', col=c('black', col[1:3]), c('iid', 'moving block', 'circular','non-overlapping'), bty='n', lty=1)


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
 





########representation des prévisions

res_boot<-  fit_forest2(block.size=40, bootstrap.ts="moving", seed = 1, param=param, data.pred=DataSim1)
fit_boot <- predict(res_boot$rf_fit, data=DataSim0)$prediction
forecast_boot <- predict(res_boot$rf_fit, data=DataSim1)$prediction

res_iid<-  fit_forest2(block.size=5, bootstrap.ts=NULL, seed = 1, param=param, data.pred=DataSim1)
fit_iid <- predict(res_iid$rf_fit, data=DataSim0)$prediction
forecast_iid <- predict(res_iid$rf_fit, data=DataSim1)$prediction


plot(DataSim0$t, DataSim0$y, type='l')
lines(DataSim0$t, fit_boot, col='blue')
lines(DataSim0$t, fit_iid, col='green')
lines(DataSim0$t, DataSim0$f, col='red', lty='dotted')



# plot(DataSim0$t_20, DataSim0$y)
# o <- order(DataSim0$t_20)
# lines(DataSim0$t_20[o], fit_boot[o], col='blue', pch=20)
# lines(DataSim0$t_20[o], fit_iid[o], col='green', pch=20)
# lines(DataSim0$t_20[1:20], DataSim0$f[1:20], col='red', pch=20)


plot(DataSim1$t, DataSim1$y, type='l')
lines(DataSim1$t, forecast_boot, col='blue')
lines(DataSim1$t, forecast_iid, col='green')
lines(DataSim1$t, DataSim1$f, col='red', lty='dotted')

oob.residuals <- DataSim0$y-res_iid$rf_fit$predictions
acf(oob.residuals, lag.max=60)

residuals <- DataSim0$y-fit_iid
acf(residuals, lag.max=60)





#####différences entre esimation et prédiction oob

plot(predict(res_iid$rf_fit, data=DataSim0)$prediction-res_iid$rf_fit$predictions)
plot(predict(res_boot$rf_fit, data=DataSim0)$prediction-res_boot$rf_fit$predictions)






############################################################################################################
##########################save results
############################################################################################################


res <- list()
res$mse_fit_iid_mat <- mse_fit_iid_mat
res$mse_fit_moving_mat <- mse_fit_moving_mat
res$mse_fit_circular_mat <- mse_fit_circular_mat
res$mse_fit_nov_mat <- mse_fit_nov_mat

res$mse_forecast_iid_mat <- mse_forecast_iid_mat
res$mse_forecast_moving_mat <- mse_forecast_moving_mat
res$mse_forecast_circular_mat <- mse_forecast_circular_mat
res$mse_forecast_nov_mat <- mse_forecast_nov_mat

res$mse_fit_iid <- mse_fit_iid_mat%>%rowMeans
res$mse_fit_moving <- mse_fit_moving_mat%>%rowMeans
res$mse_fit_circular <- mse_fit_circular_mat%>%rowMeans
res$mse_fit_nov <- mse_fit_nov_mat%>%rowMeans

res$mse_forecast_iid <- mse_forecast_iid_mat%>%rowMeans
res$mse_forecast_moving <- mse_forecast_moving_mat%>%rowMeans
res$mse_forecast_circular <- mse_forecast_circular_mat%>%rowMeans
res$mse_forecast_nov <- mse_forecast_nov_mat%>%rowMeans




saveRDS(res, file="Results/cos/cos_deux_period_sd05.RDS")










