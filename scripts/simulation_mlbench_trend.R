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

##as in bagging paper of breiman we choose learning set=200, test set=1000, 25 , we should repeat the procedure 100 times as breiman

n <- 1200

model <- list()
DataSim <- mlbench.friedman1_cor (n, sd = 1, model=model, coef.trend=0.01) 
plot(DataSim$y, type='l')


acf(DataSim$y)



Nsim <- 100
model <- list()
n <- 1200
n0 <- 200
mse_rf_iid_mat <- NULL
mse_rf_mb_mat <- NULL
mse_rf_cir_mat <- NULL
mse_rf_nov_mat <- NULL
block.size <- seq(1,40, by=5)
sd <- 1

for(i in c(1:Nsim))
{
  DataSim <- mlbench.friedman1_cor (n, sd = sd, model=model, coef.trend=0.01) 
  DataSim <- data.frame(y=DataSim$y, x=DataSim$x)

  DataSim0 <- DataSim[1:n0,]
  DataSim1 <- DataSim[(n0+1):n,]
  
  param <- list(num.trees=100,  mtry=10)
  param$data <- DataSim0
  param$formula <- y~.
  param$sample.fraction <- 1

  #####iid
  param$activate.ts <- F
  rf_iid <- do.call(ranger, args=param, quote = FALSE, envir = parent.frame())
  rf_iid.forecast <- predict(rf_iid, data=DataSim1)$predictions
  mse_rf_iid <- mean((DataSim1$y-rf_iid.forecast)^2)
  
  #####moving block boostrap  
  mse_rf_mb <- lapply(block.size , fit_forest,  bootstrap.ts="moving", activate.ts=T)%>%unlist
  
  
  #####circular moving block boostrap  
  mse_rf_cir <- lapply(block.size , fit_forest,  bootstrap.ts="circular", activate.ts=T)%>%unlist
  
  #####non-overlapping
  mse_rf_nov <- lapply(block.size , fit_forest,  bootstrap.ts="nonoverlapping", activate.ts=T)%>%unlist
  
  mse_rf_iid_mat <- cbind(mse_rf_iid_mat, mse_rf_iid)
  mse_rf_mb_mat <- cbind(mse_rf_mb_mat, mse_rf_mb)
  mse_rf_cir_mat <- cbind(mse_rf_cir_mat, mse_rf_cir)
  mse_rf_nov_mat <- cbind(mse_rf_nov_mat, mse_rf_nov)
}


mse_rf_iid <- rowMeans(mse_rf_iid_mat)
mse_rf_mb <- rowMeans(mse_rf_mb_mat)
mse_rf_cir <- rowMeans(mse_rf_cir_mat)
mse_rf_nov <- rowMeans(mse_rf_nov_mat)

res <- list()
res$mse_rf_iid_mat <- mse_rf_iid_mat
res$mse_rf_mb_mat <- mse_rf_mb_mat
res$mse_rf_cir_mat <- mse_rf_cir_mat
res$mse_rf_nov_mat <- mse_rf_nov_mat

res$mse_rf_iid <- mse_rf_iid
res$mse_rf_mb <- mse_rf_mb
res$mse_rf_cir <- mse_rf_cir
res$mse_rf_nov <- mse_rf_nov

saveRDS(res, file="Results/mlbench.friedman1/trend.RDS")


col <- piratepal("basel")
plot(block.size, mse_rf_mb, type='l', ylim=range(mse_rf_iid, mse_rf_mb, mse_rf_cir,mse_rf_nov), col=col[1])
lines(block.size, mse_rf_cir, col=col[2])
lines(block.size, mse_rf_nov, col=col[3])
abline(h=mse_rf_iid)
legend('topright', col=c('black', col[1:3]), c('iid', 'moving block', 'circular','non-overlapping'), bty='n', lty=1)







eps <- arima.sim(n = 1000, model=model, sd = 1)
var(eps)
acf(eps)
