rm(list=objects())
library(RColorBrewer)
library(magrittr)
library(opera)
library(yarrr)
library(R39Toolbox)
library(rangerts)
library(pdp)
library(mlbench)
library(tidyverse)
source("R/mlbench.friedman1_cor.R")
source("R/fit_forest_Hui.R")

Nsim <- 100
# bad example
# n <- 480

# better example
n <- 4800
n0 <- n / 2
t <- c(1:n)
sd <- 1/2
a <- 1/10
w <- 2*pi/20
delta <- w/10
cos05_t <- cos(0.5*w*t)
cos1_t <- cos(w*t)

f <- cos05_t + cos1_t  
t_20 <- rep(c(1:20), length.out = n)
t_40 <- rep(c(1:40), length.out = n)

eps <- rnorm(n, 0, sd)
y <- f + eps

plot(t, y, type='l')
lines(t, f, col='red')


param <- list(num.trees=500,  mtry=1)
param$formula <- y ~ t_20
param$sample.fraction <- 1


# block.size <- c(1, seq(5,40, by=5))
block.size <- c(1, 2, 5, 10, 20, 40, 60, 80, 100)

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

freq_iid_list      <- list()
freq_moving_list   <- list()
freq_circular_list <- list()
freq_nov_list      <- list()

for(i in 1:Nsim) {
  print(i)
  # eps <- rnorm(n, 0, sd)
  
  # DataSim <- data.frame(y, t_40, t_20)
  DataSim <- data.frame(y, cos05_t, cos1_t, t_40, t_20)
  
  #DataSim <- data.frame(y, t, f=a*t)
  DataSim0 <- DataSim[1:n0,]
  DataSim1 <- DataSim[(n0+1):n,]
  param$data <- DataSim0
  #param$data.pred <- DataSim1
  param$keep.inbag <- T
  
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
  
  freq_iid <- map_dfc(res_iid, function(res_by_block_size) {
    tibble(freq = map_dfc(res_by_block_size$rf_fit$inbag.counts, ~tibble(freq = .x)) %>% 
      as.matrix() %>% 
      rowSums())
  })
  names(freq_iid) <- str_c("freq_", block.size)
  freq_iid_list[[i]] <- freq_iid
  
  freq_moving <- map_dfc(res_moving, function(res_by_block_size) {
    tibble(freq = map_dfc(res_by_block_size$rf_fit$inbag.counts, ~tibble(freq = .x)) %>% 
             as.matrix() %>% 
             rowSums())
  })
  names(freq_moving) <- str_c("freq_", block.size)
  freq_moving_list[[i]] <- freq_moving
  
  freq_circular <- map_dfc(res_circular, function(res_by_block_size) {
    tibble(freq = map_dfc(res_by_block_size$rf_fit$inbag.counts, ~tibble(freq = .x)) %>% 
             as.matrix() %>% 
             rowSums())
  })
  names(freq_circular) <- str_c("freq_", block.size)
  freq_circular_list[[i]] <- freq_circular
  
  freq_nov <- map_dfc(res_nov, function(res_by_block_size) {
    tibble(freq = map_dfc(res_by_block_size$rf_fit$inbag.counts, ~tibble(freq = .x)) %>% 
             as.matrix() %>% 
             rowSums())
  })
  names(freq_nov) <- str_c("freq_", block.size)
  freq_nov_list[[i]] <- freq_nov
}


mse_fit_iid_MB <- mse_fit_iid_mat%>%rowMeans
mse_fit_moving_MB <- mse_fit_moving_mat%>%rowMeans
mse_fit_circular_MB <- mse_fit_circular_mat%>%rowMeans
mse_fit_nov_MB <- mse_fit_nov_mat%>%rowMeans

# mse train
col <- piratepal("basel")
plot(block.size, mse_fit_iid_MB, type='b', pch=20, 
  ylim=range(mse_fit_iid_MB, mse_fit_moving_MB, mse_fit_circular_MB, mse_fit_nov_MB),
  main = "train mse")
lines(block.size, mse_fit_moving_MB, type='b', pch=20, col=col[1])
lines(block.size, mse_fit_circular_MB, type='b', pch=20, col=col[2])
lines(block.size, mse_fit_nov_MB, type='b', pch=20, col=col[3])
legend('topright', col = c('black', col[1:3]),
  c('iid', 'moving block', 'circular','non-overlapping'), bty='n', lty=1)

err_fit_oob_iid_MB <- err_fit_oob_iid_mat%>%rowMeans
err_fit_oob_moving_MB <- err_fit_oob_moving_mat%>%rowMeans
err_fit_oob_circular_MB <- err_fit_oob_circular_mat%>%rowMeans
err_fit_oob_nov_MB <- err_fit_oob_nov_mat%>%rowMeans

# mse oob
col <- piratepal("basel")
plot(block.size, err_fit_oob_iid_MB, type='b', pch=20, 
  ylim=range(err_fit_oob_iid_MB, err_fit_oob_moving_MB, 
    err_fit_oob_circular_MB, err_fit_oob_nov_MB),
  main = "oob mse")
lines(block.size, err_fit_oob_moving_MB, type='b', pch=20, col=col[1])
lines(block.size, err_fit_oob_circular_MB, type='b', pch=20, col=col[2])
lines(block.size, err_fit_oob_nov_MB, type='b', pch=20, col=col[3])
legend('topright', col=c('black', col[1:3]), c('iid', 'moving block', 'circular','non-overlapping'), bty='n', lty=1)

# mse test
mse_forecast_iid_MB <- mse_forecast_iid_mat%>%rowMeans
mse_forecast_moving_MB <- mse_forecast_moving_mat%>%rowMeans
mse_forecast_circular_MB <- mse_forecast_circular_mat%>%rowMeans
mse_forecast_nov_MB <- mse_forecast_nov_mat%>%rowMeans

col <- piratepal("basel")
plot(block.size, mse_forecast_iid_MB, type='b', pch=20, 
  ylim=range(mse_forecast_iid_MB, mse_forecast_moving_MB, 
    mse_forecast_circular_MB, mse_forecast_nov_MB),
  main = "test mse")
lines(block.size, mse_forecast_moving_MB, type='b', pch=20, col=col[1])
lines(block.size, mse_forecast_circular_MB, type='b', pch=20, col=col[2])
lines(block.size, mse_forecast_nov_MB, type='b', pch=20, col=col[3])
legend('topright', col=c('black', col[1:3]), 
  c('iid', 'moving block', 'circular','non-overlapping'), 
  bty='n', lty=1)



# ######## representation des prévisions ####
# DataSim <- data.frame(y, t, t_40, t_20, f, cos05_t, cos1_t)
# 
# #DataSim <- data.frame(y, t, f=a*t)
# DataSim0 <- DataSim[1:n0,]
# DataSim1 <- DataSim[(n0+1):n,]
# res_boot<-  fit_forest2(block.size=40, bootstrap.ts="circular", seed = 1, param=param, data.pred=DataSim1)
# fit_boot <- predict(res_boot$rf_fit, data=DataSim0)$prediction
# forecast_boot <- predict(res_boot$rf_fit, data=DataSim1)$prediction
# 
# res_iid<-  fit_forest2(block.size=1, bootstrap.ts=NULL, seed = 1, param=param, data.pred=DataSim1)
# fit_iid <- predict(res_iid$rf_fit, data=DataSim0)$prediction
# forecast_iid <- predict(res_iid$rf_fit, data=DataSim1)$prediction
# 
# 
# plot(DataSim0$t, DataSim0$y, type='l')
# lines(DataSim0$t, fit_boot, col='blue')
# lines(DataSim0$t, fit_iid, col='green')
# lines(DataSim0$t, DataSim0$f, col='red', lty='dotted')
# 
# 
# 
# # plot(DataSim0$t_20, DataSim0$y)
# # o <- order(DataSim0$t_20)
# # lines(DataSim0$t_20[o], fit_boot[o], col='blue', pch=20)
# # lines(DataSim0$t_20[o], fit_iid[o], col='green', pch=20)
# # lines(DataSim0$t_20[1:20], DataSim0$f[1:20], col='red', pch=20)
# 
# 
# plot(DataSim1$t, DataSim1$y, type='l')
# lines(DataSim1$t, forecast_boot, col='blue')
# lines(DataSim1$t, forecast_iid, col='green')
# lines(DataSim1$t, DataSim1$f, col='red', lty='dotted')
# 
# oob.residuals <- DataSim0$y-res_iid$rf_fit$predictions
# acf(oob.residuals, lag.max = 60)
# oob.residuals <- DataSim0$y-res_boot$rf_fit$predictions
# acf(oob.residuals, lag.max = 60)
# 
# residuals <- DataSim0$y-fit_iid
# acf(residuals, lag.max=60)
# 
# 
# 
# 
# 
# #####différences entre esimation et prédiction oob
# 
# plot(predict(res_iid$rf_fit, data=DataSim0)$prediction-res_iid$rf_fit$predictions)
# plot(predict(res_boot$rf_fit, data=DataSim0)$prediction-res_boot$rf_fit$predictions)
# 
# plot(DataSim0$y - predict(res_iid$rf_fit, data=DataSim0)$prediction)
# plot(DataSim0$y - predict(res_boot$rf_fit, data=DataSim0)$prediction)


# ############################################################################################################
# ##########################save results
# ############################################################################################################
# 
# 
# res <- list()
# res$mse_fit_iid_mat <- mse_fit_iid_mat
# res$mse_fit_moving_mat <- mse_fit_moving_mat
# res$mse_fit_circular_mat <- mse_fit_circular_mat
# res$mse_fit_nov_mat <- mse_fit_nov_mat
# 
# res$mse_forecast_iid_mat <- mse_forecast_iid_mat
# res$mse_forecast_moving_mat <- mse_forecast_moving_mat
# res$mse_forecast_circular_mat <- mse_forecast_circular_mat
# res$mse_forecast_nov_mat <- mse_forecast_nov_mat
# 
# res$mse_fit_iid <- mse_fit_iid_mat%>%rowMeans
# res$mse_fit_moving <- mse_fit_moving_mat%>%rowMeans
# res$mse_fit_circular <- mse_fit_circular_mat%>%rowMeans
# res$mse_fit_nov <- mse_fit_nov_mat%>%rowMeans
# 
# res$mse_forecast_iid <- mse_forecast_iid_mat%>%rowMeans
# res$mse_forecast_moving <- mse_forecast_moving_mat%>%rowMeans
# res$mse_forecast_circular <- mse_forecast_circular_mat%>%rowMeans
# res$mse_forecast_nov <- mse_forecast_nov_mat%>%rowMeans
# 
# 
# 
# 
# saveRDS(res, file="Results/cos/cos_deux_period_sd05.RDS")














freq_iid <- map_dfc(block.size, function(i) {
  map_dfc(freq_iid_list, function(res) {
    res[[str_c("freq_", i)]]
  }) %>% rowMeans()
}) %>% mutate(mode = "iid")
names(freq_iid)[1:length(block.size)] <- str_c("freq_", block.size)

freq_moving <- map_dfc(block.size, function(i) {
  map_dfc(freq_moving_list, function(res) {
    res[[str_c("freq_", i)]]
  }) %>% rowMeans()
}) %>% mutate(mode = "moving")
names(freq_moving)[1:length(block.size)] <- str_c("freq_", block.size)

freq_circular <- map_dfc(block.size, function(i) {
  map_dfc(freq_circular_list, function(res) {
    res[[str_c("freq_", i)]]
  }) %>% rowMeans()
}) %>% mutate(mode = "circular")
names(freq_circular)[1:length(block.size)] <- str_c("freq_", block.size)

freq_nov <- map_dfc(block.size, function(i) {
  map_dfc(freq_nov_list, function(res) {
    res[[str_c("freq_", i)]]
  }) %>% rowMeans()
}) %>% mutate(mode = "nov")
names(freq_nov)[1:length(block.size)] <- str_c("freq_", block.size)

freq <- bind_rows(freq_iid, freq_moving, freq_circular, freq_nov)


ggplot(freq) +
  geom_point(aes(x = 1:nrow(freq), y = freq_1)) +
  facet_wrap(vars(mode), scales = "free")

ggplot(freq) +
  geom_point(aes(x = rep(1:(nrow(freq)/4), 4), y = freq_80)) +
  facet_wrap(vars(mode)) +
  ylim(480, 540)

ggplot(freq) +
  geom_point(aes(x = 1:nrow(freq), y = freq_20)) +
  facet_wrap(vars(mode), scales = "free")

ggplot(freq) +
  geom_point(aes(x = 1:nrow(freq), y = freq_40)) +
  facet_wrap(vars(mode), scales = "free")

ggplot(freq) +
  geom_point(aes(x = 1:nrow(freq), y = freq_80)) +
  facet_wrap(vars(mode), scales = "free")
