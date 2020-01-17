library(rangerts)
library(magrittr)
library(plotly)
library(latex2exp)
library(opera)
library(RColorBrewer)
library(yarrr)
library(ggplot2)
library(ggthemr)
library(tidyr)
library(scales)
library(mgcv)
library(rlist)
library(stats)
rmse<-function(y, ychap)
{
  return(round(sqrt(mean((y-ychap)^2,na.rm=TRUE)),digits=5))
}
mae<-function(y,ychap){
  eps <- y - ychap
  return(round(mean(abs(eps),na.rm=TRUE),digits=10))
}
mape<-function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=10))
}

AR <- function(n,phi){
  eps <- rnorm(n, 0, 1)
  x_1 = rnorm(1,0,1)
  X1 = phi*x_1 + eps[1]
  X = c(X1)
  X_t_1 = c(x_1)
  for(i in c(2:n)){
    X_t = phi*X[i-1] + eps[i]
    X = c(X, X_t)
    X_t_1 = c(X_t_1, X[i-1])
  }
  return(data.frame(X_t=X, X_t_1 = X_t_1, t=c(1:n)))
}

AR_season <- function(n,phi, a_t){
  D = length(a_t)
  eps <- rnorm(n, 0, 1)
  x_1 = rnorm(1,0,1)
  X1 = phi*x_1 + eps[1]
  X = c(X1)
  X_t_1 = c(x_1)
  for(i in c(2:n)){
    if(i%%D!=0){
      X_t = phi*X[i-1] + a_t[i%%D]*eps[i]
      X = c(X, X_t)
      X_t_1 = c(X_t_1, X[i-1])
    }
    else{
      X_t = phi*X[i-1] + a_t[D]*eps[i]
      X = c(X, X_t)
      X_t_1 = c(X_t_1, X[i-1])
    }
  }
  return(data.frame(X_t=X, X_t_1 = X_t_1, t=c(1:n)))
}

M1 <- function(n,d){
  U <- runif(n, -0.5, 0.5)
  t <- c(1:n)
  X <- U*(t%%d +1) + sin(2*pi*t/d)
  return(data.frame(X_t=X, U_t=U, t=t))
}

M2 <- function(n , d){
  eps <- rnorm(n, 0, 1)
  x_2 = rnorm(1,0,1)
  x_1 = rnorm(1,0,1)
  X1 = 0.5*x_1 + 0.25*x_2 + 1%%d + eps[1]
  X2 = 0.5*X1 + 0.25*x_1 +  2%%d + eps[2]
  X = c(X1, X2)
  X_t_1 = c(x_1,X1)
  X_t_2 = c(x_2,x_1)
  for(i in c(3:n)){
    X_t = 0.5*X[i-1] + 0.25*X[i-2] + i%%d + eps[i]
    X = c(X, X_t)
    X_t_1 = c(X_t_1, X[i-1])
    X_t_2 = c(X_t_2, X[i-2])
  }
  return(data.frame(X_t=X, X_t_1 = X_t_1, X_t_2=X_t_2, t=c(1:n)))
}

M3 <- function(n , d){
  eps <- rnorm(n, 0, 1)
  x_2 = rnorm(1,0,1)
  x_1 = rnorm(1,0,1)
  X1 = 0.8*x_1 -0.6*x_2  + eps[1] + 0.3*rnorm(1,0,1) + 1%%d
  X2 = 0.8*X1 - 0.6*x_1 + eps[2] + 0.3*eps[1] + 2%%d 
  X = c(X1, X2)
  X_t_1 = c(x_1,X1)
  X_t_2 = c(x_2,x_1)
  for(i in c(3:n)){
    X_t = 0.8*X[i-1] - 0.6*X[i-2] + i%%d + eps[i] + 0.3*eps[i-1]
    X = c(X, X_t)
    X_t_1 = c(X_t_1, X[i-1])
    X_t_2 = c(X_t_2, X[i-2])
  }
  return(data.frame(X_t=X, X_t_1 = X_t_1, X_t_2=X_t_2, t=c(1:n)))
}


M3_norm <- function(n , d){
  eps <- rnorm(n, 0, 1)
  x_1 = rnorm(1,0,1)
  x_0 = rnorm(1,0,1)
  X1 = eps[1] + 0.4*x_0 + 0.6*x_1 + 1%%d + cos(2*pi*1/d)*eps[1]
  X2 = eps[2] + 0.4*eps[1] + 0.6*x_0 +  2%%d + cos(2*pi*2/d)*eps[2]
  X = c(X1, X2)
  for(t in c(3:n)){
    X_t = eps[t] + 0.4*eps[t-1] + 0.6*eps[t-2] +  2%%d + cos(2*pi*t/d)*eps[t]
    X = c(X, X_t)
  }
  return(data.frame(X_t=X, t=c(1:n)%%d))
}

M3_unif <- function(n , d){
  eps <- runif(n, 0, 1)
  x_1 = runif(1,0,1)
  x_0 = runif(1,0,1)
  X1 = eps[1] + 0.4*x_0 + 0.6*x_1 + 1%%d + cos(2*pi*1/d)*eps[1]
  X2 = eps[2] + 0.4*eps[1] + 0.6*x_0 +  2%%d + cos(2*pi*2/d)*eps[2]
  X = c(X1, X2)
  for(t in c(3:n)){
    X_t = eps[t] + 0.4*eps[t-1] + 0.6*eps[t-2] +  2%%d + cos(2*pi*t/d)*eps[t]
    X = c(X, X_t)
  }
  return(data.frame(X_t=X, t=c(1:n)%%d))
}

friedman_m3_unif <- function(n,d){
  Data_m3 <- M3_unif(n+4,d)
  x_temp <- matrix(rep(0,(n+4)*5), ncol = 5)
  ts_x <- Data_m3$X_t
  x_temp[,1] <- ts_x
  x_temp[,2] <- c(ts_x[1], ts_x[1:(length(ts_x)-1)])
  x_temp[,3] <- c(ts_x[1:2], ts_x[1:(length(ts_x)-2)])
  x_temp[,4] <- c(ts_x[1:3], ts_x[1:(length(ts_x)-3)])
  x_temp[,5] <- c(ts_x[1:4], ts_x[1:(length(ts_x)-4)])
  x <- x_temp[c(5:nrow(x_temp)),]
  y <- 10 * sin(pi * x[, 1] * x[, 2])
  y <- y + 20 * (x[, 3] - 0.5)^2 + 10 * x[, 4] + 5 * x[, 5]
  return(data.frame(X_t=y, x=x, t=c(1:n)))
}

friedman_m3_norm <- function(n,d){
  Data_m3 <- M3_norm(n+4,d)
  x_temp <- matrix(rep(0,(n+4)*5), ncol = 5)
  ts_x <- Data_m3$X_t
  x_temp[,1] <- ts_x
  x_temp[,2] <- c(ts_x[1], ts_x[1:(length(ts_x)-1)])
  x_temp[,3] <- c(ts_x[1:2], ts_x[1:(length(ts_x)-2)])
  x_temp[,4] <- c(ts_x[1:3], ts_x[1:(length(ts_x)-3)])
  x_temp[,5] <- c(ts_x[1:4], ts_x[1:(length(ts_x)-4)])
  x <- x_temp[c(5:nrow(x_temp)),]
  y <- 10 * sin(pi * x[, 1] * x[, 2])
  y <- y + 20 * (x[, 3] - 0.5)^2 + 10 * x[, 4] + 5 * x[, 5]
  return(data.frame(X_t=y, x=x, t=c(1:n)))
}

list_m1 = M2(250,10)
list_m2 = M3_norm(500,d)
list_m2 = friedman_m3_unif(500,d)

eq <- X_t ~.

Monte_carlo_round <- 20
mtry <- 1
block_length_seq <-  c(c(2,5,10,25,50,100,125, 150, 175, 200, 225, 250, 275, 300), seq(325, 1000, 25))
block_length_seq <- c(2,3,4,5,6,7,8,9,10,11,12,23,24,25,26,27,48,49,50,51,52,98,99,100,101,102,123,124,125,126,127)


#for(d in c(10,50,100)){
for(phi in c(0.3,0.5,0.7,0.9,0.95)){
  for(j in c(1:Monte_carlo_round)){
    Dataset <- AR(1250,phi)
    #a_t = c(1, 1, 1, 2, 3, 1, 1, 1, 1, 2, 4, 6)
    #Dataset <- AR_season(1250,phi,a_t)
    Dataset <- M1(1250,100)
    plot(Dataset$X_t, type='l', ylab="X_t", xlab='Time', main='Modèle 2, d=100')
    #Data0 <- Dataset[c(1:250),]
    #Data2 <- Dataset[c(251:500),]
    Data0 <- Dataset[c(1:1000),]
    Data2 <- Dataset[c(1001:1250),]
    
    error_rmse_RF_circ_2 <- c()
    error_rmse_RF_nono_2 <- c()
    error_rmse_RF_moving_2 <- c()
    error_rmse_RF_stat_2 <- c()
    error_rmse_RF_season_2 <- c()
    
    error_mae_RF_circ_2 <- c()
    error_mae_RF_nono_2 <- c()
    error_mae_RF_moving_2 <- c()
    error_mae_RF_stat_2 <- c()
    error_mae_RF_season_2 <- c()
    
    RF <- rangerts::ranger(eq, data=Data0,mtry=mtry)
    RF_forecast_2 <- predict(RF, data=Data2)$prediction
    RF <- NULL
    RF.mae_2 <-mae(Data2$X_t, RF_forecast_2)
    RF.rmse_2 <-rmse(Data2$X_t, RF_forecast_2)
    
    for(i in block_length_seq){
      #train
      RF_circ <- rangerts::ranger(eq, data=Data0,  mtry=mtry, block.size = i, bootstrap.ts = "circular", by.end=FALSE)
      RF_circ_forecast_2 <- predict(RF_circ, data=Data2)$prediction
      RF_circ.mae_2 <-mae(Data2$X_t, RF_circ_forecast_2)
      RF_circ.rmse_2 <-rmse(Data2$X_t, RF_circ_forecast_2)
      RF_circ <- NULL
      
      RF_nono <- rangerts::ranger(eq, data=Data0,  mtry=mtry, block.size = i, bootstrap.ts = "nonoverlapping", by.end=FALSE)
      RF_nono_forecast_2 <- predict(RF_nono, data=Data2)$prediction
      RF_nono.mae_2 <-mae(Data2$X_t, RF_nono_forecast_2)
      RF_nono.rmse_2 <-rmse(Data2$X_t, RF_nono_forecast_2)
      RF_nono_forecast_2 <- NULL
      RF_nono <- NULL
      
      # 
      RF_moving <- rangerts::ranger(eq, data=Data0,  mtry=mtry,block.size = i, bootstrap.ts = "moving", by.end=FALSE)
      RF_moving_forecast_2 <- predict(RF_moving, data=Data2)$prediction
      #RF_moving.mape_2 <-mape(Data2$Y, RF_moving_forecast_2)
      RF_moving.mae_2 <-mae(Data2$X_t, RF_moving_forecast_2)
      RF_moving.rmse_2 <-rmse(Data2$X_t, RF_moving_forecast_2)
      RF_moving_forecast_2 <- NULL
      RF_moving <- NULL
      
      error_mae_RF_circ_2 <- c(error_mae_RF_circ_2, RF_circ.mae_2)
      error_mae_RF_nono_2 <- c(error_mae_RF_nono_2, RF_nono.mae_2)
      error_mae_RF_moving_2 <- c(error_mae_RF_moving_2, RF_moving.mae_2)
      
      error_rmse_RF_circ_2 <- c(error_rmse_RF_circ_2, RF_circ.rmse_2)
      error_rmse_RF_nono_2 <- c(error_rmse_RF_nono_2, RF_nono.rmse_2)
      error_rmse_RF_moving_2 <- c(error_rmse_RF_moving_2, RF_moving.rmse_2)
      
    }
    results <- list()
    results$bloc_seq <- block_length_seq
    
    ####RMSE
    results$RF.rmse_2 <-RF.rmse_2
    results$RF_circ.rmse_2 <-error_rmse_RF_circ_2
    results$RF_nono.rmse_2 <-error_rmse_RF_nono_2
    results$RF_moving.rmse_2 <-error_rmse_RF_moving_2
    
    ####mae
    results$RF.mae_2 <-RF.mae_2
    results$RF_circ.mae_2 <-error_mae_RF_circ_2
    results$RF_nono.mae_2 <-error_mae_RF_nono_2
    results$RF_moving.mae_2 <-error_mae_RF_moving_2
    
    #nomF <- paste0("Result_friedman_m3_norm.d_",d, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".RDS")
    #nomF <- paste0("Result_AR_season.phi_",phi, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".augmented_block_extra.RDS")
    nomF <- paste0("Result_AR.phi_",phi, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".augmented_block_extra.RDS")
    #nomF <- paste0("Result_M1.d_",d, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".augmented_block_extra.RDS")
    
    saveRDS(results, file=paste0("~/Documents/", nomF))
    
    #results <- NULL
    print(j)
  }
}

#for(d in c(10,50,100)){
#for(phi in c(0.3,0.5,0.7,0.9,0.95)){
RF_a <- NULL
RF_circ_a <- NULL
RF_nono_a <- NULL
RF_moving_a <- NULL
for(j in c(1:Monte_carlo_round)){
  #nomF <- paste0("Result_M1.d_",100, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".RDS")
  #nomF <- paste0("Result_M1.d_",100, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".augmented_block_extra.RDS")
  #nomF <- paste0("Result_M2.d_",10, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".augmented_block_extra.RDS")
  nomF <- paste0("Result_AR_season.phi_",0.9, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".augmented_block.RDS")
  #nomF <- paste0("Result_AR.phi_",0.7, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".augmented_block_extra.RDS")
  #nomF <- paste0("Result_friedman_m3_unif.d_",10, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".augmented_block.RDS")
  #nomF <- paste0("Result_friedman_m3_norm.d_",10, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".RDS")
  file <- readRDS(file=paste0("~/Documents/", nomF))
  RF_a <- c(RF_a, file$RF.rmse_2)
  RF_circ_a <- rbind(RF_circ_a, file$RF_circ.rmse_2)
  RF_moving_a <- rbind(RF_moving_a, file$RF_moving.rmse_2)
  RF_nono_a <- rbind(RF_nono_a, file$RF_nono.rmse_2)
}
block_length_seq = file$bloc_seq
error_block_dependent <- cbind(colMeans(RF_moving_a),colMeans(RF_nono_a),colMeans(RF_circ_a),rep(mean(RF_a), length(block_length_seq)))
data_error_block_dependent <- data.frame(Block_size = block_length_seq, rmse = c(error_block_dependent), 
                                         name = rep(c("Moving","Nonoverlap","circ", "I.I.D"), each = length(block_length_seq)))

mycols_fill <- c(rgb(114,147,203, maxColorValue = 255), rgb(225,151,76, maxColorValue = 255), rgb(132,186,91, maxColorValue = 255))
mycols_line <- c(rgb(57,106,177, maxColorValue = 255), "black",rgb(218,124,48, maxColorValue = 255), rgb(62,150,81, maxColorValue = 255))
ggplot(data_error_block_dependent, aes(x=Block_size, y=rmse)) + geom_line(aes(colour = name),size=3) + 
  xlab("Block size") + ylab("RMSE") + ggtitle("phi=0.9") +
  scale_colour_manual(values = mycols_line) +
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_continuous(breaks=pretty_breaks())+
  #geom_hline(yintercept=mean(RF_a), color="black", size=0.5)+
  labs(colour = "Variant")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=40,face="bold"),
        legend.title = element_text(size=20), legend.text = element_text(size=20),legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"))



phi = c(0.3,0.5,0.7,0.9,0.95)
d <- c(10, 50, 100)
phi = 0.3
d = 50
a_t = c(1, 1, 1, 2, 3, 1, 1, 1, 1, 2, 4, 6)
Data <- AR_season(1000,phi,a_t)
#Data <- M1(1000,d)
plot(Data$X_t, type='l')
pacf(Data$X_t, lag=1000)
acf(Data$X_t, lag=1000)


library(WaveletComp)
library(wavelets)
library(forecast)


eq <- X_t ~ .
RF <- rangerts::ranger(eq, data=Data,mtry=2)
RF_forecast <- RF$predictions
residus <- Data$X_t - RF_forecast
Data$Residus <- residus
Data$Residus_arima <- auto.arima(Data$X_t)$residuals

acf(Data$Residus,lag=1000)
pacf(Data$Residus,lag=1000)
acf(Data$Residus_arima,lag=1000)
pacf(Data$Residus_arima,lag=1000)
my.w <- analyze.wavelet(Data, "Residus",loess.span = 0,dt = 1,lowerPeriod = 6,upperPeriod = 2000,make.pval = TRUE, n.sim = 10)
#wt.image(my.w, color.key = "quantile", n.levels = 250,legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.avg(my.w, siglvl = 0.01, sigcol = "red", sigpch = 20,periodlab = "period (days)")

d = 10
phi = c(0.3,0.5,0.7,0.9,0.95)
phi = 0.3
a_t = c(1, 1, 1, 2, 3, 1, 1, 1, 1, 2, 4, 6)
list_acf_residus_rf = c()
list_acf_residus_arima = c()
list_pacf_residus_rf = c()
list_pacf_residus_arima = c()
for(i in c(1:Monte_carlo_round)){
  #Data <- M1(1000,d)
  Data <- AR_season(1000,phi,a_t)
  #Data <- AR(1000,phi)
  eq <- X_t ~ .
  RF <- rangerts::ranger(eq, data=Data,mtry=2)
  RF_forecast <- RF$predictions
  residus <- Data$X_t - RF_forecast
  residus_arima <- auto.arima(Data$X_t)$residuals
  
  list_acf_residus_rf = rbind(list_acf_residus_rf, acf(residus,lag=1000)$acf)
  list_acf_residus_arima = rbind(list_acf_residus_arima, acf(residus_arima,lag=1000)$acf)
  list_pacf_residus_rf = rbind(list_pacf_residus_rf, pacf(residus,lag=1000)$acf)
  list_pacf_residus_arima = rbind(list_pacf_residus_arima, pacf(residus_arima,lag=1000)$acf)
  
  print(i)
}
ci = 0.95
clim0 <- qnorm((1 + ci)/2)/sqrt(1000)

matplot(t(list_acf_residus_rf), cex=0.1, pch = rep(c(0:9),2))
lines(x = c(1:1000), y = rep(clim0, 1000))
lines(x = c(1:1000), y = rep(-clim0, 1000))
lines(x = c(1:1000), y = rep(sqrt(2)*clim0, 1000))
lines(x = c(1:1000), y = rep(-sqrt(2)*clim0, 1000))
matplot(t(list_acf_residus_arima), cex=0.1, pch = rep(c(0:9),2))
lines(x = c(1:1000), y = rep(clim0, 1000))
lines(x = c(1:1000), y = rep(-clim0, 1000))
lines(x = c(1:1000), y = rep(sqrt(2)*clim0, 1000))
lines(x = c(1:1000), y = rep(-sqrt(2)*clim0, 1000))
matplot(t(list_pacf_residus_rf), cex=0.1, pch = rep(c(0:9),2))
lines(x = c(1:1000), y = rep(clim0, 1000))
lines(x = c(1:1000), y = rep(-clim0, 1000))
lines(x = c(1:1000), y = rep(sqrt(2)*clim0, 1000))
lines(x = c(1:1000), y = rep(-sqrt(2)*clim0, 1000))
matplot(t(list_pacf_residus_arima), cex=0.1, pch = rep(c(0:9),2))
lines(x = c(1:1000), y = rep(clim0, 1000))
lines(x = c(1:1000), y = rep(-clim0, 1000))
lines(x = c(1:1000), y = rep(sqrt(2)*clim0, 1000))
lines(x = c(1:1000), y = rep(-sqrt(2)*clim0, 1000))
lines(x = c(1:1000), y = rep(2*clim0, 1000))
lines(x = c(1:1000), y = rep(-2*clim0, 1000))

recup_pacf_residus_rf = c()
recup_pacf_residus_arima = c()
for (r in 1:nrow(list_pacf_residus_arima)){
  for (c in 1:ncol(list_pacf_residus_arima)){
    if( abs(list_pacf_residus_arima[r,c])>= sqrt(2)*clim0){
      recup_pacf_residus_arima = c(recup_pacf_residus_arima, c)
    }
    if( abs(list_pacf_residus_rf[r,c])>= sqrt(2)*clim0){
      recup_pacf_residus_rf = c(recup_pacf_residus_rf, c)
    }
  }
}
recup_pacf_residus_arima = sort(unique(recup_pacf_residus_arima[recup_pacf_residus_arima > 1]))
recup_pacf_residus_rf = sort(unique(recup_pacf_residus_arima[recup_pacf_residus_rf > 1]))

plot(recup_pacf_residus_arima, type='l')
lines(recup_pacf_residus_rf)
lines(recup_pacf_residus_arima)
plot(block_length_seq, type='l')

eq <- X_t ~.
Monte_carlo_round <- 200
mtry <- 2
block_length_seq <- recup_pacf_residus_arima
#for(d in c(10,50,100)){
#for(d in c(10)){
#for(phi in c(0.3,0.5,0.7,0.9,0.95)){
for(phi in c(0.3)){
  for(j in c(1:Monte_carlo_round)){
    #Dataset <- AR(1250,phi)
    #a_t = c(1, 1, 1, 2, 3, 1, 1, 1, 1, 2, 4, 6)
    Dataset <- AR_season(1250,phi,a_t)
    #Dataset <- M1(1250, d)
    #Dataset <- M2(1250,d)
    #plot(Dataset$X_t, type='l')
    #Data0 <- Dataset[c(1:250),]
    #Data2 <- Dataset[c(251:500),]
    Data0 <- Dataset[c(1:1000),]
    Data2 <- Dataset[c(1001:1250),]
    
    error_rmse_RF_moving_2 <- c()
    error_mae_RF_moving_2 <- c()
    
    RF <- rangerts::ranger(eq, data=Data0,mtry=mtry)
    RF_forecast_2 <- predict(RF, data=Data2)$prediction
    RF <- NULL
    RF.mae_2 <-mae(Data2$X_t, RF_forecast_2)
    RF.rmse_2 <-rmse(Data2$X_t, RF_forecast_2)
    
    for(i in block_length_seq){
      RF_moving <- rangerts::ranger(eq, data=Data0,  mtry=mtry,block.size = i, bootstrap.ts = "moving", by.end=FALSE)
      RF_moving_forecast_2 <- predict(RF_moving, data=Data2)$prediction
      RF_moving.mae_2 <-mae(Data2$X_t, RF_moving_forecast_2)
      RF_moving.rmse_2 <-rmse(Data2$X_t, RF_moving_forecast_2)
      RF_moving_forecast_2 <- NULL
      RF_moving <- NULL
      
      error_mae_RF_moving_2 <- c(error_mae_RF_moving_2, RF_moving.mae_2)
      error_rmse_RF_moving_2 <- c(error_rmse_RF_moving_2, RF_moving.rmse_2)
      
    }
    results <- list()
    results$bloc_seq <- block_length_seq
    results$list_pacf_arima <- list_pacf_residus_arima
    ####RMSE
    results$RF.rmse_2 <-RF.rmse_2
    results$RF_moving.rmse_2 <-error_rmse_RF_moving_2
    
    ####mae
    results$RF.mae_2 <-RF.mae_2
    results$RF_moving.mae_2 <-error_mae_RF_moving_2
    
    #nomF <- paste0("Result_friedman_m3_norm.d_",d, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".RDS")
    nomF <- paste0("Result_AR_season.phi_",phi, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".pacf_arima.RDS")
    #nomF <- paste0("Result_AR.phi_",phi, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".augmented_block_extra.RDS")
    #nomF <- paste0("Result_M1.d_",d, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".pacf_arima.RDS")
    
    saveRDS(results, file=paste0("~/Documents/", nomF))
    
    results <- NULL
    print(j)
  }
}



phi=0.3
nomF <- paste0("Result_AR_season.phi_",phi, ".n0_", 1000, "test_", 250, ".Monte_carlo_", 1, ".pacf_arima.RDS")
file <- readRDS(file=paste0("~/Documents/", nomF))
block_length_seq = file$bloc_seq
recup_pacf_residus_arima = c()
for (r in 1:nrow(list_pacf_residus_arima)){
  for (c in 1:ncol(list_pacf_residus_arima)){
    if( abs(list_pacf_residus_arima[r,c])>= 2*clim0){
      recup_pacf_residus_arima = c(recup_pacf_residus_arima, c)
    }
  }
}
recup_pacf_residus_arima = sort(unique(recup_pacf_residus_arima[recup_pacf_residus_arima > 1]))
index_ <- c()
for(e in recup_pacf_residus_arima){
  index_t = which(block_length_seq == e)
  index_ = c(index_, index_t)
}

RF_a <- NULL
RF_moving_a <- NULL
for(j in c(1:Monte_carlo_round)){
  nomF <- paste0("Result_M1.d_",100, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".pacf_arima.RDS")
  #nomF <- paste0("Result_AR_season.phi_",0.9, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".pacf_arima.RDS")
  file <- readRDS(file=paste0("~/Documents/", nomF))
  RF_a <- c(RF_a, file$RF.rmse_2)
  RF_moving_a <- rbind(RF_moving_a, file$RF_moving.rmse_2)
}

block_length_seq <- file$bloc_seq
error_block_dependent <- cbind(colMeans(RF_moving_a),rep(mean(RF_a), length(block_length_seq)))
data_error_block_dependent <- data.frame(Block_size = block_length_seq, rmse = c(error_block_dependent), 
                                         name = rep(c("Moving", "I.I.D"), each = length(block_length_seq)))

mycols_fill <- c(rgb(114,147,203, maxColorValue = 255), rgb(225,151,76, maxColorValue = 255), rgb(132,186,91, maxColorValue = 255))
mycols_line <- c(rgb(57,106,177, maxColorValue = 255), "black",rgb(218,124,48, maxColorValue = 255), rgb(62,150,81, maxColorValue = 255))

ggplot(data_error_block_dependent, aes(x=Block_size, y=rmse)) + geom_line(aes(colour = name),size=3) + 
  xlab("Block size") + ylab("RMSE") + ggtitle("d=100") +
  scale_colour_manual(values = mycols_line) +
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_continuous(breaks=pretty_breaks())+
  #geom_hline(yintercept=mean(RF_a), color="black", size=0.5)+
  labs(colour = "Variant")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=40,face="bold"),
        legend.title = element_text(size=20), legend.text = element_text(size=20),legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"))



#prendre directement pacf de la série pour ajuster
eq <- X_t ~.
Monte_carlo_round <- 500
mtry <- 2
ci = 0.95
a_t = c(1, 1, 1, 2, 3, 1, 1, 1, 1, 2, 4, 6)
for(d in c(10,50,100)){
  #for(d in c(10)){
  #for(phi in c(0.3,0.5,0.7,0.9,0.95)){
  #for(phi in c(0.3)){
  for(j in c(201:Monte_carlo_round)){
    #Dataset <- AR(1250,phi)
    #a_t = c(1, 1, 1, 2, 3, 1, 1, 1, 1, 2, 4, 6)
    #Dataset <- AR_season(1250,phi,a_t)
    Dataset <- M1(1250, d)
    #Dataset <- M2(1250,d)
    #plot(Dataset$X_t, type='l')
    #Data0 <- Dataset[c(1:250),]
    #Data2 <- Dataset[c(251:500),]
    Data0 <- Dataset[c(1:1000),]
    Data2 <- Dataset[c(1001:1250),]
    
    residus_arima <- auto.arima(Data0$X_t)$residuals
    list_pacf_residus_arima = pacf(residus_arima,lag=1000)$acf
    recup_pacf_residus_arima = c()
    clim0 <- qnorm((1 + ci)/2)/sqrt(1000)
    for (r in 1:length(list_pacf_residus_arima)){
      if( abs(list_pacf_residus_arima[r])>= sqrt(2)*clim0){
        recup_pacf_residus_arima = c(recup_pacf_residus_arima, r)
      }
    }
    recup_pacf_residus_arima = sort(unique(recup_pacf_residus_arima[recup_pacf_residus_arima > 1]))
    block_length_seq <-  recup_pacf_residus_arima
    
    
    error_rmse_RF_moving_2 <- c()
    error_mae_RF_moving_2 <- c()
    
    RF <- rangerts::ranger(eq, data=Data0,mtry=mtry)
    RF_forecast_2 <- predict(RF, data=Data2)$prediction
    RF <- NULL
    RF.mae_2 <-mae(Data2$X_t, RF_forecast_2)
    RF.rmse_2 <-rmse(Data2$X_t, RF_forecast_2)
    
    for(i in block_length_seq){
      RF_moving <- rangerts::ranger(eq, data=Data0,  mtry=mtry,block.size = i, bootstrap.ts = "moving", by.end=FALSE)
      RF_moving_forecast_2 <- predict(RF_moving, data=Data2)$prediction
      RF_moving.mae_2 <-mae(Data2$X_t, RF_moving_forecast_2)
      RF_moving.rmse_2 <-rmse(Data2$X_t, RF_moving_forecast_2)
      RF_moving_forecast_2 <- NULL
      RF_moving <- NULL
      
      error_mae_RF_moving_2 <- c(error_mae_RF_moving_2, RF_moving.mae_2)
      error_rmse_RF_moving_2 <- c(error_rmse_RF_moving_2, RF_moving.rmse_2)
      
    }
    results <- list()
    results$bloc_seq <- block_length_seq
    results$list_pacf_arima <- list_pacf_residus_arima
    ####RMSE
    results$RF.rmse_2 <-RF.rmse_2
    results$RF_moving.rmse_2 <-error_rmse_RF_moving_2
    
    ####mae
    results$RF.mae_2 <-RF.mae_2
    results$RF_moving.mae_2 <-error_mae_RF_moving_2
    
    #nomF <- paste0("Result_AR_season.phi_",phi, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".pacf_direct_arima.RDS")
    nomF <- paste0("Result_M1.d_",d, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".pacf_direct_arima.RDS")
    
    saveRDS(results, file=paste0("~/Documents/", nomF))
    
    results <- NULL
    print(j)
  }
}


RF_a <- NULL
RF_moving_a <- list()
block_seq_pacf <- list()
for(j in c(1:Monte_carlo_round)){
  nomF <- paste0("Result_M1.d_",10, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".pacf_direct_arima.RDS")
  #nomF <- paste0("Result_AR_season.phi_",0.95, ".n0_", 1000, "test_", 250, ".Monte_carlo_", j, ".pacf_direct_arima.RDS")
  file <- readRDS(file=paste0("~/Documents/", nomF))
  RF_a <- c(RF_a, file$RF.rmse_2)
  RF_moving_a[[j]] <- file$RF_moving.rmse_2
  block_seq_pacf[[j]] <- file$bloc_seq
}

RF_moving_list_block <- vector("list", max(unlist(block_seq_pacf)))
for(i in c(1:max(unlist(block_seq_pacf)))){
  for(j in c(1:Monte_carlo_round)){
    if(i %in% block_seq_pacf[[j]]){
      index_i <- which(block_seq_pacf[[j]]==i)
      RF_moving_list_block[[i]] = c(RF_moving_list_block[[i]], RF_moving_a[[j]][index_i])
    }
  }
}

mean_RF_moving_list_block <- lapply(RF_moving_list_block, mean)
number_obs_list = table(unlist(block_seq_pacf))
y_RF_block <- unlist(mean_RF_moving_list_block)[!is.na(unlist(mean_RF_moving_list_block))]
block_RF <- sort(unique(unlist(block_seq_pacf)))

error_block_dependent <- cbind(y_RF_block,rep(mean(RF_a), length(block_RF)))
data_error_block_dependent <- data.frame(Block_size = block_RF, rmse = c(error_block_dependent), 
                                         name = rep(c("Moving", "I.I.D"), each = length(block_RF)), Number_obs = c(number_obs_list, rep(Monte_carlo_round, length(block_RF))))

mycols_fill <- c(rgb(114,147,203, maxColorValue = 255), rgb(225,151,76, maxColorValue = 255), rgb(132,186,91, maxColorValue = 255))
mycols_line <- c(rgb(57,106,177, maxColorValue = 255), "black",rgb(218,124,48, maxColorValue = 255), rgb(62,150,81, maxColorValue = 255))

ggplot(data_error_block_dependent, aes(x=Block_size, y=rmse)) + geom_line(aes(colour = name),size=3) + 
  xlab("Block size") + ylab("RMSE") + ggtitle("d=") +
  scale_colour_manual(values = mycols_line) +
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_continuous(breaks=pretty_breaks())+
  #geom_hline(yintercept=mean(RF_a), color="black", size=0.5)+
  labs(colour = "Variant")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=40,face="bold"),
        legend.title = element_text(size=20), legend.text = element_text(size=20),legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"))

ggplot(data_error_block_dependent, aes(x=Block_size, y=rmse, color=Number_obs)) + geom_point(size=3)+scale_color_gradientn(colours = rainbow(3))
