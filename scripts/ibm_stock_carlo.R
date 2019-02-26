rmse<-function(y, ychap, digits){
  return(round(sqrt(mean((y-ychap)^2,na.rm=TRUE)),digits=digits))
}
mape<-function(y,ychap, digits){
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=digits))
}
library(rangerts)
library(forecast)
dat <- read.csv2("data/ibm-common-stock-closing-prices.csv", header = TRUE, dec=".")
n <- nrow(dat)-1
Data <- dat[,2][1:(nrow(dat)-1)]
Data <- data.frame(Data, 1)
names(Data) <- c("Y", "Empty")
Data$Y <- as.numeric(Data$Y)
plot(Data$Y,type='l')
plot(dat[,2], type='l')
#past creation
#best past yet c(1:7) -> mtry = 3
past <- c(1:7)

for(i in past){
  name.i = paste0("Y_", i)
  if (i > 0){
    x = data.frame(c(Data[1:i, "Y"], Data[1:(n-i), "Y"]))
  }
  names(x) = name.i
  Data <- data.frame(Data, x)
}

Data <- Data[,-2]
#train and test settings
app_prop <- 0.8
percent_train <- 0.8

n0 <- floor(n*app_prop)
n_test <- floor(n*percent_train)
#t_deb <- sample(c(1:(n_test-n0)),1)
Data0 <- Data[c(1:n0),]
Data1 <- Data[c(n_test:n),]
#arima_auto <- auto.arima(Data0$Y)
#forest's parameters

mtry_tree <- c(1:7)
rf_error <- c()
for(j in c(1:1)){
  for(m in mtry_tree){
    RF <- rangerts::ranger(Y~.,  data=Data0, mtry=m)
    RF_forecast <- predict(RF, data=Data1)$prediction
    RF.rmse <-rmse(Data1$Y, RF_forecast,digits = 5)
    rf_error <- c(rf_error, RF.rmse)
    print(RF.rmse)
  }
  print(j)
}

which.min(rf_error)


Monte_carlo_round <- 10 
mtry_tree <- 3
block_length_seq <- seq(2,700,20)

for(j in c(1:Monte_carlo_round)){
  error_mtry_RF_circ <- NULL
  error_mtry_RF_stat <- NULL
  error_mtry_RF_moving <- NULL
  error_mtry_RF_nono <- NULL
  error_RF <- NULL
  for(m in mtry_tree){
    error_mape_RF_circ <- c()
    error_mape_RF_stat <- c()
    error_mape_RF_moving <- c()
    error_mape_RF_nono <- c()
    error_rmse_RF_circ <- c()
    error_rmse_RF_stat <- c()
    error_rmse_RF_moving <- c()
    error_rmse_RF_nono <- c()
    RF <- rangerts::ranger(Y~., data=Data0, mtry=m)
    RF_forecast <- predict(RF, data=Data1)$prediction
    RF.mape <-mape(Data1$Y, RF_forecast, digits=5)
    RF.rmse <-rmse(Data1$Y, RF_forecast, digits=5)
    error_RF <- c(error_RF, RF.mape)
    for(i in block_length_seq){
      #train
      RF_circ <- rangerts::ranger(Y~., data=Data0,   mtry=m, activate.ts = TRUE, block.size = i, bootstrap.ts = "circular")
      RF_stat <- rangerts::ranger(Y~., data=Data0, mtry=m,  activate.ts = TRUE, block.size = i, bootstrap.ts = "stationary")
      RF_moving <- rangerts::ranger(Y~., data=Data0,  mtry=m,   activate.ts = TRUE, block.size = i, bootstrap.ts = "moving")
      RF_nono <- rangerts::ranger(Y~., data=Data0,   mtry=m,  activate.ts = TRUE, block.size = i, bootstrap.ts = "nonoverlapping")
      #test
      RF_circ_forecast <- predict(RF_circ, data=Data1)$prediction
      RF_stat_forecast <- predict(RF_stat, data=Data1)$prediction
      RF_moving_forecast <- predict(RF_moving, data=Data1)$prediction
      RF_nono_forecast <- predict(RF_nono, data=Data1)$prediction
      
      #error
      RF_circ.mape <-mape(Data1$Y, RF_circ_forecast, digits=5)
      RF_stat.mape <-mape(Data1$Y, RF_stat_forecast, digits=5)
      RF_moving.mape <-mape(Data1$Y, RF_moving_forecast, digits=5)
      RF_nono.mape <-mape(Data1$Y, RF_nono_forecast, digits=5)
      error_mape_RF_circ <- c(error_mape_RF_circ, RF_circ.mape)
      error_mape_RF_stat <- c(error_mape_RF_stat, RF_stat.mape)
      error_mape_RF_moving <- c(error_mape_RF_moving, RF_moving.mape)
      error_mape_RF_nono <- c(error_mape_RF_nono, RF_nono.mape)
      
      RF_circ.rmse <-rmse(Data1$Y, RF_circ_forecast, digits=5)
      RF_stat.rmse <-rmse(Data1$Y, RF_stat_forecast, digits=5)
      RF_moving.rmse <-rmse(Data1$Y, RF_moving_forecast, digits=5)
      RF_nono.rmse <-rmse(Data1$Y, RF_nono_forecast, digits=5)
      error_rmse_RF_circ <- c(error_rmse_RF_circ, RF_circ.rmse)
      error_rmse_RF_stat <- c(error_rmse_RF_stat, RF_stat.rmse)
      error_rmse_RF_moving <- c(error_rmse_RF_moving, RF_moving.rmse)
      error_rmse_RF_nono <- c(error_rmse_RF_nono, RF_nono.rmse)
      
    }
    results <- list()
    results$p <- nrow(Data0)-1
    results$Index_monte_carlo <- i
    results$mtry <- m
    results$bloc_seq <- block_length_seq
    results$app_prop <- app_prop
    ####RMSE
    results$RF.rmse <-RF.rmse
    results$RF_circ.rmse <-error_rmse_RF_circ
    results$RF_stat.rmse <-error_rmse_RF_stat
    results$RF_moving.rmse <-error_rmse_RF_moving
    results$RF_nono.rmse <-error_rmse_RF_nono
    ####MAPE
    results$RF.mape <-RF.mape
    results$RF_circ.mape <-error_mape_RF_circ
    results$RF_stat.mape <-error_mape_RF_stat
    results$RF_moving.mape <-error_mape_RF_moving
    results$RF_nono.mape <-error_mape_RF_nono
    
    nomF <- paste0("Result.Monte_carlo_", j, "num.trees_", 500, ".mtry_", m, ".app_prop",app_prop, ".RDS")
    saveRDS(results, file=paste0("data/ibm_results/", nomF))
    print(m)
  }
  print(j)
}

#boxplot
RF_a <- c()
RF_circ_a <- c()
RF_stat_a <- c()
RF_moving_a <- c()
RF_nono_a <- c()
for(j in c(1:Monte_carlo_round)){
  nomF <- paste0("Result.Monte_carlo_", j, "num.trees_", 500, ".mtry_", 3, ".app_prop",app_prop, ".RDS")
  file <- readRDS(paste0("data/ibm_results/", nomF))
  RF_a <- c(RF_a, file$RF.rmse)
  RF_circ_a <- c(RF_circ_a, min(file$RF_circ.rmse))
  RF_stat_a <- c(RF_stat_a, min(file$RF_stat.rmse))
  RF_moving_a <- c(RF_moving_a, min(file$RF_moving.rmse))
  RF_nono_a <- c(RF_nono_a, min(file$RF_nono.rmse))
}


data_rf <- cbind(RF_a, RF_circ_a, RF_stat_a, RF_moving_a, RF_nono_a)
boxplot(data_rf)
