source("/home/goehry/Bureau/RUN/DeParty/functions.R")
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
#expert
past <- c(1:21)

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
t_deb <- sample(c(1:(n_test-n0)),1)
Data0 <- Data[c(t_deb:(t_deb+n0)),]
Data1 <- Data[n_test:n,]
arima_auto <- auto.arima(Data0$Y,max.q=50, max.p=50, max.P=10, max.Q = 10, max.order = 20, max.d = 10)
#forest's parameters
Monte_carlo_round <- 10 #
mtry_tree <- c(1:21)
block_length_seq <- seq(2,700,20)

rf_error <- c()
for(j in c(1:1)){
  for(m in mtry_tree){
    RF <- rangerts::ranger(Y~.,  data=Data0, mtry=m)
    RF_forecast <- predict(RF, data=Data1)$prediction
    #RF.mape <-mape(Data1$V97, RF_forecast, digits=5)
    RF.rmse <-rmse(Data1$Y, RF_forecast,digits = 5)
    rf_error <- c(rf_error, RF.rmse)
    print(RF.rmse)
  }
  print(j)
}

which.min(rf_error)

mtry_tree <- 14


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
    RF <- ranger(Y~., data=Data0, mtry=m)
    RF_forecast <- predict(RF, data=Data1)$prediction
    RF.mape <-mape(Data1$Y, RF_forecast, digits=5)
    RF.rmse <-rmse(Data1$Y, RF_forecast, digits=5)
    error_RF <- c(error_RF, RF.mape)
    for(i in block_length_seq){
      #train
      RF_circ <- ranger(Y~., data=Data0,   mtry=m, activate.ts = TRUE, block.size = i, bootstrap.ts = "circular")
      RF_stat <- ranger(Y~., data=Data0, mtry=m,  activate.ts = TRUE, block.size = i, bootstrap.ts = "stationary")
      RF_moving <- ranger(Y~., data=Data0,  mtry=m,   activate.ts = TRUE, block.size = i, bootstrap.ts = "moving")
      RF_nono <- ranger(Y~., data=Data0,   mtry=m,  activate.ts = TRUE, block.size = i, bootstrap.ts = "nonoverlapping")
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
    
    nomF <- paste0("Result.Monte_carlo_", j, "num.trees_", 500, ".mtry_", m, ".app_prop",app_prop, "_past_man_2.RDS")
    saveRDS(results, file=paste0("/home/goehry/Documents/Info/RF\ Time\ series/ibm_results/", nomF))
    print(m)
  }
  print(i)
}

RF_error_mape <- c()
RF_circ_error_mape <- c()
RF_stat_error_mape <- c()
RF_moving_error_mape <- c()
RF_nono_error_mape <- c()
for(m in mtry_tree){
  RF_temp <- c()
  RF_circ_temp <- c()
  RF_stat_temp <- c()
  RF_moving_temp <- c()
  RF_nono_temp <- c()
  for(j in c(1:Monte_carlo_round)){
    nomF <- paste0("Result.Monte_carlo_", j, "num.trees_", 500, ".mtry_", m, ".app_prop",app_prop, "_past_auto.RDS")
    file <- readRDS(paste0("/home/goehry/Documents/Info/RF\ Time\ series/ibm_results/", nomF))
    RF_temp <- c(RF_temp, file$RF.mape)
    RF_circ_temp <- rbind(RF_circ_temp, file$RF_circ.mape)
    RF_stat_temp <- rbind(RF_stat_temp, file$RF_stat.mape)
    RF_moving_temp <- rbind(RF_moving_temp, file$RF_moving.mape)
    RF_nono_temp <- rbind(RF_nono_temp, file$RF_nono.mape)
  }
  RF_error_mape <- cbind(RF_error_mape, RF_temp)
  RF_circ_error_mape <- cbind(RF_circ_error_mape, colMeans(RF_circ_temp))
  RF_stat_error_mape <- cbind(RF_stat_error_mape, colMeans(RF_stat_temp))
  RF_moving_error_mape <- cbind(RF_moving_error_mape, colMeans(RF_moving_temp))
  RF_nono_error_mape <- cbind(RF_nono_error_mape, colMeans(RF_nono_temp))
}

RF_mean <- colMeans(RF_error_mape)
RF_circ_best <- apply(RF_circ_error_mape, 2, min)
RF_stat_best <- apply(RF_stat_error_mape, 2, min)
RF_moving_best <- apply(RF_moving_error_mape, 2, min)
RF_nono_best <- apply(RF_nono_error_mape, 2, min)

RF_circ_whichbest <- apply(RF_circ_error_mape, 2, which.min)
RF_stat_whichbest <- apply(RF_stat_error_mape, 2, which.min)
RF_moving_whichbest <- apply(RF_moving_error_mape, 2, which.min)
RF_nono_whichbest <- apply(RF_nono_error_mape, 2, which.min)

plot(x=c(1:7),RF_mean, ylim = c(0.78,0.92))
lines(RF_circ_best, type = "p", col = "blue")
lines(RF_stat_best, type = "p", col="red")
lines(RF_moving_best, type = "p", col="pink")
lines(RF_nono_best, type = "p", col="green")


#faire boxplot pour mtry = 3, and best parameters
RF_a <- c()
RF_circ_a <- c()
RF_stat_a <- c()
RF_moving_a <- c()
RF_nono_a <- c()
for(j in c(1:Monte_carlo_round)){
  nomF <- paste0("Result.Monte_carlo_", j, "num.trees_", 500, ".mtry_", 14, ".app_prop",app_prop, "_past_man_2.RDS")
  file <- readRDS(paste0("/home/goehry/Documents/Info/RF\ Time\ series/ibm_results/", nomF))
  RF_a <- c(RF_a, file$RF.rmse)
  RF_circ_a <- c(RF_circ_a, min(file$RF_circ.rmse))
  RF_stat_a <- c(RF_stat_a, min(file$RF_stat.rmse))
  RF_moving_a <- c(RF_moving_a, min(file$RF_moving.rmse))
  RF_nono_a <- c(RF_nono_a, min(file$RF_nono.rmse))
}


data_rf <- cbind(RF_a, RF_circ_a, RF_stat_a, RF_moving_a, RF_nono_a)
boxplot(data_rf)


#mtry = 3, past = c(1:14) meilleur (0.79 - 0.85)