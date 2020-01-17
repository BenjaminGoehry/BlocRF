rmse<-function(y, ychap){
  return(round(sqrt(mean((y-ychap)^2,na.rm=TRUE)),digits=5))
}
mae<-function(y,ychap){
  eps <- y - ychap
  return(round(mean(abs(eps),na.rm=TRUE),digits=10))
}
mape<-function(y,ychap){
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=10))
}

fit_forest <- function(block.size, param, bootstrap.ts, data_test){
  param$block.size <- block.size
  param$bootstrap.ts <- bootstrap.ts
  #param$seed <- seed
  #param$oob.error <- T
  rf_fit <- do.call(ranger, args=param, quote = FALSE, envir = parent.frame())
  rf_fit.forecast <- predict(rf_fit, data=data_test)$predictions
  rmse_test <- rmse(data_test$y, rf_fit.forecast) #sqrt(mean((data_test$y-rf_fit.forecast)^2))
  rmse_oob <- rf_fit$prediction.error
  
  #mape_test <- mape(data_test$y, rf_fit.forecast)
  mae_test <- mae(data_test$y, rf_fit.forecast)
  
  #rmse_test_f <- rmse(data_test$f, rf_fit.forecast)
  #mape_test_f <- mape(data_test$f, rf_fit.forecast)
  #mae_test_f <- mae(data_test$f, rf_fit.forecast)
  
  
  l <- list()
  #l$rf_fit.forecast <- rf_fit.forecast
  l$rmse_oob <- rmse_oob
  
  l$rmse_test <- rmse_test
  #l$mape_test <- mape_test
  l$mae_test <- mae_test
  #l$rmse_test_f <- rmse_test_f
  #l$mape_test_f <- mape_test_f
  #l$mae_test_f <- mae_test_f
  
  return(l)
}

fit_forest_p <- function(block.size, param, bootstrap.ts, data_test){
  param$block.size <- block.size
  param$bootstrap.ts <- bootstrap.ts
  #param$seed <- seed
  #param$oob.error <- T
  rf_fit <- do.call(ranger, args=param, quote = FALSE, envir = parent.frame())
  rf_fit.forecast <- predict(rf_fit, data=data_test)$predictions
  rmse_test <- rmse(data_test$Load, rf_fit.forecast) #sqrt(mean((data_test$y-rf_fit.forecast)^2))
  rmse_oob <- rf_fit$prediction.error
  
  mape_test <- mape(data_test$Load, rf_fit.forecast)
  mae_test <- mae(data_test$Load, rf_fit.forecast)

  
  l <- list()
  l$inbag.counts <- rf_fit$inbag.counts
  l$rmse_test <- rmse_test
  l$mape_test <- mape_test
  l$mae_test <- mae_test
  
  return(l)
}