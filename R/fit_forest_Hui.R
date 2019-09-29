fit_forest <- function(block.size, bootstrap.ts, activate.ts)
{
  param$block.size <- block.size
  param$bootstrap.ts <- bootstrap.ts
  param$activate.ts <- activate.ts
  rf_fit <- do.call(ranger, args=param, quote = FALSE, envir = parent.frame())
  rf_fit.forecast <- predict(rf_fit, data=DataSim1)$predictions
  mse_rf_fit <- mean((DataSim1$y-rf_fit.forecast)^2)
  return(mse_rf_fit)
}


fit_forest2 <- function(block.size, bootstrap.ts, seed, param, data.pred)
{
  param$block.size <- block.size
  param$bootstrap.ts <- bootstrap.ts
  param$seed <- seed
  param$oob.error <- T
  rf_fit <- do.call(ranger, args=param, quote = FALSE, envir = parent.frame())
  rf_fit.forecast <- predict(rf_fit, data=data.pred)$predictions
  mse_rf_forecast <- mean((data.pred$y-rf_fit.forecast)^2)
  
  mse_rf_fit <- mean((param$data$y-predict(rf_fit, data=param$data)$predictions)^2)
  mse_rf_fit_oob <- mean((param$data$y-rf_fit$predictions)^2)
  error_fit_oob <- rf_fit$prediction.error
  
  l <- list()
  l$mse_forecast <- mse_rf_forecast
  l$mse_fit <- mse_rf_fit
  l$mse_fit_oob <- mse_rf_fit_oob
  l$err_fit_oob <- error_fit_oob
  l$rf_fit <- rf_fit
  l$rf_fit.forecast <- rf_fit.forecast
  
  return(l)
}

