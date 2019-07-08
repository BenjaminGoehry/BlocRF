

blocksize <- 25

Nsim <- 100
n <- 200
n0 <- 100
t <- c(1:n)
sd <- 1/2
a <- 1/10
w <- 2*pi/20
delta <- w/10
cos05_t <- cos(0.5*w*t)
cos1_t <- cos(w*t)

f <- cos05_t + cos1_t  
t_20 <- rep(c(1:20),  length(t)/20)
formula <- y~t_20

mse_mov_fit <-NULL
mse_mov_forecast <-NULL
mse_iid_fit <-NULL
mse_iid_forecast <-NULL

for( i in c(1:Nsim))
{
  eps <- rnorm(n, 0, sd)
  y <- f+eps
  
  DataSim <- data.frame(y, t, cos05_t, cos1_t, f, t_20)
  DataSim0 <- DataSim[1:n0,]
  DataSim1 <- DataSim[(n0+1):n,]
  param$data <- DataSim0
  
  rf_moving <- rangerts::ranger(formula = formula, data=DataSim0, bootstrap.ts="moving", activate.ts=T, mtry=1, num.trees=100)
  rf_moving.fit <- predict(rf_moving, data=DataSim0)$prediction
  rf_moving.forecast <- predict(rf_moving, data=DataSim1)$prediction
  mse_mov_fit <- c(mse_mov_fit, mean((DataSim0$y - rf_moving.fit)^2))
  mse_mov_forecast <- c(mse_mov_forecast, mean((DataSim1$y - rf_moving.forecast)^2))
  
  rf_iid <- rangerts::ranger(formula = formula, data=DataSim0, bootstrap.ts="moving", activate.ts=F, mtry=1, num.trees=100)
  rf_iid.fit <- predict(rf_iid, data=DataSim0)$prediction
  rf_iid.forecast <- predict(rf_iid, data=DataSim1)$prediction
  mse_iid_fit <- c(mse_iid_fit,mean((DataSim0$y - rf_iid.fit)^2))
  mse_iid_forecast <- c(mse_iid_forecast, mean((DataSim1$y - rf_iid.forecast)^2))
}


mean(mse_iid_fit)
mean(mse_mov_fit)

mean(mse_iid_forecast)
mean(mse_mov_forecast)




