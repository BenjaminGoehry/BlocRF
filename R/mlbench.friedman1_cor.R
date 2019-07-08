mlbench.friedman1_cor <- function (n, sd = 1, model=list(ar = 0.5), coef.trend=0) 
{
  x <- matrix(runif(10 * n), ncol = 10)
  y <- 10 * sin(pi * x[, 1] * x[, 2])
  y <- y + 20 * (x[, 3] - 0.5)^2 + 10 * x[, 4] + 5 * x[, 5]
  
  
  if (sd > 0) {
    eps <- arima.sim(n = n, model=model, sd = sd)
    y <- y + eps
  }
  
  if(coef.trend!=0)
  {
    y <- y+coef.trend*c(1:n)
  }
  
  list(x = x, y = y)
}


# sd <- 1
# model <- list(ma = 2*c(1:10))
# 
# x <- matrix(runif(10 * n), ncol = 10)
# y <- 10 * sin(pi * x[, 1] * x[, 2])
# y <- y + 20 * (x[, 3] - 0.5)^2 + 10 * x[, 4] + 5 * x[, 5]
# y_nn <- y
# 
# if (sd > 0) {
#   eps <- arima.sim(n = n, model=model, sd = sd)
#   y <- y + eps
# }
# 
# mean(y_nn^2)/var(eps)
# 
# acf(eps)
# acf(y)
# 

#arima.sim(n = n, model=list(), sd=1)

#20^2/(1-0.9^2)



