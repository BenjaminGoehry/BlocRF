source("/home/goehry/Documents/Info/randomForest/rf_package_perso/simpleRF.R")


n = 1000

epsilon <- rnorm(n,0,1)
x_1 <- runif(n,0,10)
x_2 <- runif(n,0, 2)
y <- x_1 + x_2 + epsilon



data <- as.data.frame(cbind(y, x_1, x_2))

test <- data[sample(c(1:n), 10),]

model <- simpleRF(y ~ ., data=data)
pred <- model$predict(test)

model2 <- simpleRF(y ~ ., data=data, probability_moving_bloc = 1/5)
pred2 <- model2$predict(test)
