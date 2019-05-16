library(rangerts)
library(survival)
context("bootstrap_ts")


test_that("Does not crash when variable named 'none'", {
  dat <- data.frame(y = rbinom(100, 1, .5),
                    x = rbinom(100, 1, .5),
                    none = rbinom(100, 1, .5))
  rf <- ranger(data = dat, dependent.variable.name = "y",
               bootstrap.ts = 'seasonal',
               block.size = 9,
               period = 5)
  expect_equal(rf$forest$independent.variable.names, c("x", "none"))
  expect_true(!is.na(rf$prediction.error))
  expect_true(all(!is.na(rf$predictions)))
})

test_that("importance = permutation", {
  data <- rangerts::elec_data

  # feature engineering
  data$Time2 <- data$Time^2
  data$TempTime <- data$Time*data$Temp
  data$TempChauf <- pmin(0, data$Temp - 15)
  data$TempChaufTime <- pmin(0, data$Temp - 15) * data$Time

  noel <- which(abs(data$Day - 24) <= 3 & data$Month == 12)
  consoNoel = vector("numeric", length(data$Time))
  consoNoel[noel] = 1
  data$consoNoel <- consoNoel
  data$MonthF <- as.factor(data$Month)

  # split train and test
  df_train <- data %>%
    dplyr::filter(Test == 0) %>%
    dplyr::select(- Test)

  df_test <- data %>%
    dplyr::filter(Test == 1) %>%
    dplyr::select(- Test)

  # set general parameters
  nb_trees <- 1000
  mtry <- floor(sqrt(ncol(df_train)))
  block_size <- 52
  rf_no_rep <- rangerts::ranger(Load ~ ., data = df_train,
                                num.trees = nb_trees,
                                mtry = mtry,
                                replace = T, # default = T too
                                seed = 1,
                                bootstrap.ts = "nonoverlapping",
                                block.size = block_size,
                                # importance = "permutation",
                                keep.inbag = T)
  expect_true(!is.null(rf_no_rep$variable.importance))
})
