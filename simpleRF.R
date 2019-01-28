require(stats)
require(tseries)
source("/home/goehry/Documents/Info/randomForest/rf_package_perso/ForestRegression.R")
source("/home/goehry/Documents/Info/randomForest/rf_package_perso/Forest.R")

simpleRF <- function(formula, data, num_trees = 50, mtry = NULL, 
                     min_node_size = NULL, replace = TRUE, probability = FALSE, 
                     splitrule = NULL, unordered_factors = "ignore", 
                     num_threads = 1, block_length=0, sim="stationary") {
  
  model.data <- model.frame(formula, data)
  
  if (class(model.data[, 1]) == "factor") {
    if (probability) {
      treetype <- "Probability" 
    } else {
      treetype <- "Classification"
    }
  } else if (class(model.data[, 1]) == "numeric") {
    treetype <- "Regression"
  } else if (class(model.data[, 1]) == "Surv") {
    treetype <- "Survival"
  } else {
    stop("Unkown response type.")
  }
  
  ## Check parameters
  if (is.null(mtry)) {
    mtry <- sqrt(ncol(model.data)-1)
  } else if (mtry > ncol(model.data)-1) {
    stop("Mtry cannot be larger than number of independent variables.")
  }
  if (is.null(min_node_size)) {
    if (treetype == "Classification") {
      min_node_size <- 1
    } else if (treetype == "Probability") {
      min_node_size <- 10
    } else if (treetype == "Regression") {
      min_node_size <- 5
    } else if (treetype == "Survival") {
      min_node_size <- 3
    }
  }
  
  ## Splitrule
  if (is.null(splitrule)) {
    if (treetype == "Classification") {
      splitrule <- "Gini"
    } else if (treetype == "Probability") {
      splitrule <- "Gini"
    } else if (treetype == "Regression") {
      splitrule <- "Variance"
    } else if (treetype == "Survival") {
      splitrule <- "Logrank"
    }
  }
  
  ## Unordered factors
  if (!(unordered_factors %in% c("ignore", "order_once", "order_split", "partition"))) {
    stop("Unknown value for unordered_factors.")
  }
  covariate_levels <- list()
  
  if (unordered_factors == "order_once") {
    ## Reorder factor columns depending on response type
    model.data <- reorder.factor.columns(model.data)
    
    ## Save levels
    covariate_levels <- lapply(model.data[, -1], levels)
  }
  else if (unordered_factors == "ignore") {
    ## Just set to ordered if "ignore"
    character.idx <- sapply(model.data[, -1], is.character)
    ordered.idx <- sapply(model.data[, -1], is.ordered)
    factor.idx <- sapply(model.data[, -1], is.factor)
    recode.idx <- character.idx | (factor.idx & !ordered.idx)
    model.data[, -1][, recode.idx] <- lapply(model.data[, -1][, recode.idx], as.ordered)
    
    ## Save levels
    covariate_levels <- lapply(model.data[, -1], levels)
  }
  
  ## Create forest object
  # if (treetype == "Classification") {
  #   forest <- ForestClassification$new(num_trees = as.integer(num_trees), mtry = as.integer(mtry), 
  #                                      min_node_size = as.integer(min_node_size), 
  #                                      replace = replace, splitrule = splitrule,
  #                                      data = model.data,#Data$new(data = model.data), 
  #                                      formula = formula, unordered_factors = unordered_factors, 
  #                                      covariate_levels = covariate_levels,
  #                                      response_levels = levels(model.data[, 1]))
  # } else if (treetype == "Probability") {
  #   forest <- ForestProbability$new(num_trees = as.integer(num_trees), mtry = as.integer(mtry), 
  #                                   min_node_size = as.integer(min_node_size), 
  #                                   replace = replace, splitrule = splitrule,
  #                                   data = Data$new(data = model.data), 
  #                                   formula = formula, unordered_factors = unordered_factors,
  #                                   covariate_levels = covariate_levels,
  #                                   response_levels = levels(model.data[, 1]))
  # } else if (treetype == "Regression") {
    forest <- ForestRegression$new(num_trees = as.integer(num_trees), mtry = as.integer(mtry), 
                                   min_node_size = as.integer(min_node_size), 
                                   replace = replace, splitrule = splitrule,
                                   data = model.data,#Data$new(data = model.data), 
                                   formula = formula, unordered_factors = unordered_factors, 
                                   covariate_levels = covariate_levels,
                                   block_length = block_length,
                                   sim=sim)
  # } else if (treetype == "Survival") {
  #   idx.death <- model.data[, 1][, 2] == 1
  #   timepoints <- sort(unique(model.data[idx.death, 1][, 1]))
  #   forest <- ForestSurvival$new(num_trees = as.integer(num_trees), mtry = as.integer(mtry), 
  #                                min_node_size = as.integer(min_node_size), 
  #                                replace = replace, splitrule = splitrule,
  #                                data = Data$new(data = model.data), 
  #                                formula = formula, unordered_factors = unordered_factors, 
  #                                covariate_levels = covariate_levels,
  #                                timepoints = timepoints)
  # } else {
  #   stop("Unkown tree type.")
  # }
  
  ## Grow forest
  forest$grow(num_threads = num_threads)
  
  ## Return forest
  return(forest) 
}