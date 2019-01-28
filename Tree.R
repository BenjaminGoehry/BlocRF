Tree <- setRefClass("Tree", 
                    fields = list(
                      mtry = "integer", 
                      min_node_size = "integer",
                      splitrule = "character",
                      unordered_factors = "character",
                      data = "data.frame", 
                      sampleIDs = "list",    
                      oob_sampleIDs = "integer",
                      child_nodeIDs = "list",      
                      split_varIDs = "integer",    
                      split_values = "numeric", 
                      split_levels_left = "list",
                      block_length = "numeric",
                      sim="character"),  
                    methods = list(
                      
                      grow = function(replace) {   
                        ## Bootstrap 
                        num_samples <- nrow(data)
                        if ( block_length > 0){
                          num_bootstrap_samples <- num_samples
                          bootstrap_sample <- tsbootstrap(data[,1], nb=1, statistic = NULL, m=1, b=block_length, type = sim)
                        }
                        if (replace && block_length == 0) {
                          num_bootstrap_samples <- num_samples
                          bootstrap_sample <- sample(num_samples, num_bootstrap_samples, replace = replace)
                        } else {
                          num_bootstrap_samples <- num_samples * 0.6321
                          bootstrap_sample <- sample(num_samples, num_bootstrap_samples, replace = replace)
                        }
                        
                        oob_sampleIDs <<- (1:num_samples)[-bootstrap_sample]
                        
                        ## Assign bootstrap samples to root node
                        sampleIDs <<- list(bootstrap_sample)
                        
                        ## Call recursive splitting function on root node
                        splitNode(1)
                      }, 
                      
                      splitNode = function(nodeID) {
                        ## Sample possible split variables
                        possible_split_varIDs <- sample(ncol(data)-1, mtry, replace = FALSE) + 1
                        
                        ## Split node
                        split <- splitNodeInternal(nodeID, possible_split_varIDs)
                        
                        if (!is.null(split)) {
                          ## Assign split
                          split_varIDs[[nodeID]] <<- split$varID
                          split_values[[nodeID]] <<- split$value
                          
                          ## Create child nodes
                          left_child <- length(sampleIDs) + 1
                          right_child <- length(sampleIDs) + 2
                          child_nodeIDs[[nodeID]] <<- c(left_child, right_child)
                          
                          ## For each sample in node, assign to left or right child
                          if (length(split_levels_left[[nodeID]]) == 0) {
                            ## Ordered splitting
                            idx <- data[sampleIDs[[nodeID]], split$varID] <= split$value
                          } else {
                            # Unordered splitting
                            idx <- data[sampleIDs[[nodeID]], split$varID] %in% split_levels_left[[nodeID]]
                          }
                          sampleIDs[[left_child]] <<- sampleIDs[[nodeID]][idx]
                          sampleIDs[[right_child]] <<- sampleIDs[[nodeID]][!idx]                    
                          
                          ## Recursively call split node on child nodes
                          splitNode(left_child)
                          splitNode(right_child)
                        } else {
                          ## Compute estimate for terminal node
                          split_values[[nodeID]] <<- estimate(nodeID)
                          split_varIDs[[nodeID]] <<- NA
                        }
                      },
                      
                      splitNodeInternal = function(nodeID, possible_split_varIDs) { 
                        ## Empty virtual function
                      },
                      
                      estimate = function(nodeID) {
                        ## Empty virtual function
                      }, 
                      
                      getNodePrediction = function(nodeID) {
                        ## Empty virtual function
                      },
                      
                      predict = function(predict_data) {
                        ## Initialize
                        num_samples_predict <- nrow(predict_data)
                        predictions <- list()
                        
                        ## For each sample start in root and drop down tree
                        for (i in 1:num_samples_predict) {
                          nodeID <- 1
                          while(TRUE) {
                            ## Break if terminal node
                            if (nodeID > length(child_nodeIDs) || is.null(child_nodeIDs[[nodeID]])) {
                              break
                            }
                            
                            ## Move to child
                            if (length(split_levels_left[[nodeID]]) == 0) {
                              ## Ordered splitting
                              value <- as.numeric(predict_data[i, split_varIDs[nodeID]])
                              if (value <= split_values[nodeID]) {
                                nodeID <- child_nodeIDs[[nodeID]][1]
                              } else {
                                nodeID <- child_nodeIDs[[nodeID]][2]
                              }
                            } else {
                              ## Unordered splitting
                              value <- predict_data[i, split_varIDs[nodeID]]
                              if (value %in% split_levels_left[[nodeID]]) {
                                nodeID <- child_nodeIDs[[nodeID]][1]
                              } else {
                                nodeID <- child_nodeIDs[[nodeID]][2]
                              }
                            }
                            
                          }
                          
                          ## Add to prediction
                          predictions[[i]] <- getNodePrediction(nodeID)
                        }
                        return(simplify2array(predictions))
                      }, 
                      
                      predictOOB = function() {
                        ## Initialize
                        num_samples_predict <- length(oob_sampleIDs)
                        predictions <- list()
                        
                        ## For each sample start in root and drop down tree
                        for (i in 1:num_samples_predict) {
                          nodeID <- 1
                          while(TRUE) {
                            ## Break if terminal node
                            if (nodeID > length(child_nodeIDs) || is.null(child_nodeIDs[[nodeID]])) {
                              break
                            }
                            
                            ## Move to child
                            if (length(split_levels_left[[nodeID]]) == 0) {
                              ## Ordered splitting
                              value <- as.numeric(data[oob_sampleIDs[i], split_varIDs[nodeID]])
                              if (value <= split_values[nodeID]) {
                                nodeID <- child_nodeIDs[[nodeID]][1]
                              } else {
                                nodeID <- child_nodeIDs[[nodeID]][2]
                              }
                            } else {
                              ## Unordered splitting
                              value <- data[oob_sampleIDs[i], split_varIDs[nodeID]]
                              if (value %in% split_levels_left[[nodeID]]) {
                                nodeID <- child_nodeIDs[[nodeID]][1]
                              } else {
                                nodeID <- child_nodeIDs[[nodeID]][2]
                              }
                            }
                            
                          }
                          
                          ## Add to prediction
                          predictions[[i]] <- getNodePrediction(nodeID)
                        }
                        return(simplify2array(predictions))
                      })
)