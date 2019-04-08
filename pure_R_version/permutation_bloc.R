#out of bag, censé déjà être en forme de blocs
#a_n = block.size, longueur des blocs
#f
#importance pour la j-e variable de X
importance_dep_j <- function(oob, a_n){
  Y <- oob[,1]
  X <- oob[, -1]
  N_oob <- nrow(oob)
  number_of_blocs <- N_oob/a_n
  index_blocs <- c(1:number_of_blocs)
  bloc_permutation <- sample(index_blocs, size = length(index_blocs), replace = FALSE)
  new_index <- 1+c(0:(permutation_index-1))*a_n
  R_n <- 0
  R_n_permut <- 0
  for(i in c(1:N_oob)){
    R_n = R_n + (Y[i] - f(X[i]))^2
    R_n_permut = R_n_permut + (Y[i] - predict( cbind(X[i,-j], X[new_index[i],j])    ))^2
  }
  return(R_n/N_oob - R_n_permut/N_oob)
  
}