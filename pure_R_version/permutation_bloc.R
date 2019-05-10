#block oob -> transforme oob d'un échantillon en un oob qui respecte la taille des blocs donné en paramètre
#index[i] -> donne l'indice dans la série temporelle de base de i
#out of bag, censé déjà être en forme de blocs
#a_n = block.size, longueur des blocs
index <- function(i){
  #return(which(i in time_series))
  return(i)
}
block_oob <- function(oob, a_n){
  block_oob <- c()
  n_oob <- length(oob)
  start_new_obs <- c(1)
  end_new_obs <- c()
  length_temp <- 1
  for(i in c(2:n_oob)){
    index_temp <- index(oob[i-1])
    if(index(oob[i])!=index_temp+1 ){
        start_new_obs <-c(start_new_obs, oob[i])
        end_new_obs <- c(end_new_obs, index_temp)
        length_temp <- 1
    }else{
      length_temp <- length_temp+1
    }
  }
  end_new_obs <- c(end_new_obs,oob[n_oob])
  length_standard_oob <- 1 + end_new_obs - start_new_obs
  j<- 0
  for(l in length_standard_oob){
    j <- j+1
    if(l>= a_n && l<2*a_n){
      sample_length <- l - a_n
      u <- start_new_obs[j] + sample(c(0:sample_length), 1)
      block_oob <- c(block_oob, u+c(0:(a_n-1)))
    }else if(l>=2*a_n){
      number_of_blocks_temp <- floor(l/a_n)
      for(i in c(1:number_of_blocks_temp)){
        if(i==number_of_blocks_temp){
          sample_length <- l - number_of_blocks_temp*a_n
        }else{
          sample_length <- 0
        }
        u <- start_new_obs[j]+(i-1)*a_n + sample(c(0:sample_length), 1)
        block_oob <- c(block_oob, u+c(0:(a_n-1)))
      }
    }
    
  }
  return(block_oob)
}
oob <- c(1, 4, 5, 6,7, 8,9, 10, 11, 12, 13, 14, 18, 20,21,22, 35, 46, 50,51,52,53,54)
a_n <- 3
block_oob(oob,a_n)
oob <- c(1,3,5,6)
block_oob(oob,a_n)

##ensuite, comme oob_block respecte la taille des blocs, on peut faire la permutation par blocs de taille égale

#f
#importance pour la j-eme variable de X et pour un arbre
importance_dep_j <- function(oob, a_n,j){
  Y <- oob[,1]
  X <- oob[, -1]
  N_oob <- nrow(oob)
  number_of_blocs <- N_oob/a_n
  index_blocs <- c(1:number_of_blocs)
  bloc_permutation <- sample(index_blocs, size = length(index_blocs), replace = FALSE)
  new_index <- c()
  for(i in bloc_permutation){
    new_index <- c(new_index, c((a_n*(i-1)+1):(a_n*i)))
  }
  R_n <- 0
  R_n_permut <- 0
  for(i in c(1:N_oob)){
    R_n = R_n + (Y[i] - predict(X[i]))^2
    R_n_permut = R_n_permut + (Y[i] - predict( cbind(X[i,-j], X[new_index[i],j]) ))^2
  }
  return(R_n_permut/N_oob - R_n/N_oob)
  
}


a_n <- 4
oob <- matrix(runif(100), ncol=5)
Y <- oob[,1]
X <- oob[, -1]
N_oob <- nrow(oob)
number_of_blocs <- N_oob/a_n
index_blocs <- c(1:number_of_blocs)
bloc_permutation <- sample(index_blocs, size = length(index_blocs), replace = FALSE)
new_index <- c()
for(i in bloc_permutation){
  new_index <- c(new_index, c((a_n*(i-1)+1):(a_n*i)))
}




