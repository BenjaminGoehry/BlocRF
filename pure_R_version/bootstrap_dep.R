moving_block <- function(time_serie, alpha_n, l){
  n <- length(time_serie)
  B <- c() #initialisation
  k <- ceiling(alpha_n/l) #number of blocks
  for(i in 1:k){ #overlap allowed
    index <- sample(c(1:(n-l+1)),1)
    index_block <- c(index:(index+l-1))
    B <- c(B, index_block)
  }
  selected_B <- B[1:alpha_n]
  oob_B <- c(1:n)[-selected_B]
  return(list(inbag = time_serie[selected_B], oob = time_serie[oob_B]))
}

#d = periodicity
seasonal_block <- function(time_serie, alpha_n, l,d){
  n <- length(time_serie)
  B <- c() #initialisation
  k <- ceiling(alpha_n/l) #number of blocks
  for(i in 1:k){ #overlap allowed
    index <- sample(seq(1, n-l+1, by = d),1)
    index_block <- c(index:(index+l-1))
    B <- c(B, index_block)
  }
  selected_B <- B[1:alpha_n]
  oob_B <- c(1:n)[-selected_B]
  return(list(inbag = time_serie[selected_B], oob = time_serie[oob_B]))
}

non_overlag_mb <- function(time_serie, alpha_n, l){
  n <- length(time_serie)
  B <- c() #initialisation
  k <- ceiling(alpha_n/l) #number of blocks
  index_available <- c(1:(n-l+1))
  B <- split(c(1:n), rep(1:k, each=l))
  B <- unlist(B,use.names = FALSE)
  selected_B <- B[1:alpha_n]
  oob_B <- c(1:n)[-selected_B]
  return(list(inbag = time_serie[selected_B], oob = time_serie[oob_B]))
}

circular_mb <- function(time_serie, alpha_n, l){
  n <- length(time_serie)
  B <- c() #initialisation
  k <- ceiling(alpha_n/l) #number of blocks
  for(i in 1:k){ #overlap allowed
    index <- sample(c(1:n),1)
    if(index <= n-l+1){
      index_block <- c(index:(index+l-1))
    }
    else{
      index_block <- c(c(index:n),c(1:(l-length(c(index:n)))))
    }
    B <- c(B, index_block)
  }
  selected_B <- B[1:alpha_n]
  oob_B <- c(1:n)[-selected_B]
  return(list(inbag = time_serie[selected_B], oob = time_serie[oob_B]))
}

stationary_bootstrap <- function(time_serie,alpha_n, l, proba=1/l){
  n <- length(time_serie)
  B <- c() #initialisation
  k <- ceiling(alpha_n/l) #number of blocks
  for(j in 1:k){
    loc <- round(runif(1,1,n)) # loc for location
    for (i in 1:n){
      proba_test = runif(1,0,1)
      # In probability gprob, we take next observation, otherwise we start a new block
      if(proba_test>proba){
        loc = loc+1
      }else{
        loc <- round(runif(1,1,n))
        }
      if(loc>n){
        loc <- loc-n # wrap the serie as a circule
      }
      B <- c(B, loc)
    }
  }
  #ajout test que length(B) >= alpha_n
  selected_B <- B[1:alpha_n]
  oob_B <- c(1:n)[-selected_B]
  return(list(inbag = time_serie[selected_B], oob = time_serie[oob_B]))
}


#Test
time_series <- c(1:20)
alpha <- 15
l <- 3
#test_mb
mb <- moving_block(time_series, alpha, l)
print(mb)

#test seasonal
d <- 3
seq(1, n-l+1, by = d)
sb <- seasonal_block(time_series, alpha, l, d)
print(sb)
#stationary
stat_mb <- stationary_bootstrap(time_series, alpha, l)
print(stat_mb)

#circular
circ_mb <- circular_mb(time_series, alpha, l)
print(circ_mb)

#non overlap
non_ov <- non_overlag_mb(time_series, alpha, l)
print(non_ov)


