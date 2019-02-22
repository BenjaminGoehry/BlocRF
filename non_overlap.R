

#série temporelle de taille n, X_1, .. X_n
#alpha_n nombre de points sélectionnés
#l longueur d'un bloc
#overlap not allowed
non_overlap_function <- function(time_serie, alpha_n, l){
  n <- length(time_serie)
  B <- c() #initialisation
  k <- floor(n/l) #take k such that kl<=n
  index_possible <- c(1:(k-1))
  index_used <- c()
  M<- k-2
  for(i in c(1:M)){ 
    index_temp <- sample(index_possible, 1)
    begin <- index_temp*l
    end <- begin + l - 1
    B_temp <- c(begin:end)
    B <- c(B, B_temp)
    index_used <- c(index_used, index_temp)
    index_possible <- index_possible[!index_possible %in% index_used] #remove index used
    print(index_temp)
    print(index_possible)
  }
  selected_B <- B[1:alpha_n]
  oob_B <- c(1:n)[-selected_B]
  print(selected_B)
  print(oob_B)
  return(list(inbag = time_serie[selected_B], oob = time_serie[oob_B]))
}


time_serie <- c(1:30)
l=3
alpha_n =15

boot_ <- print(max(table(non_overlap_function(time_serie, alpha_n, l))))
