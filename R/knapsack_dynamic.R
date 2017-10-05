#'@title Dynamic knapsack method 
#'@description Calulate with the dynamic knapsack method how to get the maximum value out of optimal weight.
#'@param x must be a data frame with variables v (value) and w (weight)
#'@param W integer. Specify the maximum value for the knapsack. 
#'@export
#'@return Returns a list of the maximum value and which elemets used.
#'@examples 
#'knapsack_objects <-
#'data.frame(
#'  w=sample(1:4000, size = 2000, replace = TRUE),
#'  v=runif(n = 2000, 0, 10000))
#'  knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
#'  knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)



knapsack_dynamic <- function(x, W){
  
  stopifnot(is.data.frame(x) & is.numeric(W))
  
  if((sort(colnames(x))[1] == "v" & sort(colnames(x))[2] == "w" )==FALSE){
    stop("Could not find 'w' or 'v'")
  }
  
  
  
  matr <- matrix(NA, ncol = W + 1, nrow = nrow(x) + 1)
  matr[1,] <- 0
  matr[,1] <- 0 
  
  el_order <- order(x$w)
  
  wt <- x[order(x$w), 1]
  val <- x[order(x$w), 2]
  elements <- c()
  
  for (i in 1:(nrow(x) + 1)) {
    for (j in 1:(W + 1)) {
      if (i == 1 || j == 1) {
        matr[i, j] <- 0
      } else if (wt[i - 1] < j - 1 | wt[i - 1] == j - 1) {
        if(matr[i - 1, j - wt[i - 1]] == 0){
          tal <- 0
        } else {
          tal <- matr[i - 1, j - wt[i - 1]]
        }
        matr[i, j] <- max(val[i - 1] + tal,  matr[i - 1, j])
      } else{
        matr[i, j] <- matr[i-1, j]
      }
      
    }
  }
  
  
  #Colaberated with Milda
  i <- nrow(x) + 1
  j <- W + 1
  n <- 1
  
  while (i >= 2 && j >= 1) {
    if (matr[i, j] > matr[i - 1, j]) {
      elements[n] <- el_order[i - 1]
      n <- n + 1
      j <- j - wt[i - 1]
    }
    i <- i - 1
  }
  
  list_ret <- list(value = round(max(matr)), elements = sort(elements))
  return(list_ret)
}





