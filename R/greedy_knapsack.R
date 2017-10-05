#'@title Greedy knapsack problem
#'@description Calulate with the greedy method how to get the maximum value out of optimal weight.
#'@param x must be a data frame with variables v (value) and w (weight)
#'@param W integer. Specify the maximum value for the knapsack. 
#'@export
#'@return Returns a list of the maximum value and which elemets used.
#'@examples 
#'knapsack_objects <-
#'data.frame(
#'  w=sample(1:4000, size = 2000, replace = TRUE),
#'  v=runif(n = 2000, 0, 10000))
#'  greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#'  greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)



greedy_knapsack <- function(x, W){
  
  stopifnot(is.data.frame(x) & is.numeric(W))
  
  if((sort(colnames(x))[1] == "v" & sort(colnames(x))[2] == "w" )==FALSE){
    stop("Could not find 'w' or 'v'")
  }
  
  val_per_w <- x$v / x$w
  x$val_per_w <- val_per_w
  #order the data
  data_greed_sort <- x[order(x$val_per_w,decreasing = TRUE),]
  
  
  summie <- data_greed_sort$w[1]
  n <-0
  txt <- c()
  #do the summs
  while(summie < W){
    n <- n+1
    summie <- sum(data_greed_sort$w[1:n]) 
    val <- sum(data_greed_sort$v[1:n])
    txt[n] <- rownames(data_greed_sort)[n]
    
    
  }
  
  ret_list <- list(value = round(val - data_greed_sort$v[n] ,0),
                   elements = as.numeric(txt[1:(n-1)]))
  
  return(ret_list)
  
}
