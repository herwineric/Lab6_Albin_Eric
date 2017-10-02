#'@title Longitude and Latitude of a address/destination
#'@description Return the longitude and latitude of a address/destination
#'@param x must be a data frame with variables v (value) and w (weight)
#'@param W integer. Specify the maximum value for the knapsack. 
#'@param parallel logical. Default is TRUE
#'@export
#'@return Returns a list of the maximum value and which elemets used.
#'@examples 
#'knapsack_objects <-
#'data.frame(
#'  w=sample(1:4000, size = 2000, replace = TRUE),
#'  v=runif(n = n, 0, 10000))
#'  knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500,parallel = FALSE)
#'  knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500,parallel = TRUE)





knapsack_brute_force <- function(x, W, parallel = FALSE){
  
  stopifnot(is.data.frame(x) | is.integer(W))
  
  if((sort(colnames(x))[1] == "v" & sort(colnames(x))[2] == "w" )==FALSE){
    stop("Could not find 'w' or 'v'")
  }
  
  
  if(parallel == FALSE){
    listas_txt <- lapply(1:nrow(x), FUN =  function(y) {
      temp <- combn(rownames(x), y)
      apply(temp,2,paste, collapse = " ")
    })
    listas_w <- lapply(1:nrow(x), FUN =  function(y) { 
      temp <-combn(x$w, y)
      apply(temp,2,sum)
    })
    listas_v <- lapply(1:nrow(x), FUN =  function(y) { 
      temp <-combn(x$v, y)
      apply(temp,2,sum)
    })
    
    list_0_txt <- unlist(listas_txt)
    list_0_w <- unlist(listas_w)
    list_0_v <- round(unlist(listas_v),0)
    
    #find maximum
    maximum <- max(list_0_v[which(list_0_w < W)])
    
    #find the maximum combination
    element <- list_0_txt[which(list_0_w < W & list_0_v == maximum)]
    
    
    list_ret <- list(value = maximum, elements = element)
    
  } else {
    
    
    x <<- x
    #CPU parallel
    library(parallel)
    # Calculate the number of cores
    no_cores <- detectCores() - 1
    # Initiate cluster
    cl <- makeCluster(no_cores)
    
    
    #do the exact as non-parallel, but with parallel
    clusterExport(cl, c("x"))
    listas_txt <- parLapply(cl, 1:nrow(x), fun =  function(y) {
      temp <- combn(rownames(x), y)
      apply(temp,2,paste,collapse = " ")
    })
    listas_w <- parLapply(cl, 1:nrow(x), fun =  function(y) {
      temp <- combn(x$w, y)
      apply(temp,2,sum)
    })
    listas_v <- parLapply(cl,1:nrow(x), fun =  function(y) { 
      temp <- combn(x$v, y)
      apply(temp, 2, sum)
    })
    stopCluster(cl)
    
    list_0_txt <- unlist(listas_txt)
    list_0_w <- unlist(listas_w)
    list_0_v <- round(unlist(listas_v),0)
    
    maximum <- max(list_0_v[which(list_0_w < W)])
    
    #find the maximum combination
    element <- list_0_txt[which(list_0_w < W & list_0_v == maximum)]
    
    list_ret <- list(value = maximum, elements = as.numeric(strsplit(element, " ")[[1]]))
    
  }
  
  return(list_ret)
}
