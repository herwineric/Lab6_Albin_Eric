## ---- echo=FALSE, include=FALSE------------------------------------------

knapsack_objects <-
data.frame(
  w=sample(1:4000, size = 2000, replace = TRUE),
  v=runif(n = 2000, 0, 10000))
#Brute force


brute_force_knapsack <- function(x, W, parallel = FALSE){
  
  stopifnot(is.data.frame(x) | is.integer(W))
  
  if((sort(colnames(x))[1] == "v" & sort(colnames(x))[2] == "w" )==FALSE){
    stop("Could not find 'w' or 'v'")
  }
  
  #table(x$w > W)
  
  if(all(x$w > W)){
    message("The maximum weight is lower then any weight in the data frame")
  } else {
    
    
    if(parallel == FALSE){
      listas_txt <- lapply(1:nrow(x), FUN =  function(y) {
        combn(rownames(x), y, paste, collapse = " ")
        #apply(temp,2,paste, collapse = " ")
      })
      listas_w <- lapply(1:nrow(x), FUN =  function(y) { 
        combn(x$w, y, sum)
        #apply(temp,2,sum)
      })
      listas_v <- lapply(1:nrow(x), FUN =  function(y) { 
        combn(x$v, y,sum)
      })
      
      list_0_txt <- unlist(listas_txt)
      list_0_w <- unlist(listas_w)
      list_0_v <- round(unlist(listas_v),0)
      
      #find maximum
      maximum <- max(list_0_v[which(list_0_w < W)])
      
      #find the maximum combination
      element <- list_0_txt[which(list_0_w < W & list_0_v == maximum)]
      
      
      list_ret <- list(value = maximum, elements = as.numeric(strsplit(element, " ")[[1]]))
      
      
    } else {
      
      
      #x <<- x
      #CPU parallel
      require(parallel)
      requireNamespace("parallel")
      
      # Calculate the number of cores
      no_cores <- detectCores() - 1
      # Initiate cluster
      cl <- makeCluster(no_cores)
      
      
      #do the exact as non-parallel, but with parallel
      clusterExport(cl, c("x"),envir = environment())
      listas_txt <- parLapplyLB(cl, 1:nrow(x), fun =  function(y) {
        combn(rownames(x), y, paste0, collapse = " ")
        
      })
      listas_w <- parLapplyLB(cl, 1:nrow(x), fun =  function(y) {
        combn(x$w, y, sum)
        
      })
      listas_v <- parLapplyLB(cl,1:nrow(x), fun =  function(y) { 
        combn(x$v, y , sum)
        
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
    
  }

  
  return(list_ret)
}


#greedy knapsack


greedy_knapsack <- function(x, W){
  
  stopifnot(is.data.frame(x) | is.integer(W))
  
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



#dynamic knapsack


knapsack_dynamic <- function(x, W){
  
  stopifnot(is.data.frame(x) | is.integer(W))
  
  if((sort(colnames(x))[1] == "v" & sort(colnames(x))[2] == "w" )==FALSE){
    stop("Could not find 'w' or 'v'")
  }
  
  
  
  matr <- matrix(NA, ncol = W + 1, nrow = nrow(x) + 1)
  matr[1,] <- 0
  matr[,2] <- 0 
  
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





## ----echo=FALSE,include=FALSE--------------------------------------------
x <- Sys.time()
brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500,parallel = FALSE)
y <- Sys.time()
time <- y-x

## ------------------------------------------------------------------------
#brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500,parallel = FALSE)
time

## ----echo=FALSE,include=FALSE--------------------------------------------
x <- Sys.time()
knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500)
y <- Sys.time()
time <- y-x

## ------------------------------------------------------------------------
#knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500)
time

## ----echo=FALSE,include=FALSE--------------------------------------------
knapsack_objects <-
data.frame(
  w=sample(1:4000, size = 1000000, replace = TRUE),
  v=runif(n = 2000, 0, 10000))

x <- Sys.time()
lord <- lapply(seq(1001, 1000000, 1000) , function(z){
  greedy_knapsack(x = knapsack_objects[(z-1000)  :z,], W = 3500)
} )
y <- Sys.time()
time <- y-x

## ------------------------------------------------------------------------
#knapsack_dynamic(x = knapsack_objects[1:1000000,], W = 3500)
time

## ------------------------------------------------------------------------
#lineprof(hej <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500,parallel = FALSE))

## ------------------------------------------------------------------------
#lineprof(hej <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500,parallel = TRUE))

## ------------------------------------------------------------------------
#lineprof(hej <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))

## ------------------------------------------------------------------------
#lineprof(hej <- greedy_knapsack(x = knapsack_objects[1:800,], W = 3500))

