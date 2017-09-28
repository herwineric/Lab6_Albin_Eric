set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )


### PARRALELL OCH VANLIG BLIR INTE SAMMA!!!!!
knapsack_brute_force <- function(x, W,parallel = FALSE){
  
  
  ##JAG TROR DET AR DENNA SOM GOR FEL!
  if(parallel == FALSE){
    vect2 <- c()
    txt <- c()
    for(i in 1:nrow(x)){
      for(j in 1:nrow(x)){
        ett <- x$w[i] + x$w[j]
        if(ett > W | x$w[i] == x$w[j]) {
          next
        }
        
        #DENNA SKA VARA nrow(x)*nrow(x) lang
        vect2[i] <- round(x[i,2] + x[j,2],0)
        txt[i] <- paste(rownames(x[i,]), rownames(x[j,]))
      }
    }
    
    list_ret <- list(value = vect2[which.max(vect2)], elements = txt[which.max(vect2)])
    
  } else {
    
    a_w <- x$w
    b_w <<- x$w
    c_v <- x$v
    d_v <<- x$v
    
    #CPU parallel
    library(parallel)
    # Calculate the number of cores
    no_cores <- detectCores() - 1
    # Initiate cluster
    cl <- makeCluster(no_cores)
    
    
    
    clusterExport(cl, "b_w")
    matr1 <- parSapply(cl=cl, X = a_w, 
                       FUN = function(x){
                         sapply(X = b_w, FUN = function(y){ x+y })
                       }
    )
    matr1Evolve <- matr1[lower.tri(matr1,diag = FALSE)]
    
    
    clusterExport(cl, "d_v")
    matr2 <- parSapply(cl, X = c_v,
                       FUN = function(x){ 
                         sapply(X = d_v, FUN = function(y){ round(x+y,0)
                         })
                       }
    )
    matr2Evolve <- matr2[lower.tri(matr2,diag = FALSE)]
    stopCluster(cl)
    
    #data with all possible wights and values
    data <- data.frame(matr1Evolve, matr2Evolve)
    
    #hittar dem som passar W mattet
    W_fit <- data[which(data$matr1Evolve<W),]
    tail(W_fit)
    r_name <- as.numeric(rownames(W_fit[which.max(W_fit$matr2Evolve),]))
    
    #hittar maximum
    maximum <- max(W_fit$matr2Evolve)
    
    elemenT <- which(matr2 == maximum, arr.ind = TRUE)[1,]
    
    list_ret <- list(value = maximum, elements = as.numeric(elemenT))
    
  }
  
  return(list_ret)
}


knapsack_brute_force(x = knapsack_objects[1:100,], W = 3500, parallel = T)
knapsack_brute_force(x = knapsack_objects[1:8,], W = 2000)
knapsack_brute_force(x = knapsack_objects[1:12,], W = 2000)


