set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

intToBits(x$w)

### PARRALELL OCH VANLIG BLIR INTE SAMMA!!!!!
knapsack_brute_force <- function(x, W, parallel = FALSE){
  
  
  ##JAG TROR DET AR DENNA SOM GOR FEL!
  if(parallel == FALSE){
    vect2 <- c()
    test <- c()
    n <- 0
    for(i in 1:nrow(x)){
      for(j in 1:nrow(x)){
        n <- n+1
        test[n] <- x$w[i] + x$w[j]
        vect2[n] <- round(x[i,2] + x[j,2],0)
      }
    }
    matrWEI <-  matrix(test,nrow(x))
    matrVAL <- matrix(vect2,nrow(x))
    matrWEI2 <- matrWEI[lower.tri(matrWEI,diag = FALSE)]
    matrVAL2 <- matrVAL[lower.tri(matrVAL, diag = FALSE)]
    
    data <- data.frame(matrWEI2, matrVAL2)
    
    #hittar vilket fit som passar W
    W_fit <- data[which(data$matrWEI2<W),]
    r_name <- as.numeric(rownames(W_fit[which.max(W_fit$matr2Evolve),]))
    
    #hittar maximum
    maximum <- max(W_fit$matrVAL2)
    
    elemenT <- which(matrVAL == maximum, arr.ind = TRUE)[1,]
    
    
    list_ret <- list(value = maximum, elements = as.numeric(elemenT))
    
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
    
    r_name <- as.numeric(rownames(W_fit[which.max(W_fit$matr2Evolve),]))
    
    #hittar maximum
    maximum <- max(W_fit$matr2Evolve)
    
    elemenT <- which(matr2 == maximum, arr.ind = TRUE)[1,]
    
    list_ret <- list(value = maximum, elements = as.numeric(elemenT))
    
  }
  
  return(list_ret)
}


knapsack_brute_force(x = knapsack_objects[100:250,], W = 3500, parallel = T)
knapsack_brute_force(x = knapsack_objects[1:8,], W = 10000)
knapsack_brute_force(x = knapsack_objects[1:12,], W = 2000)









################

furp <- as.numeric(intToBits(x$w))
data.frame(furp, x$w, sort(rep(1:8,8)))

qout <- round(furp*x$v,0)

tapply( qout, (seq_along(qout)-1) %/% 10, sum)


(furp*x$v)[ furp> 0]

length((furp*x$w)[ furp> 0])





############# dynamic stuff
W <- 3500
m <- matrix(0,nrow = nrow(x),ncol = W+1)

for(i in 1:nrow(x)){
  m[i,] <- 0:W
}


m[2,]

for(i in 2:nrow(x)){
  for(j in 1:(W+1)){
    if(x$w[i] > j){
      m[i,j] <- m[i-1,j]
      
    } else {
      m[i,j] <- max(m[i-1, j], m[i-1, j-x$w[i]] + x$v[i])
    }
    
  }
}
m
which(m == max(m), arr.ind = TRUE)

max(m)


    






