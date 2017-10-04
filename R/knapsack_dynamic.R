

W=3500


x <- data.frame(w = c(1,3,4,5), v = c(1,4,5,7))

W <- 7






W=20

knapsack_dynamic <- function(x, W){
  
  
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


knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)




max(matr)







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
elements
  
  
  
  
  
  
  
  
