knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)




i=2
j=1

matr <- matrix(NA, ncol = W, nrow = nrow(x)+1)
matr[1,] <- 0
test_v <- c()
n <- 0
rownames(matr) <- c(NA, 1:nrow(x))
for(i in 2:(nrow(x)+1)){
  for(j in 1:W){
    
    if(x$w[i-1] <= j){
      
      if(j-x$w[i-1] == 0){
        plus_vikt <- 0
      } else {
        plus_vikt <-  matr[i-1, j-x$w[i-1]]
      }
      
      matr[i,j] <- max(x$v[i-1] + plus_vikt, matr[i-1,j])
      
    } else if(x$w[i-1] > j){
      
      matr[i,j] <- matr[i-1,j]

    }
    
  }
}
 
sort(as.numeric(test_v),TRUE)


######### måste bara få ut vilka element som maxxade den!

element_first <- which(max(matr) == matr,arr.ind = TRUE)
ett <- element_first[nrow(element_first),][1]
tva <- element_first[nrow(element_first),][2]

matr[ett-1,tva]

nrow(matr)-1

txt <- c()


for(i in 1:nrow(x)){
  if(matr[ett - 1 ,tva] == max(matr)){
    txt[1] <- rownames(matr)[ett-1]
  } else {
    back <- matr[ett,tva] - x$v[ett-1]
    matr[ett-1, which(matr[ett-1,] == back)]
    txt[2] <- rownames(matr)[ett-1]
  }
}



  
  
  
  
  
  
  
  
  
  
