set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = 2000, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500,parallel = F)
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)


knapsack_brute_force <- function(x, W, parallel = FALSE){
  
  
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
    
    listas_v <- parLapply(cl,1:nrow(x), fun =  function(y) { 
      temp_v <- combn(x$v, y,sum)
      temp_w <- combn(x$w, y,sum)
      temp_txt <- unlist(combn(rownames(x), 3, paste0, collapse = " "))
      
      
    })
    
    fix_list_w <- parLapply(cl,listas_w, colSums)
    
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
W <- 3500

hej <- as.numeric(intToBits(1:(2^nrow(x)))) * x$v
hejw <- as.numeric(intToBits(1:(2^nrow(x)))) * x$w


#hej <- round(hej,0)

hej2 <- tapply( hej, (seq_along(hej)-1) %/% 10, sum)
hej2w <- tapply( hejw, (seq_along(hejw)-1) %/% 10, sum)


which(hej2w < W & hej2w > 0)


round(hej2[hej2 > 15000 & hej2 <17000],0)


W <- 3500

z <- Sys.time()
knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500,parallel = F)
y <- Sys.time()
y-z

z <- Sys.time()
knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500,parallel = TRUE)
y <- Sys.time()
y-z


maxTid <- 15
tid <- c()
n <- 0
for(i in 5:maxTid){
  n <- n+1
  z <- Sys.time()
  knapsack_brute_force(x = knapsack_objects[1:i,], W = 3500,parallel = F)
  y <- Sys.time()
  tid[n] <- y-z
}





ts(c(1,5,2,6,3,67,3))

######### RÄTT


x[as.numeric(rownames(x)),]

sum(data[laistan[[3]][,1],]$v)



listas_txt <- lapply(1:nrow(x), FUN =  function(y)  combn(rownames(x), y))
listas_w <- lapply(1:nrow(x), FUN =  function(y)  combn(x$w, y))
listas_v <- lapply(1:nrow(x), FUN =  function(y)  combn(x$v, y))

#find combination of elements
sum_txt <- lapply(listas_txt, function(x){
  apply(x,2,paste, collapse = " ")
})
list_0_txt <- unlist(sum_txt)

sum_weights <- lapply(listas_w, function(x){
  apply(x,2,sum)
})
list_0_w <- unlist(sum_weights)

sum_value <- lapply(listas_v, function(x){
  apply(x,2,sum)
})
list_0_v <- round(unlist(sum_value),0)

#find maximum
maximum <- max(list_0_v[which(list_0_w < W)])

#find the maximum combination
element <- list_0_txt[which(list_0_w < W & list_0_v == maximum)]






lapply




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


    

paste(c("hej", "svej"),collapse = " ")

paste0(c("hej", "svej"),collapse = " ")



