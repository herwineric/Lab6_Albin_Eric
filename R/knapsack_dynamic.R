knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)


x


hej <- combn(x$w, 2,function(x) {
  temp <- sum(x)
  if(temp > W){
    temp <- NA
  }
  temp
})
sort(hej, TRUE)





for(i in 1:nrow(x)){
  
  
  
  
  
}