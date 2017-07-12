vecv <- c(103,82,179)
primecount <- function(x) {
    k <- 0 # assign 0 to k
    for (n in x) {
    k <- n/2
    j<- 2
    count <- 0
        while (j<= k){
      if(n%%j== 0){
      count<- count+1
      
  break}
          j <-j+1
    }
    if(count== 0)
    print("prime")
    else print("not prime")
       }
}
primecount(vecv)

##################################################


sumofcube <- function(x) {
  if (x > 0) {
    k <- 1
    cube <- 0
    while(k<= x){
      cube <- cube +(k^3)
      k <-k +1
    } 
    print(cube)
    
  }
}

##################################################

vecp <- c(2,2,3,3,4,5,7,11,15,19,24,29) 
  
primecot <- function(x) {
  k <- 0 # assign 0 to k
  for (n in x) {
    k <- n/2
    j<- 2
    count <- 0
    
    while (j<= k){
      if(n%%j== 0){
        count<- count+1
        
        break}
      
      j <-j+1
    }
    if(count== 0)
      return(1)
    else break
    
  }
}

prime.fn <- function(x) {
  countp <- 0
  for(n in x) {
    
    if( is.numeric(primecot(n) ))
      countp <- countp+1
    else next
    
  } 
  if(countp ==0)
    print("non primes")
  else
    print (countp)
}

prime.fn(vecp)
########################################################


ModeN = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

ModeN(vecp)

#######################################################
