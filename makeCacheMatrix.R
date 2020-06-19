## This function creates a matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()){
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv  <<- NULL
  }
  
  get <- function()x
  setInverse <- function(inverse)inv <<- inverse
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

## This function computes the inverse of  matrix created by 
## makeCacheMatrix above. If the inverse is already calculated and not changed it will
## retrieve the inverse from the cache.


cacheSolve<- function(x, ...){
      inv <- x$getInverse()
      if(!is.null(inv)){
         message("getting cache data")
         return(inv)
        }
  
     mat<- x$get()
     inv<-solve(mat,...)
     x$setInverse(inv)
     inv
  
}