## These two functions (makeCachematrix and cacheSolve) cache the
## inverse of a matrix. As matrix inversion is usually a costly 
## computation and caching the inverse of a matrix may be some 
## benefit.

## makeCachematrix is a function that creates the inverse of 
##     the special "matrix object that can cache its inverse.

makeCachematrix <- function(x = matrix()){
  inve <- NULL
  set <- function(y){
    x <<- y
    inve <<- NULL
  }
  consi <- function() {x}
  setinverse <- function(inverse) {
    inve <<- inverse
  }
  getinverse <- function() {
    inve
  }
  list(set = set, consi = consi,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes de inverse of the special matrix returned
## by makeCachematrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should 
## retrieve the inverse from the cache. 

cacheSolve <- function(x, ...){
  inve <- x$getinverse()
  if(!is.null(inve)){
    message("getting cached data")
    return(inve)
  }
  data <- x$consi()
  inve <- solve(data, ...)
  x$setinverse(inve)
  inve
}
