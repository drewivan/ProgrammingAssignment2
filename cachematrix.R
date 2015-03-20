## functions necessary to compute the inverse of a matrix
## and cache the computation to save time if the function is called again

## this function will create a special matrix that caches its inverse
## call it with an invertible matrix, then call cacheSolve to solve it the first time

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## compute the inverse of a matrix; return the cached value if available or
## actually perform the computation if required
## the first time it is called, the inverse will be computed (no message will be displayed, just the result)
## subsequent times, the message "getting cached inverse" will be displayed before the answer
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  myMatrix <- x$get()
  inv <- solve(myMatrix)
  x$setinverse(inv)
  return(inv)
  
}

## return a sample matrix
sampleMatrix <- function() {
  m <- matrix(, 2, 2)
  m[1,1] <- 4
  m[1,2] <- 3
  m[2,1] <- 3
  m[2,2] <- 2
  return(as.matrix(m))
}