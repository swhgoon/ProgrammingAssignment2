## Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly. 
## Sample:
## m <- makeCacheMatrix(x=matrix(1:4, nrow=2))
## cacheSolve(m) # compute once
## cacheSolve(m) # getting cached data
## Benchmark:
## >system.time(for(i in 1:10000) solve(m$get()))
## user  system elapsed 
## 0.294   0.000   0.291 
## > system.time(for(i in 1:10000) cacheSolve(m))
## user  system elapsed 
## 0.016   0.000   0.016

## This function creates a special “matrix” object(list) that can cache its inverse.
## original matrix: set()        get()
## inverse  matrix: setinverse() getinverse()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setinverse = setInverse,
       getinverse = getInverse)
}

## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
                     
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    # message("getting cached data") # for Debug only
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
