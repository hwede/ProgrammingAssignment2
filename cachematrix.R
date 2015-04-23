## These functions implememt the calculation of the inverse of a square matrix
## by calculating this inverse only when it is called the first time
## and using the cached value when it is called again

## This function caches the matrix data, and sets up references
## to functions to get and set the matrix data and the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function first tries to use the cached inverse matrix,
## and when that is not available it calculates the inverse matrix
## and stores the result in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
