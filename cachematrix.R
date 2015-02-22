# makeCacheMatrix is a function that returns a list of functions
# It stores matrix / cached value of matrix & inverse
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   computes inverse of matrix
# * getInverse     output cached value
#
makeCacheMatrix <- function(x = numeric()) {
  
  # holds the cached value or NULL if 0 value
  # at start nothing is cached so set it to NULL
  cache <- NULL
  
  # retain matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    cache <<- NULL
  }
  # outputs stored matrix
  getMatrix <- function() {
    x
  }
  
  # compute/cache inverse of matrix
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # output cached value
  getInverse <- function() {
    cache
  }
  
  # return a list of each function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

# Calc the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # gives inverse
  inverse
}