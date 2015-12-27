## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix is a function that returns a list of functions
# This function creates a special "matrix" object that can cache its inverse.
# Contains the following functions:
#  setMatrix      set the value of a matrix
#  getMatrix      get the value of a matrix
#  cacheInverse   get the cahced value (inverse of the matrix)
#  getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  inv <- NULL
  
  # store a matrix
  set <- function(y) {
    x <<- y
    # since the matrix is assigned a new value, flush the cache
    inv <<- NULL
  }
  
  # returns the stored matrix
  get <- function() x
  
  # cache the given argument 
  setInverse <- function(inverse) inv <<- inverse
  
  # get the cached value
  getInverse <- function() inv
  
  # return a list. Each named element of the list is a function
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## rejects NA values
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}