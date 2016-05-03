# Author: Fabian Leuthold                     Date: may, 3rd, 2016
#
# Programming assignment 2: Lexical Scoping
# -----------------------------------------------------------------------------------
# Assignment: Write the following functions:
#   1) makeCacheMatrix: 
#      This function creates a special "matrix" object that can cache its inverse.
#   2) cacheSolve: 
#      This function computes the inverse of the special "matrix" returned by 
#      makeCacheMatrix above. If the inverse has already been calculated (and 
#      the matrix has not changed), then the cachesolve should retrieve the 
#      inverse from the cache.

# makeCacheMatrix:
# decorate a given matrix with new functions to set/get the inverse
# as well as to set/get its own value
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# cacheSolve:
# for a given decorated matrix check, whether it's inverse 
# is set and return it, if so, otherwise calculate it and 
# store it on the matrix itself for later access.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}