## makeCacheMatrix and cacheSolve functions calculates and stores inverse of a matrix. 
## The inverse of the matrix is stored till the makeCacheMatrix is called again.

## makeCacheMatrix takes matrix object as an argument and creates functions for the matrix.
## This output of makeCacheMatrix is a list of functions:
## set the matrix, get the matrix, setsolve (set the inverse), getsolve(get the already cached inverse)
## set function is not practically used by cacheSolve but it is essential to reset the values in makeCacheMatrix
## when new matrix comes in


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve function takes makeCacheMatrix output as an argument. It then calls inverse value from makeCacheMatrix.
## After that it checks if the inverse variable returned is null or has a value. 
## if inverse variable not null, cacheSolve is getting the already stored value from makeCacheMatrix
## otherwise, cacheSolve calculates the inverse of the matrix (which was passed in makeCacheMatrix as arugument)
## and returns the inverse value to makeCacheMatrix to store for future.
## For the first time when cacheSolve is called, cacheSolve will always calcualte the inverse of the matrix. 

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}