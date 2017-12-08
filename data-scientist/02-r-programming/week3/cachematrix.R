## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix:
#            input: x <- numeric square matrix
#           output: list of functions (set: set the matrix data; get: get the matrix data; setinverse: set the inverse of the matrix; getinverse: get the inverse of the matrix).

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(m_inverse) inv <<- m_inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve:
#       input: x <- matrix object created by the makeCacheMatrix function
#              ... <- additional parameters for the inverse function
#      output: inverse of the matrix in x
cacheSolve <- function(x, ...) {
  inv <- x$getinverse();
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get();
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}