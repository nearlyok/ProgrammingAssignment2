## Caching the Inverse of a Matrix.


## This function creates a special "matrix" object that can cache its inverse.
## The "matrix" object is really a list containing the functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(invm) inv <<- invm
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created with the
## above function. However, it first checks to see if the inverse of the matrix has already
## been calculated. If so, it gets the inverse value from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse matrix
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
