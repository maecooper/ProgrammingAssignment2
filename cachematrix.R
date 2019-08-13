## A matrix is fed into makeCacheMatrix. cacheSolve will find the inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse.
## The output is a list of functions and the matrix, sometimes with a cached inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(im) inv <<- im
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Putting in the result of makeCacheMatrix as the parameter will either
##  result in calculating the inverse of the matrix given to makeCachMatrix or use the already cached inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
    if(!is.null(inv)) {
      message("using cached data")
      return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinv(inv)
    inv
}
