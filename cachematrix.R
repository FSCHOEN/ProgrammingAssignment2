## This class can be used, to compute the inverse of a matrix and store it as cached data for further use.

## makeCachematrix: Creates a matrix object with a cacheable inverse.
## Input  - x: square invertible matrix
## Return - matrix-like Object, with cacheable inverse (see caheSolve)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve: Computes the inverse of the matrix or returns the cached value, if already computed.
## Input  - x: CacheMatrix to inverse
## Return - inverse matrix
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
