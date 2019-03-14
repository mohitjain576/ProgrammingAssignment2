## the below function are to calculate the inverse of a matrix and
## store it as cache

## this fuction is to make cache matrix

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
       setInverse = setInverse,
       getInverse = getInverse)
}


## this function calculates the inverse and assigns cache value if already calculated

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data of inverse")
    return(inv)
  }
  mdata <- x$get()
  inv <- solve(mdata, ...)
  x$setInverse(inv)
  return(inv)
}
