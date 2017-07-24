## Matrix inversion is a costly computation and there may be benefit to caching the inverse of a matrix rather than 
## computing it repeatedly.

## Below is the function makeCacheMatrix that will create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Below is the function cachesolve that will compute the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated then cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
}
