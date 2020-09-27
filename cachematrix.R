## makeCacheMatrix function creates a matrix that can cache its inverse.makeCacheMatrix contains get, set, setInverse, getInverse.
## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix mentioned above.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##initializing inverse as NULL
  set <- function(y) {
    x <<-y
    inv <<- NULL
  }
}
get <- function() {x} # function will obtain matrix x
setInverse <- function(inverse) {inv <<- inverse}
getInverse <- function(){inv}
list(set = set, get = get, 
     setInverse = setInverse,
     getInverse = getInverse)

# The following is to cache data
CacheSolve <- function(x, ...) ## This is to obtain inverse if the matrix
{
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data is cached data")
    return(inv) ##returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv ## return a matrix that is the inverse of x
}
