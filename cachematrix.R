## This pair of functions caches the inverse of the input matrix.



## The makeCacheMatrix function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}




## The cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
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
