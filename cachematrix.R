## Caching the Inverse of Matrix
## to caching the inverse of matrix is easy rather then to generated it every time
## stores a matrix and caches its inverse.

## makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse_c <- NULL
  set <- function(y) {
    x <<- y
    inverse_c <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse_r) inverse_c <<- inverse_r
  get_inverse <- function() inverse_c
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## cacheSolve computes the inverse of the special "matrix" created by makeCacheMatrix above. 
## if the inverse of matrix is already created then cacheSolve function is return from cache. 

cacheSolve <- function(x, ...) {
  inverse_c <- x$get_inverse()
  if (!is.null(inverse_c)) {
    message("getting cached data")
    return(inverse_c)
  }
  data_m <- x$get()
  inverse_c <- solve(data_m, ...)
  x$set_inverse(inverse_c)
  inverse_c
}
