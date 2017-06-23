## This function caches the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inv
      getinv <- function() inv
      list(set = set, get = get, 
           setinv = setinv, 
           getinv = getinv)
}


## cacheSolve computes the inverse of matrix created by makeCacheMatrix. If the inverse
## has already been calculated and is unchanged, then it gets the invervse from the cache

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      mtx <- x$get()
      inv <- solve(mtx, ...)
      x$setinv(inv)
      inv
}
