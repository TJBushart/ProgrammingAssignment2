## These functions work together to either calculate or return the previously calculated
## inversion of a matrix

## makeCacheMatrix accepts a matrix value and can store its calculated inverse
## Pass it a matrix for intial setting. Use its set method to change the stored matrix.
## getInverse and setInverse are used by cacheSolve for caching


makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL

  set <- function(newMatrix) {
    x <<- newMatrix
    cachedInverse <<- NULL
  }
  get <- function() {
    return(x)
  }
  setInverse <- function(inverse) {
    cachedInverse <<- inverse
  }
  getInverse <- function() {
    return(cachedInverse)
  }

  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve takes a makeCacheMatrix argument and calculates its inverse or
## returns a previously calculated inverse for the same matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached inversion")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  return(inverse)
}


