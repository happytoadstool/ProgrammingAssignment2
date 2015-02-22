## Two functions (makeCacheMatrix and cacheSolve) that calculate the inverse of a matrix
## using the solve function, and store the result in a cached variable (m).

## makeCacheMatrix function defines a set of functions for interacting with a matrix.
## These functions (set, get, setinvers, getinverse) are then available to be called.
## setinverse is the function that caches the result, and getinverse returns the cached result.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a matrix 'x', checks if the inverse has already been cached and returns it if so
## if it has not already been cached, it calculates the inverse using solve and calls the setinverse function
## to cache the value 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
