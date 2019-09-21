## These functions will create an object that stores a matrix and its inverse

## makeCacheMatrix creates an object that stores the matrix and a set of functions
## that store and access the matrix itself and its inverse, when the latter exists

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix) inverse <<- matrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve uses an object created from makeCacheMatrix and tries to access its
## cached inverse. If it doesn't exist, it creates the inverse and stores it in 
## the object

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
