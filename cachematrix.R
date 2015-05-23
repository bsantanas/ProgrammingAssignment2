## cachematrix contains utility functions to optimize the calculation of
## a matrix inverse, storing this value in cache and therefore avoiding 
## the need to calculate it multiple times

## makeCacheMatrix creates a list object containing a matrix
## and the value of its inverse stored both in cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve Attempts to get the inverse of the supplied matrix from cache,
## if it doesn't exist, it calculates and stores the value

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  message("calculating inverse")
  inverse <- solve(data, ...)
  x$setinv(inverse)
  # Return a matrix that is the inverse of 'x'
  inverse
}

## testsolve iterates through both functions to appreciate the optimization
testsolve <- function(n = 10) {
  x <- matrix(runif(16,1,10),4,4)
  mymatrix <- makeCacheMatrix(x)
  for (i in 1:n) {
    cacheSolve(mymatrix)
  }
  cacheSolve(mymatrix)
}

