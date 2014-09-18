## Using makeVector and cachemean functions, I try to get the inverse of a special matrix
## Basically, I have two functions (makeCacheMatrix and cacheSolve) than creates a matrix and
## calculate the inverse

## This function can create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function can compute the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated cacheSolve function retrieves the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}