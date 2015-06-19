## The functions below cache the inverse of a matrix to reduce the computation time.

## The makeCacheMatrix function contains a list of functions for setting the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
    matrixinv <- NULL
    set <- function(y) {
      x <<- y
      matrixinv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) matrixinv<<- inverse
    getinv <- function() matrixinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }
  




## The cacheSolve function checks whether or not  the inverse of the matrix has been cached.
## If the inverse of matrix has been cached the function retrieves the cached value.
## If the inverse of matrix has not been cached the function calculates the inverse
## and sets the value into cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixinv <- x$getinv()
  if(!is.null(matrixinv)) {
    message("getting cached data")
    return(matrixinv)
  }
  data <- x$get()
  matrixinv <- solve(data, ...)
  x$setinv(matrixinv)
  matrixinv
}
