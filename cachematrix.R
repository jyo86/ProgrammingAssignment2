#############################################################
## Pair of functions that cache the inverse of a matrix.
## Sample usage:
##   myMatrix <- matrix(rnorm(4), nrow = 2) 
##   matrix <- makeCacheMatrix(myMatrix)
##   matrix$get()
##   inverseMatrix <- cacheSolve(matrix)
##   inverseMatrix
##   
#############################################################

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function (y) {
      x <<- y
      inv <<- NULL
  }
  
  get <- function () x
  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## It computates and returns the inverse matrix x 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
  
    if ( ! is.null(inv)){
        messges("Getting the cached inverse matrix.")
        return (inv)
    }
    
    data <- x$get()
    inv <- solve(data,...)
    x$setInverse(inv)
    inv
}
