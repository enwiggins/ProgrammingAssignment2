## Catching the Inverse of a Matrix

## function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  set <- function(y) { 
    x <<- y            ##set value of matrix   
    inv <<- NULL       ##clear the cache
  }
  get <- function() x  ## function gets value of matrix
  setInverse <- function(inverse) inv <<- inverse  ##sets inverse when no cache inverse
  getInverse <- function() inv    ##function to get inverse
  list(set = set, get = get,  ##returns list of above four functions
       setInverse = setInverse,
       getInverse = getInverse)
}

## function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## if inverse already calculated and matrix has not changed, cachesolve retrieves inverse
## from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()  ##gets cache value of inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##If cache empty complete the following steps
  data <- x$get()  ##get matrix
  inv <- solve(data, ...)  ##calculate inverse
  x$setInverse(inv)  ##cache result
  inv
}

