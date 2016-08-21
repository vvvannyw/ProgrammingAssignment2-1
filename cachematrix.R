## The function is to cache the inverse of a matrix.

## We should make sure that the matrix is square first so that we can calculate
## the inverse of it.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}



## This function computes the inverse of the matrix we created before.
## If the inverse has already been calculated, then it should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- inverse(data, ...)
  x$setinv(inv)
  inv
}
