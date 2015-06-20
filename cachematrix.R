## The following 2 functions can be used to cache the inverse of a matrix. It saves time
## by caching the inverse of the matrix first rather than compute it repeatedly.

## makeCacheMatrix creates a function that returns a list. The function do the following:
## (a) set the value of the matrix
## (b) get the value of the matrix
## (c) set the value of inverse of the matrix
## (d) get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversematrix <<- inverse
  getinverse <- function() inversematrix
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function returns the inverse of a matrix. It checks if the inverse has already been
## computed. If yes, it gets the result and skips the computation. Else, it computes the
## inverse of the matrix and sets the values in the cache via setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinverse()
  if (!is.null(inversematrix)) {
    message("getting cached matrix.")
    return(inversematrix)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inversematrix)
  inversematrix
}
