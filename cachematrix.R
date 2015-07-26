## These functions allow for a set of functions to be defined specific to
## the supplied matrix. The first time the inverse of the matrix is requested
## the result is supplied but also cached. Subsequent requests for the inverse
## are satisfied from the cache and thus better performance can be provided 
## for large matrices
## 
## Example of usage is:
##  p <- matrix(4,7,,2,6,nrow=2,ncol=2)
##  ee <- makeCachematrix(p)
##  q <- cacheSolve(ee)
##  q2 <- cacheSolve(ee)

## input matrix needs to be invertable or error results.

## created by Ian Dent, 26-July-2015

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## functions are created as follows:
# set: resets to starting point
# get: retrieves the original matrix
# setinverse: stores the inverse matrix
# getinverse: retrieves the inverse matrix.

makeCacheMatrix <- function(x) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

} # end of makeCacheMatrix function

## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
} # end of cacheSolve function
