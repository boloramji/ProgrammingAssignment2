######################################################################################################
##
## This code file contains various R functions that help to find the inverse of the matrix efficiently
## by catching the inverse of matrix
##
## Usage : 
## 
## Step 1: call makeCacheMatrix function to create cacheInfrastructure for storing Matrix Inverse
##         Example: amatrix <- makeCacheMatrix(matrix(rnorm(9),nrow=3,ncol=3,byrow=TRUE))
## Step 2: call the solver function to fetch Matrix inverse from cache/ or Recompute if not exists 
##         in cache and cache result
##         Example: amatrix$cacheSolve()
## Last modified: boloramji 9/15/2014
##
#######################################################################################################

## 
## makeCacheMatrix - 
## This function creates a special "matrix" object that can cache its inverse. cache storage for Matrix 
## Inverse is created
##    Input : NUmeric Square matrix ( n row by n columns). Should be nonsigular/invertible
##    output: Cache
#######################################################################################################

makeCacheMatrix  <- function(x =numeric()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y; 
    m <<- NULL
  }
  get <- function() x
  
  #####################################################################################################
  
  ## cacheSolve - 
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ## If the inverse has already been calculated 
  ## (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
  ## If inverse is not in the cache, then computes the inverse  and caches for next time use 
  ## Input  : None
  ## Output : Return a matrix that is the inverse of input Matrix 'x'
  ## Usage : x$cacheSolve()
  #####################################################################################################
  cacheSolve <- function (...) {
    if(is.null(m)) {
      message("Recaculating Inverse!")
      m <<- solve(x,...)
    }
    m
  }
  
  list(set = set, get = get, cacheSolve = cacheSolve)
}
