######################################################################################################
##
## This code file contains various R functions that help to find the inverse of the matrix efficiently
## by catching the inverse of matrix. This program corresponds to Programming Assignment 2 of 
## course 
## "R programming" 
##  by Dr.Roger D. Peng, Dr.Jeff Leek, Dr.Brian Caffo
## : https://class.coursera.org/rprog-007 
#######################################################################################################
## Usage : 
## 
## Step 1: call makeCacheMatrix function to create cacheInfrastructure for storing Matrix Inverse
##         Example: amatrix <- makeCacheMatrix(matrix(rnorm(9),nrow=3,ncol=3,byrow=TRUE))
## Step 2: call the solver function to fetch Matrix inverse from cache/ or Recompute if not exists 
##         in cache and cache result
##         Example: cacheSolve(amatrix)
#######################################################################################################
##        original source/Acknowledgements : Dr.R.D. Peng 
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
  ## initialize the cache with NULL value, m stores the inverse of matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ##   getInverse : gets matrix inverse from cache, return NULLL if not compted
  getInverse <- function() m
  ##   setInverse : updates the cache with newly computed matrix inverse
  setInverse <- function(s)  { 
    m <<- s 
  }
  # all the functions available in the environment. kind of creating an environment of functions
  list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)
}

#####################################################################################################
## cacheSolve - 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
## If inverse is not in the cache, then computes the inverse  and caches for next time use 
## Input  : None
## Output : Return a matrix that is the inverse of input Matrix 'x'
## Usage : cacheSolve(x)
#####################################################################################################

cacheSolve <- function (x,...) {
  data <- x$get()
  message("Input matrix:")
  print(data)
  m<- x$getInverse()
  if(is.null(m)) {
    message("Recomputing matrix Inverse:")
    m <- solve(data,...)
    x$setInverse(m)
  }
  else message("Fetching matrix Inverse from Cache:")
 
  m
}