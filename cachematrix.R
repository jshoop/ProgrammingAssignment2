## These functions provide the ability to cache the inverse of a 
## square invertible matrix for efficiency for future retrieval.

## makeCacheMatrix creates the list of functions to get or set 
## a cached version of the inverse of the supplied matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the "cache" variable to NULL
  cache <- NULL
  
  ## Create the function to "set" the matrix
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  ## Create the function to get the value of the matrix
  get <- function() x
  
  ## Create the function to cache the inverted matrix
  setInverse <- function(y) cache <<- y
  
  ## Create the function to get the cached inverted matrix
  getInverse <- function(y) cache
  
  ## return the list of functions
  list( set = set, get = get, 
        setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inverse of a matrix and assumes that the 
## matrix is in fact invertible. For efficiency, cacheSolve attempts 
## to use a previously cached inverse of the supplied matrix. 
## If the cache has not yet been created, the inverse is calculated
## and cached for future use.

cacheSolve <- function(x, ...) {
  ## Attempt to get the cached inversted matrix
  invertedMatrix <- x$getInverse()
  
  ## If the inverse of the matrix has not been cached,
  ## invert it and cache it
  if(is.null(invertedMatrix)) {
    
    message("Cache unavailable. Inverting matrix and caching.")
    
    myMatrix <- x$get() ## Get the matrix data
    invertedMatrix <- solve(myMatrix, ...) ## Invert the matrix
    x$setInverse(invertedMatrix) ## Cache the inverted matrix
  }
    else message("Using previously cached inverted matrix.")

  ## Return a matrix that is the inverse of 'x'
  invertedMatrix
}
