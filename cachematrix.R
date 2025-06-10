## Put comments here that give an overall description of what your
## functions do
##
## These functions work together to cache the inverse of a matrix.
## makeCacheMatrix creates a special "matrix" object that can store
## the matrix and cache its inverse.
## cacheSolve computes the inverse of the matrix stored in the special
## object returned by makeCacheMatrix. If the inverse is already cached,
## it retrieves it instead of recomputing.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # initialize inverse cache as NULL
  
  # set a new matrix and reset cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the matrix
  get <- function() x
  
  # set the inverse cache
  setinverse <- function(inverse) inv <<- inverse
  
  # get the inverse cache
  getinverse <- function() inv
  
  # return a list of all the above functions to interact with this object
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" object.
## If the inverse is already cached, it returns the cached value.
## Otherwise, it computes the inverse, caches it, and returns it.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # try to get cached inverse
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()  # get the matrix
  inv <- solve(mat, ...)  # compute inverse
  x$setinverse(inv)  # cache inverse
  
  inv  # return inverse
}


