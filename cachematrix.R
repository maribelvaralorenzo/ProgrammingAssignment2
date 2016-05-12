## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(Inverse) inverse <<- Inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#this function obtains the inverse of the matrix create with the makeCacheMatrix.
#if the inverse is already calculated the function returns the inverse from the cache
#and skips the computation. Otherwise, it calculates the inverse of the matrix
#and sets this value in the cache with the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("matrix is in memory")
    return(inverse)
  }
  inverse <- solve(x$get())
  
  x$setinverse(inverse)
  return(inverse)
}
