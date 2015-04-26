## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" 
## that is really a list containing functions to
## set value of the matrix
## get value of the matrix
## set value of the inverse matrix
## get value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invM <<- inv
  getinv <- function() invM
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}

## calculates the inverse of the special "matrix" created
## with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
