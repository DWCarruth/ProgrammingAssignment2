## This files contains functions to implement a 'cacheable'
## inverse matrix.
## makeCacheMatrix - creates the matrix
## cacheSolve - solves the inverse (retrieves from cache if available)

## makeCacheMatrix creates a special "matrix" 
## that is really a list containing functions to
## 1) set - set value of the matrix
## 2) get - get value of the matrix
## 3) setinv - set value of the inverse matrix
## 4) getinv - get value of the inverse matrix
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
  ## attempt to retrieve inverse matrix from cache
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if not in cache, get the matrix data
  data <- x$get()
  # calculate the inverse matrix
  inv <- solve(data, ...)
  # store in cache
  x$setinv(inv)
  # return the inverse
  inv
}
