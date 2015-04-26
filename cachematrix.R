## Put comments here that give an overall description of what your
## functions do:
## - makeCacheMatrix: defines and several memory manipulation functions
##   for storing/getting data variables located in his environment
## - cacheSolve: computes the inverse and save it the memory environment 
##   the first time you calls it.
##   Next call times, it only returns the value stored from the 
##   cached memory environment
##   One issue is that both functions share the same environment

## Write a short comment describing this function:

## This function implements in the same way as on makeVector.R
## generate sets/gets for save/load in memory one matrix (x or m)
## and returns these four functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinvmtx <- function(inv) m <<- inv
    getinvmtx <- function() m
    list(set = set, get = get,
         setinvmtx = setinvmtx,
         getinvmtx = getinvmtx)  
}


## Write a short comment describing this function:

## Same of cachemean for numeric vector, cacheSolve
## checks if matrix hasn't been loaded before, and if so
## then calculate inverse an cache it in m, otherwise returns its value 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmtx()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # solve(): gets the inverse of a data matrix
  # assuming that always data is invertible
  m <- solve(data, ...)
  x$setinvmtx(m)
  m
  
}
