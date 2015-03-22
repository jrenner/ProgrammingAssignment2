## Put comments here that give an overall description of what your
## functions do

## create a matrix object capable of storing an inverse matrix value
## has member functions 'set', 'get', 'setInv' and 'getInv'
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(newMtx) {
      x <<- newMtx
      inv <<- NULL
  }
  get <- function() x
  setInv <- function(newInv) inv <<- newInv
  getInv <- function() inv
  list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## takes a matrix object created by makeCacheMatrix as an argument
## and returns the inverse of the stored matrix.
## if the inverse has been cached, it will return the cached value
## otherwise it will calculate the inverse, cache it, and return the value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    mtx <- x$get()
    if (!is.null(inv)) {
        message("returning cached data")    
        return(inv)
    }  
    inv <- solve(mtx)
    message("new inverse")
    x$setInv(inv)
    inv
}