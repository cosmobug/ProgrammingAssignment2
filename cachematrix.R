## Below are two functions that are used to create a special object that 
## stores a matrix and cache's its inverse. The first function, 
## makeCacheMatrix creates a special "matrix", which is really 
## a list containing a function to:
##    1.  set the value of the matrix
##    2.  get the value of the matrix
##    3.  set the value of the inverse
##    4.  get the value of the inverse
##
## The second function,  first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache via the setinv function.



## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }

  get <- function(){
    x
  }

  setinv <- function(solve){
    s <<- solve
  }
  
  getinv <- function(){
    s
  }
  
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinv()
  if (!is.null(s)) {
    ##Let user know using cached data
    message("Getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}
