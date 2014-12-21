## usage: 
##rm(list = ls())
##x = rnorm(100,2,1) and 
##xx<-matrix(x,10,10) 
##defines some xx 10X10 matrix
## inverse is given by xinv<-cacheSolve(makeCacheMatrix(xx))

## this function transforms x into a list (or a recursive object that can be handled with $)
# in this environment it defines a list of functions setsolve, to set solve into cache
# getsolve gets the inverse from cache
# get gets the numerical values from the x
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL#x$get() #NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <<- function(solve) m <<- set(solve)
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
}
  datamat <-x$get()
  m<-solve(datamat,...)
  x$setsolve(m)
  m
}
