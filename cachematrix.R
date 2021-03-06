## The first function, makeCashMatrix, creates a special "matrix", 
## which is really a list containing a function to:
##   1. set the values of the matrix
##   2. get the values of the matrix
##   3. set the value of the inverse
##   4. get the value of the inverse
##
## The second function, cacheSolve, calculates the inverse of the special "matrix" 
## created with the first function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache 
## via the setinv function.



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinv <- function(solve) m <<- solve
     getinv <- function() m
     list(set = set, get = get, setinv = setinv, getinv = getinv)
     
}


## This function calculates the inverse of the special "matrix" created by
## the makeCacheMatrix function, or if the inverse has already been calculated,
## it gets and returns the value from the cache.

cacheSolve <- function(x, ...) {
     
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinv(m)
     m     
}
