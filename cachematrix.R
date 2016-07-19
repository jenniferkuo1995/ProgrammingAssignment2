#July 19, 2016
#Jennifer Kuo
#Programming Assignment 2 for Courera Introduction to R

# These two functions potentially reduce the time taken for computing the inverse of large matrices. 
# They will calculate and cach the inverse of a matrix, so that it can be looked up rather than 
# computed again when used repeatedly.

## makeCacheMatrix is a function which creates a special "matrix" object that can cache its inverse.
#this special matrix is actually a list containing a function that will set and get the value of the
#matrix, as well as set & get the value of the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, get = get, 
          setinv = setinv, 
          getinv = getinv)
     
}


##cacheSolve computes the inverse of the special matrix returned by the makeCacheMatrix function. 
#If the inverse has been calculated, than cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     
     if(!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     
     data <- x$get()
     inv <- solve(data)
     x$setinv(inv)
     inv
}

