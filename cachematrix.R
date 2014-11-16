##R programming project 2 
##Solution sourced from https://github.com/edj-boston/coursera-r-programming/blob/master/programming-assignment-2/cachematrix.R

## Two functions to cache inverse of a matrix
## Initiated inversion matrix
makeCacheMatrix <- function( m = matrix() ) {

##Initialize
     i <- NULL
     
##Method to set the matrix
     set <- function( matrix ) {
         m <<- matrix
         i <<- NULL
     }
     
## Method to get and return the matrix
     get <- function() {
         m
     }
     
## Sets its inverse
     setInverse <- function(inverse) {
         i <<- inverse
     }
     
## Gets the inverse, returning the inverse property
     getInverse <- function() {
         i
    }
    
##Returns a list of methods
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
 }
 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.
 cacheSolve <- function(x, ...) {
 
 ##Return an inverse matrix
     m <- x$getInverse()
     
##Only returns if it is already set (not null)
     if(!is.null(m)) {
         message("getting cached data")
         return(m)
     }
     
## Get the matrix from the object
     data <- x$get()
     
## Matrix multiplication to get its inverse, which is set to the object and returned
     m <- solve(data) %*% data
     x$setInverse(m)
     m
 }
