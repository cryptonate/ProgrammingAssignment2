##R programming project 2 
##Solution sourced from https://github.com/edj-boston/coursera-r-programming/blob/master/programming-assignment-2/cachematrix.R

## Two functions to cache inverse of a matrix
## Initiated inversion matrix
makeCacheMatrix <- function( matr = matrix() ) {

##Initialize and set the matrix
     inv <- NULL
     set <- function( matrix ) {
         matr <<- matrix
         inv <<- NULL
     }
     
## Method to get and return the matrix
     get <- function() {
         matr
     }
     
## Sets the matrix's inverse
     setInverse <- function(inverse) {
         inv <<- inverse
     }
     
## Gets the and returns the inverse and its property
     getInverse <- function() {
         inv
    }
    
##Returns a list of methods
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
 }
 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.
 cacheSolve <- function(x, ...) {
 
 ##Return an inverse matrix
     matr <- x$getInverse()
     
##Only returns if it is already set (not null)
     if(!is.null(matr)) {
         message("getting cached data")
         return(matr)
     }
     
## Get the matrix from the object
     data <- x$get()
     
## Matrix multiplication to get its inverse, which is set to the object and returned
     matr <- solve(data) %*% data
     x$setInverse(matr)
     matr
 }
