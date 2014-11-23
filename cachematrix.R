##R programming project 2 
##Solution sourced primarily from https://github.com/edj-boston/coursera-r-programming/blob/master/programming-assignment-2/cachematrix.R

## Two functions to cache inverse of a matrix
## Initiated inversion matrix
makeCacheMatrix <- function(matr = matrix()) {

##Initialize and set the matrix
     inv <- NULL
     setmethod <- function(matrix) {
         matr <<- matrix
         inv <<- NULL
     }
     
## Method to get and return the matrix
     getmethod <- function() {
         matr
     }
     
## Sets the matrix's inverse
     setInv <- function(inverse) {
         inv <<- inverse
     }
     
## Gets the and returns the inverse and its property
     getInv <- function() {
         inv
    }
    
##Returns a list of methods
     list(getmethod = getmethod, setmethod = setmethod, getInv = getInv, setInv = setInv)
 }
 
## This function computes the inverse of the list of methods ("matrix") returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache with the line "if(!is.null(matr))".
 cacheSolve <- function(x, ...) {
 
 ##Return an inverse matrix
     matr <- matr$getInv()
     
##Only returns if it is already set (not null)
     if(!is.null(matr)) {
         message("getting cached data")
         return(matr)
     }
     
## Get the matrix from the object
     input <- matr$get()
     
## Matrix multiplication to get its inverse, which is set to the object and returned
     matr <- solve(input) %*% input
     matr$setInv(matr)
     matr
 }
