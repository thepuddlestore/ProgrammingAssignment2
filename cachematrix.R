## Overall Description: The first function, "makeCacheMatrix," creates a list of four functions
## and a cache variable, "inverse," for the inverse any invertible matrix. The second function,
## "cacheSolve," either a) retrieves and returns the inverted matrix from the "inverse" cache or 
## b) calculates the inverse, stores it to the "inverse" cache and returns it.


## Function Description: makeCacheMatrix() takes an argument "x" (which is assumed to be an 
## invertible matrix), initializes the cache variable, "inverse", to NULL and creates a list of four 
## functions: setmatrix() takes an argument "y" and assigns its value to "x", it also re-sets
## "inverse" to NULL in case it was holding an inverse for a different matrix. getmatrix() prints
## the value of the matrix "x". setsolve() assigns the cache "inverse" to the inverse of matrix "x".
## getsolve() prints the value of the cache "inverse". Finally, list() coerces each of those four 
## functions into the output for makeCacheMatrix(), a list.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setmatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        getmatrix <- function() x
        setsolve <- function() inverse <<- solve(x)
        getsolve <- function() inverse
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Function Overview: cacheSolve() computes the inverse of the matrix returned by the 
## makeCacheMatrix above by assigning cache variable "inverse" the value of x$getsolve().
## It checks if the inverse has already been calculated via !is.null(inverse); if it has, it 
## gives the message "getting cached data" and returns the cached "inverse". Otherwise, it assigns
## a new variable, "data", the value of the matrix via x$getmatrix(), assigns the "inverse" the
## inverted matrix. It then executes the setsolve() funt
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## This function is good to go

cacheSolve <- function(x, ...) {
        inverse <- x$getsolve()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$getmatrix()
        inverse <- solve(data, ...)
        x$setsolve <- function() {
                inverse <<- solve(data)
        }
        inverse
}