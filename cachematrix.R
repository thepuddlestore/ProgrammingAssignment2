## Overall Description: The first function, "makeCacheMatrix," creates a list of four functions
## and a cache variable, "inverse," for the inverse any invertible matrix. The second function,
## "cacheSolve," either a) retrieves and returns the inverted matrix from the "inverse" cache or 
## b) calculates the inverse, stores it to the "inverse" cache and returns it.


## Function Description: makeCacheMatrix() takes an argument "x" (which is assumed to be a square 
## invertible matrix), initializes the cache variable, "inverse", to NULL and creates a list of four 
## functions: setmatrix() takes an argument "y" and super-assigns its value to "x", it also sets (or 
## re-sets) inverse" to NULL in case the value of "y" changed. getmatrix() prints the value of the 
## matrix "x". setsolve() takes an argument, "matrix", and super-assigns the cache, "inverse", to the 
## inverse of matrix "x". getsolve() prints the value of the cache "inverse". Finally, list() coerces 
## each of those four functions into the output for makeCacheMatrix(), a list.

makeCacheMatrix <- function(x = matrix()) {   ## makeCacheMatrix() takes an argument, "x", which will be coerced into a matrix
        inverse <- NULL                       ## "inverse" is initiated and assigned NULL
        setmatrix <- function(y) {  
                x <<- y                       ## setmatrix() takes an argument, "y", and super-assigns it to "x"
                inverse <<- NULL              ## it also assigns (or re-assigns) "inverse" to be NULL
        }
        getmatrix <- function() x             ## getmatrix() prints the value of "x" (a square, invertible matrix)
        setsolve <- function(matrix) inverse <<- solve(x)       ## setsolve() takes an argument, "matrix", and assigns its inverse value to "inverse"
        getsolve <- function() inverse                          ## getsolve() prints the newly assigned value of "inverse"
        list(setmatrix = setmatrix, getmatrix = getmatrix,      ## finally, list coerces the four functions into the output for makeCacheMatrix()
             setsolve = setsolve,
             getsolve = getsolve)
}

## Function Overview: cacheSolve() computes the inverse of the matrix returned by the function
## "makeCacheMatrix" above by assigning cache variable, "inverse", the value of x$getsolve() which
## is class "matrix". It then checks if the inverse has already been calculated via !is.null(inverse); 
## if it has, it gives the message "getting cached data" and returns the cached value of "inverse". 
## Otherwise, it assigns a new variable, "data", the value of x$getmatrix(), and assigns "inverse" the
## inverted matrix via x$setsolve(data). Finally, it returns the value of "inverse". 

cacheSolve <- function(x, ...) {              ## cacheSolve() takes an argument, "x" (whose value is the contents of makeCacheMatrix())
  inverse <- x$getsolve()                     ## and assigns "inverse" the value of x$getsolve() (which may contain the inverted matrix or may be NULL)
  if(!is.null(inverse)) {                     ## if the value of "inverse" is not NULL
    message("getting cached data")            ## "getting cached data" is printed 
    return(inverse)                           ## and the value of "inverse" is returned
  }
  data <- x$getmatrix()                       ## otherwise, the value of x$getmatrix() (a square, invertible matrix) is assigned to a placeholder variable, "data"
  inverse <- x$setsolve(data)                 ## x$setsolve() is then called on "data" to assign assigns its inverse value to "inverse"
  inverse                                     ## finally, the value of "inverse" is printed
}