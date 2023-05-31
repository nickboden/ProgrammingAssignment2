## These functions cache the inverse of a matrix.
## I created them for the Week 3 Coursera Assignment on R programming.
##As someone with no coding background, this took a lot of trial and error. 


##The instructions were as follows: 

##     Write the following functions:

## A ~ makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## B ~ cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##     If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## C ~ Computing the inverse of a square matrix can be done with the solve function in R. 
##     For example, if X is a square invertible matrix, then solve(X) returns its inverse.
##     For this assignment, assume that the matrix supplied is always invertible.

## This first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) z <<- inverse
        getinverse <- function() z
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getinverse()
        if(!is.null(z)) {
                message ("getting cached data")
                return(z)
        }
        data <- x$getinverse()
        z <- inverse(data, ...)
        z$setinverse(z)
        z
}