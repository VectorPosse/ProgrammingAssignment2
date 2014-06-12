## These functions will cache a matrix inverse
## and allow one to use its value without having to
## recompute it.

## makeCacheMatrix takes a matrix as input and creates a list
## of four functions:
##
## $set will store the input matrix
## $get will return the value of a previously stored matrix.
## $setinv will store the inverse matrix.
## $getinv will return the value of the stored inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve takes as its input a list of four functions as
## created by the makeCacheMatrix function. If there is already 
## a stored inverse matrix, then cacheSolve will simply return it.
## If there is no stored inverse matrix, this function will calculate
## the inverse, store it in the cache, then return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
