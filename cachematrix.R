## This pair of functions takes as its input a square invertible
## matrix and caches the inverse of that matrix in the 'parent' (first)
## function. These functions allow a matrix inverse to be cached 
## than recomputed each time the value is called.

## makeCacheMatrix is the parent function of the pair of functions.
## It takes as its input a square invertible matrix, and outputs a 
## vector consisting of four functions: set, get, setinv, and getinv.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## introduces the object 'i' for later use
        set <- function(y){ 
                x <<- y 
                i <<- NULL
        } ## <<- operator allows child function (cacheSolve) to change 
        ## values in parent function (makeCacheMatrix). x <<- y
        ## allows 'set' to change the matrix used in the functions.
        ## i <<- NULL resets the cached inverse of a matrix to NULL.
        get <- function() {x}
        setinv <- function(inv) {i <<- inv}
        getinv <- function() {i}
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is the child function of the pair of functions.
## It takes as its input the makeCacheMatrix function.
## cacheSolve computes the inverse of a matrix, then caches
## that inverse of a matrix back in the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        i <- x$getinv() ## assigns 'getinv' from parent function to 'i'
        if(!is.null(i)) { ## checks if 'getinv' is null
                message("getting cached data")
                return(i)
        } ## if 'getinv' is NOT null in parent function, then function
        ## returns the cached inverse of a matrix to user
        data <- x$get() ## assigns 'get' from parent function to 'data'
        i <- solve(data, ...) ## computes inverse of inputted matrix
        x$setinv(i) ## caches inverse in 'setinv' of parent function
        i ## returns inverse of matrix to user
}
