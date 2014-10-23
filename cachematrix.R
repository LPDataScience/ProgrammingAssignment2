## cachematrix.R
## Routines for caching matrix inverse

## makeCacheMatrix
# Takes matrix and returns a matrix object that can cache its inverse
# x = square, non-singular matrix
# Returns a matrix object with the following methods:
#   $set(m): sets a new matrix (m) and clears the cached inverse
#   $get(): gets the current matrix
#   $setinverse(inv): caches the inverse (inv) in the object
#   $getinverse(): returns the current cached inverse or NULL if not set
makeCacheMatrix <- function(x = matrix()) {
    # clear the inverse matrix
    inv <- NULL
    
    # set matrix function
    set <- function(y) {
        # store the matrix in the parent env
        x <<- y
        # clear the inverse (not calculated yet)
        inv <<- NULL
    }
    
    # get the matrix
    get <- function() x
    
    # set the inverse matrix
    setinverse <- function(newinv) inv <<- newinv
    
    # get the inverse matrix
    getinverse <- function() inv
    
    # return list of functions
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve
## Solve for the inverse of a matrix and cache the results for future use
## x must be a matrix created by makeCacheMatrix() and set with a square matrix
## cacheSolve may then be used repeatedly with the special matrix x where it
## caculate the inverse on the first use and use the cache on subsequent uses.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    # check for cached inverse and return if found
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # no cached inverse found so calculate and store
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
