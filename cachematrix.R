## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    # Set matrix value and delete cached inverse only if new matrix is
    # different than current value
    set <- function(y) {
        if (!identical(x, y)) {
            x <<- y
            inverse <<- NULL
        }
    }
    
    # Return current matrix value
    get <- function() x
    
    # Cache inverse value
    setinverse <- function(solve) inverse <<- solve
    
    # Return cached inverse value
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve retrieves the inverse from the 
## cache.

cacheSolve <- function(x, ...) {
    
    # Return cached inverse value if one is found
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # No cached inverse, so need to calculate
    message("cached data not found, calculating value")
    data <- x$get()
    inverse <- solve(data, ...)
    
    # Cache inverse matrix for future use
    x$setinverse(inverse)
    
    # Return inverse matrix
    inverse
}