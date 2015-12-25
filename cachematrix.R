## These functions allow creation of a matrix, that can store its own inverse

## MakeCacheMatrix serves to create special kind of OOP matrix

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve enables to store an inverse for the special matrix object created with the
## function above

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    
    inverse
}