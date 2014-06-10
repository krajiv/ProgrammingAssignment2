## The cachematrix functions are designed to compute the inverse of a matrix.
## The computed inverse is stored in memory, so that it can be looked instead of 
## computing the inverse of similar matrices.

## The makeCacheMatrix function creates a list of operational methods for the
## matrix. The getData & the setData methods provide access(get/set) to the matrix elements
## The getInverse & setInverse methods are use to get and set the inverse value respectively

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        getData <- function() {
                x
        }
        setData <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getInverse <- function() {
                inv
        }
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        list(getData = getData, setData = setData, getInverse = getInverse, setInverse = setInverse)
}


## The cacheSolve method is used to compute the inverse of a matrix.
## If the inverse is available(pre-computed), then the result is read from
## the cache and returned. Otherwise, the inverse is computed and added to
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Retreived cached inverse...")
                return(inv)
        }
        
        message("No cache entry found... computing mean")
        data <- x$getData()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
