# The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
#
# set the value of the matrix
#
# get the value of the matrix
#
# set the value of the inverse
#
# get the value of the inverse

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    k <- NULL
    set <- function(y) {
        x <<- y
        k <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) k <<- inverse
    getinverse <- function() k
    list(set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## Thw following function computes the inverse of the special “matrix” returned by makeCacheMatrix defined above

cacheSolve <- function(x, ...) {
    k <- x$getinverse()
    if (!is.null(k)) {
        message("getting cached data")
        return(k)
    }
    data <- x$get()
    k <- solve(data, ...)
    x$setinverse(k)
    k
}
