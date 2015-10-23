## The following functions perform calculation and caching
## of the inverse of an invertible matrix. This way
## a potentially repetitive and time-consuming operation
## such as the calculation of the inverse of a larger matrix
## can be stored and recalled for further use.

## The first function, makeCacheMatrix creates a list, consisting of
## functions which set the matrix, get the matrix, set the inverse
## of the matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function checks the function list created by makeCacheMatrix
## for a stored inverse of the original matrix fed into the latter function.
## If found, it is returned. If not, it is calculated, returned and stored
## into the inv variable of the list for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
