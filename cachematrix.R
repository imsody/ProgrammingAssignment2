### functions for computing a special matrix inverse


makeCacheMatrix <- function(x = matrix()) {
## Creates a special matrix object that can cache its inverse.
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


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
## computes the inverse of the special matrix returned by makeCacheMatrix 
## if the inverse has already been calculated and the matrix has not changed, 
## then cacheSolve retrieves the inverse from the cache.
    inv <- x$getinv()
    print(inv)
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
