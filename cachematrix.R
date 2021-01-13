## These functions can be used for creating a matrix and caching the computations of the functions.

## Function1
##  set() - sets the value of the matrix
##  get()  - gets the value of the matrix
##  setinv() - sets the value of the inverse
##  getinv() - gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
        }
        get <- function() {x}
        setinv <- function(inverse) {inv <<- inverse}
        getinv <- function() {inv}
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Function2
## The function computes the inverse of the created matrix by the function above. If the inverse was calculated,
## the following function will take the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv))
        {message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}
