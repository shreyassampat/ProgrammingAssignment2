## These are a set of functions that use lexical scoping to cache a matrix and
## its inverse

## makeCacheMatrix returns a list of functions that set or return the matrix
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inver) inv <<- inver
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve retrieves the inverse of x's matrix from cache if it has been set
## otherwise it calculates and sets it in addition to returning it

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    currentmatrix <- x$get()
    inv <- solve(currentmatrix)
    x$setinverse(inv)
    inv
}