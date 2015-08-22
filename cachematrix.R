## The file contains two function written for the Programming
## Assignment2 for the R Programming on Coursera

## This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x=matrix()) {
    m <- diag(nrow(x))
    set <- function(y) {
        x <<- y
        m <<- diag(nrow(y))
    }
    get <- function() x
    setInverse <- function (inver) m <<- inver
    getInverse <- function () m
    list (set = set, get = get, 
          setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the 
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if (!identical(m,diag(nrow(m)))) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
