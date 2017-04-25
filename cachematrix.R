## below are a pair of functions that can be used to create a special object that stores a matrix and caches its inverse, 
## instead of calculating the inverse repeatedly
## This function creates a matrix that can inverse its cache.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    calc <- function(y) {
          x <<- y
          inv <<- NULL
    }
    pull <- function() x
    calcInverse <- function(inverse)  inv <<- inverse
    pullInverse <- function() inv
    
    list(calc = calc,
         pull = pull,
         calcInverse = calcInverse,
         pullInverse = pullInverse
         )
}


## Function computes the inverse of a matrix, created by the makeCacheMatrix function above. 
## If the inverse has already been calculated, the function recieves data from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$pullInverse()
    if(!is.null(inv)) {
          message("pulling cached data")
          return(inv)
    }
    
    mat <- x$pull()
    inv <- solve(mat, ...)
    x$calcInverse(inv)
    inv
}
