# These two functions are used to solve for the inverse of a matrix, cache the
# result. If the inverse is required again, then it can be accessed from cache,
# rather than needing to be calculated again. This can be used to speed up code 
# that requires the inverse of a matrix multiple times.



# This function returns a list, containing 4 functions that can be use to set
# the value of a matrix, get the value of a matrix, set the value of the matrix
# inverse, and get the value of the matrix inverse. It also creates an internal
# variable called "inverse" which is used to cache the output of Solve, called
# by cacheSolve().

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL                             ## creates and initialize the cache for matrix inverse
    set <- function(y) {
        x <<- y
        inverse <<- NULL                        ## reset the cache if the matrix x changes
    }
    get <- function() x  ## return x
    setInverse <- function(i) inverse <<- i     ## store the solved inverse in cache
    getInverse <- function() inverse            ## return the inverse
    list(set = set, get = get,  ... =           ## return the list of functions that can be called
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function first checks if a cached result of the inverse matrix is

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()                 ## retreive the value from cache
    if( !is.null(i) ) {                 ## is the cache not null?
        message("getting cached data")
        return(i)                       ## return the cache, exit out of cacheSolve()
                                        ## the rest of the code only is run if i was null
    }
    data <- x$get()                     ## get the matrix we need to calculate the inverse of
    i <- solve(data)                    ## calculate that inverse, store in i
    x$setInverse(i)                     ## store the result in cache
    i                                   ## return the calculate result
}

