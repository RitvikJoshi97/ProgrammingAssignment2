## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
       set <- function(y) {                    ## this will define the set function to assign new value
       x <<- y                             
       inv <<- NULL                            ## this will : if there is a new matrix, reset inv to NULL
    }
    get <- function() x                        ## this will define the get fucntion - returns value of the matrix argument

    setinverse <- function(inverse) inv <<- inverse  ## this will assign value of inv in parent environment
    getinverse <- function() inv                     ## this gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
        if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
