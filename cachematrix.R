## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## this will solve cache

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
