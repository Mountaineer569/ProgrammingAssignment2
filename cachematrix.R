## R programming
## Assignment 2

# makeCacheMatrix function creates a special list containing a function to:
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the value of the inverse matrix
#   4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        # note the '<<-' symbol assigns a value to object in another environment
        x <<- y
        inv <<- NULL
    }
    get <- function() 
        x
    
    setinverse <- function(inverse) 
        inv <<- inverse
    
    getinverse <- function() 
        inv
    
    # store results in a list
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# caclulates the inverse of the matrix created with makeCacheMatrix.R function.
# If the inverse has already been calculated, it gets it from the cache and skips the computation.
# Otherwise, it calcs the inverse of the matrix and sets the value of the inverse in the cache via
# the setinverse function.

cacheSolve <- function(x) {
    inv <- x$getinverse()
    
    # if the inverse of the matrix already exists in the cache, 
    # pull it instead of recalculating it.
    if(!is.null(inv))  {
        message("getting cached data")
        return(inv)
    }
    # otherwise, calc the inverse of the matrix and store in the cache for next time.
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
