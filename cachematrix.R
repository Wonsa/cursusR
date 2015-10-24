## Creates a list containing a function to set matrix,
## get matrix, set inverse matrix and get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {       
        x <<- y                 
        inv <<- NULL              
    }                          
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set=set,get=get,
         setinv = setinv,
         getinv = getinv)
}


## Calculates the inverse of the matrix created with makeCacheMatrix

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