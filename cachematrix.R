## These functions calculate the inverse of a matrix and cache it
## so it can be quickly recalled.

## creates a matrix object including set and get functions for accessing
## and setting the matrix, as well as function for setting and getting its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                # use this function to reset x to some other matrix
                x <<- y
                # set cached inverse to NULL
                inv <<- NULL
        }
        #get is a function that returns the matrix x
        get <- function() x
        #setinv is a function that sets inv to the value stored in inverse
        setinv <- function(inverse) inv <<- inverse
        #getinv is a function that returns the stored inverse of x
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Computes the inverse of the special matrix object returned by makeCacheMatrix
# If the inverse has already been calculated, retrieve inverse from cache.

cacheSolve <- function(x, ...) {
        ## First, attempt to retrieve cached value of inv
        inv <- x$getinv()
        if(!is.null(inv)) { #if there is a cached value
                message("getting cached data")
                return(inv) #exit this function
        }
        #else, if inv is NULL, calculate inverse and store in inv
        # First, import matrix object x into data
        data <- x$get()
        # Now take its inverse
        inv <- solve(data, ...)
        # Now store this value in matrix object x's inv variable
        x$setinv(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
