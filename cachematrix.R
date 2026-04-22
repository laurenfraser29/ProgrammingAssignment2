## Put comments here that give an overall description of what your
## functions do

## These functions create a matrix object that then caches its inverse so that it does 
## not need to be recomputed every time. 

## Write a short comment describing this function
## This function creates a matrix object that stores (caches) its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function checks if the inverse is already calculated, 
## if it is the cachesolve should retrieve the inverse from the cache, if not
## it computes the inverse of the matrix (makeCacheMatrix) and stores it. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv    ## Return a matrix that is the inverse of 'x'
}