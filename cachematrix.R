## The functions listed here computes the inverse of an invertible matrix 
## and caches the result. This avoids re-computation of the inverse on
## subsequent calls saving system resources.
## example:
## test <- makeCacheMatrix(matrix(c(4,2,7,6),2,2))
## cacheSolve(test)
## On the first call the console will show the computed result, but on
## subsequent calls of cacheSolve(test) the console will print the message
## 'getting cached data' and display the result from the cache.   

## makeCacheMatrix function takes a matrix as an input and creates a special list
## containing functions to set the matrix, get the matrix, cache the inverse of 
## the matrix, retrieve the cached inverse.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse) 
}


## cacheSolve function takes the special list returned by the makeCacheMatrix function 
## and return the inverse of a matrix. On the first call, it computes the inverse and 
## sets the result into the cache and on subsequent calls, fetches the result from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
