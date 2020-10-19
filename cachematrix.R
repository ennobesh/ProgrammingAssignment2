## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. This assignment is to write a pair 
## of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
        a <- NULL
        set <- function(b)
        {
                x <<- b
                a <<- NULL
        }
        get <- function()
        {
                x
        }
        setInverse <- function(inverse)
        {
                a <- inverse
        }
        getInverse <- function()
        {
                a
        }
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) 
{
        a <-x$getInverse()
        if (!is.null(a))
        {
                message("Getting cached data...")
                return (a)
        }
        data <-x$get()
        a <- solve(data, ...)
        x$setInverse(a)
        a
}
