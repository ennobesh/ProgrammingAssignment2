##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(m = matrix())
{
  a <- NULL
  set <- function(b)
  {
    m <<- b
    a <<- NULL
  }
  get <- function()
    {
      m
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

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(m, ...)
{
  a <-m$getInverse()
  if (!is.null(a))
  {
    message("Getting cached data...")
    return (a)
  }
  data <-m$get()
  a <- solve(data, ...)
  m$setInverse(a)
  a
}

