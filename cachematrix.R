## Matrix inversion is costly. The pair of functions below cache the inverse
## of a matrix, as an alternative to computing it repeatedly.

## NOTE: I have tested this code with the mutliple tests provided by the TA's
## in the forums, and it passes.

## This function creates a special "matrix" object that can cache its inverse.
## This function stores a list of functions.
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      ## Set the matrix in cache
      set <- function(y){
          x <<- y
          i <<- NULL
      }
      ## Get the matrix in cache
      get <- function() x
      ## Directly set the inverse matrix in cache
      setinverse <- function(inverse) i <<- inverse
      ## Get the inverse matrix in cache
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated
## (and the matrix has not changed), then the `cacheSolve` retrieves the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if(!is.null(i)){
          ## Retrieve inverse from cache, if available
          message("getting cached data")
          return(i)
      }
      ## If inverse not available from cache, calculate it
      data <- x$get()
      i <- solve(data,...)
      x$setinverse(i)
      i
}
