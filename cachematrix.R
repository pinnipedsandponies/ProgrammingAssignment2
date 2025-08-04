## These functions take advantage of scoping rules to preserve or
## change the state of an object in R. The first function creates a
## matrix object that can cache it's inverse. 
## The second function solves the matrix by checking if it has changed
## and returning the inverse, or if its the same, finding and returning
## the cached value.

## makeCacheMatrix creates a matrix that will cache its inverse value

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function() inv <<- solve(x)
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix created by makeCacheMatrix
## if the matrix has not changed, cacheSolve returns the cached value.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv      
}
}
