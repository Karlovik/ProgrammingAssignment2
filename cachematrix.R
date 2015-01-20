## First makeCacheMatrix creates a list of functions that
## handles the information in that environment, specifically a matrix x
## and its inverse and then, cacheSolve computes the inverse if not 
## saved beforehand or retrieves the recorded matrix otherwise 


## Gives a list of functions that handles the access to a matrix and its inverse 
## saved in a cache environment

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


## Retrieves the inverse matrix from the cached enviroment if any, if not
## computes it

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
  ## Return a matrix that is the inverse of 'x'
}
