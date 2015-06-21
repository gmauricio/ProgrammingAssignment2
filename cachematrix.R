## The above functions are used to calculate the inverse of a matrix, 
## and cache it to avoid computing it again if needed more than once. 


## makeCacheMatrix, creates a special "matrix", that is able to cache its inverse.
## It contains functions to:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## sets the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## gets the value of the matrix
  get <- function() x
  ## sets the value of the inversed matrix
  setinverse <- function(inverse) i <<- inverse
  ## get the value of the inversed matrix
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve, calculates the inverse of the special "matrix". 
## First it checks if it already has been calculadted, if so, it gets it from the cache.
## Otherwise, it calculates the inverse by using the "solve" function and saves the value in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
