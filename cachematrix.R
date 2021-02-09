## The functions makeCacheMatrix and cacheSolve when used together can compute 
## and store in the cache the inverse of a given matrix x (as long as that 
## matrix is invertible)


## makeCacheMatrix takes an input x which must be a matrix and creates a 
## special "matrix" object, returning a list with 
## functions that can set both the matrix and its inverse, as well as get 
## (retrieve from the cache) the same values
## x$set: sets the matrix to be evaluated
## x$get: retrieves the matrix
## x$setinverse: sets inverse of the matrix
## x$getinverse: retrieves the inverse of the matrix

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


## cacheSolve takes as an input the list resulting from makeCacheMatrix and 
## can return the cached inverse matrix if it has already been calculated, 
## or compute the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) 
  }
  data <- x$get() 
  inv <- solve(data) 
  x$setinverse(inv) 
  inv
}
