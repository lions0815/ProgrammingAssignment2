
## makeCacheMatrix and cacheSolve work together to cache the inverse calculation of a matrix
##
## makeCacheMatrix constructs a "matrix" list which is used for storing the matrix and its inverse
## cacheSolve calulates the matrix inverse and stores its value in the matrix list


## makeCacheMatrix takes a matrix as an argument and returns a "matrix" list wih the appropriate functions to get/set the matrix and get/set the matrix inverse
makeCacheMatrix <- function(mat = matrix()) {
  mat_inv <- NULL
  set <- function(m) {
    mat <<- m
    mat_inv <<- NULL
  }
  get <- function() mat
  setinverse <- function(inv) mat_inv <<- inv
  getinverse <- function() mat_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a "matrix" list (constructed by makeCacheMatrix) and returns the inverse of the matrix
## it caches the matrix ones it's computed
cacheSolve <- function(x, ...) {
  inv <- m$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- m$get()
  inv <- solve(mat, ...)
  m$setinverse(inv)
  inv  
}
