## Function "makeCacheMatrix" creates  an object that can keep a matrix and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # variable for inversed matrix 
  i <- NULL 
  set <- function(y) {  
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) i <<- solve
  get_inv <- function() i
  
  # this will be returned
  list(set = set, get = get, 
       set_inv = set_inv,
       get_inv = get_inv)
}

## Function "cacheSolve" calculate the inverse of the matrix returned
## by function "makeCacheMatrix". If the inverse was calculated
## then the "cacheSolve" should return the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$get_inv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matr <- x$get()
  i <- solve(matr, ...)
  x$set_inv(i)
  i
}


# Tests
example <- makeCacheMatrix(matrix(c(2,3,4,8), 2, 2))
example$get()
example$get_inv()
cacheSolve(example)
