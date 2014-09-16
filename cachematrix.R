## Two functions below are used to create a special object that stores a matrix 
## and caches its inverse

## makeCacheMatrix creates a special "matrix" that can cache its inverse.
## makeCacheMatrix contains 4 functions which 
## 1) set the value of the matrix; 
## 2) get the value of the matrix; 
## 3) set the value of its inverse;
## 4) get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) m <<- solve
  get_inverse <- function() m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix function. 
## cacheSolve retrives the inverse from the cache if it has already been calculated (given that the matrix is the same).
## Otherwise cacheSolve computes the inverse, then cashes and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}
