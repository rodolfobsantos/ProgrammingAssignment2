## Programming Assignment 2
## Functions that calculate and cache the inverse of a matrix

## makeCacheMatrix: initialize a matrix and contains functions that return the matrix (get),
##                  its inverse (getInverse) and set its inverse (set)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve: calculates the inverse of a matrix initialized by makeCacheMatrix and returns
##             it. If the inverse was already evaluated, returns the cached inverse matrix

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}