## Matrix inversion is a time-consuming computation 
## Caching the inverse of a matrix is more efficient than a repeat compute


## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.
## It does the following:
## - sets the value of the matrix
## - gets the value of the matrix
## - sets the value of the inverse matrix
## - gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
     set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the inverse will be retrieved.
## Otherwise, the function computes the inverse and sets the value of the 
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
