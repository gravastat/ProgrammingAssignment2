## cachematrix.R contains two functions:
##
## 	1.) makeCacheMatrix
##	2.) cacheSolve 
##
## makeCacheMatrix allows for the creation of a matrix
## and can cache the inverse of the matrix if it has
## been caclculated by the solveInverse.  The provided
## matrix is assumed to be invertible.
##
## example command line input and output
## >cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## makeCacheMatrix function:
## commandline input should be of the form:
## makeCacheMatrix(matrix(data,nrow,ncol))
## for example
## makeCacheMatrix(matrix(1:4,2,2))
## and can cache the inverse of the matrix if it has 
## been caclculated

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<-y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<-solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will retrieve the cached inverse matrix
## or it will calculate the inverse matrix using
## the solve() function provide by base R 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
