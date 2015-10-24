## makeCacheMatrix and cacheSolve when used together
## will find the inverse of a matrix x

## makeCacheMatrix creates a list of values
## related to the matrix x and its inverse, known as m,
## and stores these values for later retrieval.

makeCacheMatrix <- function(x = matrix()) {
  
  ## m is the stored inverse of the matrix x
  ## sets m to null
  m <- NULL
  
  ## commented out seemingly superflous code
  #fSet <- function(y) {
  #  x <<- y ## puts y into matrix x
  #  m <<- NULL ## sets the stored inverse of matrix x to null
  #}
  
  ## returns matrix x
  fGet <- function() {
    x
  }
  
  ## fSetinverse takes the argument 'solve' from cacheSolve
  ## which cacheSolve defines as the inverse of matrix x, known as m
  ## and writes over m in the list for matrix x
  fSetinverse <- function(inverseforcache) {
    m <<- inverseforcache
  }
  
  ## returns the inverse of matrix x, known as m
  fGetinverse <- function() {
    m
  }
  
  ## names each object defined in the makeCacheMatrix environment
  ## as an element of a list  
  list(
    ## commented out seemingly superflous code
    #fSet = fSet,
    fGet = fGet,
    fSetinverse = fSetinverse,
    fGetinverse = fGetinverse)
}


## cacheSolve solves for the inverse of a matrix
## and stores the value in the list created by makeCacheMatrix
## for later retrieval

cacheSolve <- function(x, ...) {
  
  ## retrieves fGetinverse value from list for matrix x
  ## this is the inverse of matrix x, known as m
  m <- x$fGetinverse()
  
  ## if the inverse of matrix x, known as m, contains a value
  ## return the message "getting cached data" and value of m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## retrieves fGet value from list for matrix x
  ## this is the matrix x
  data <- x$fGet()
  
  ## solves for the inverse of the matrix x, stored as data
  ## puts value into m
  m <- solve(data, ...)
  
  ## caches m in fSetinverse value for list for matrix x
  x$fSetinverse(m)
  
  ## prints the inverse of the matrix x, known as m
  m
}

## I was sick and tired of running both functions seperately on a dataset.
## Hence, the below function.
inverse <- function(x) {
  cacheSolve(makeCacheMatrix(x))
}