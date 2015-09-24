## Create an object list of functions associated with an input matrix and
## calculate the inverse of this matrix, if there is no cached value for it,
## and store new value.

## makeCacheMatrix creates a matrix object list of functions 'set', 'get' an input
## matrix and to retrieve and store the inverse of the input matrix. 

makeCacheMatrix <- function(x = matrix()) {

## Initialize inverse matrix variable
  
    invm <- NULL

## Create 'set' function to replace current input matrix
    
    set <- function(y) {
    x <<- y
    invm <<- NULL
    }
    
## Create 'get' function to retrieve input matrix
    
  get <- function() x

## Create 'setminv' function to set/cache the inverse matrix
  
  setminv <- function(minverse) invm <<- minverse

## Create 'getminv' function to retrieve cached inverse matrix
  
  getminv <- function() invm
  
## Store functions in matrix object list
  
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}


## cacheSolve function retrieves cached inverse matrix value associated with
## input matrix 'x' and will either return inverse value if it exists, otherwise
## it will calculate the inverse and store in the cache.

cacheSolve <- function(x, ...) {

## Retrieve inverse value of matrix
  
    invm <- x$getminv()

## If the matrix inverse exists, then retrieve from cache and return value.
    
    if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
    }

## This section retrieves input matrix, calculates the inverse with 'solve',
## stores inverse value in the cache and then returns it.

  data <- x$get()
  invm <- solve(data, ...)
  x$setminv(invm)
  invm
  
  }
