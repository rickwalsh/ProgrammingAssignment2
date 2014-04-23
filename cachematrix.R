##
## makeCacheMatrix: creates a special 
## "matrix" object that can cache its inverse
## Rick Walsh 4/22/14

makeCacheMatrix <- function(x = matrix()) {

  ## 'set' function added to the list
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
    
  ## 'get' function added to the list   
  get <- function() x

  ## 'setsolve' runs 'solve' on matrix
  setsolve<- function(solve) m <<- solve

  ## 'getsolve' gets matrix
  getsolve <- function() m

  ## create list with 4 args for return
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve: establishes matrix and
## provides for caching the inverse
cacheSolve <- function(x, ...) {
  
  ## establish matrix and set it to matrix from list
  m <- matrix()
  m <- x$getsolve()
  
  ## if matrix is not null, it's cached so return it  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  ## if not cached, get it from list
  data <- x$get()

  ## 'solve' and save as matrix
  m <- solve(data, ...)
  x$setsolve(m)

  ## return inverse matrix
  m
}

