## Caching the inverse of a matrix

## makeCacheMatrix creates a list that contains a function to 
## set the value of the matrix (set), 
## get the value of the matrix (get), 
## set the value of the inverse matrix (setinv), 
## and get the value of the inverse matrix (getinv)

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the value of the inverse matrix
  m <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse matrix 
  setinv <- function(inv_matrix) m <<- inv_matrix
  
  ##get the value of the inverse matrix
  getinv <- function() m
  
  ## pass the value of the function makeCacheMatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function calculates the inverse of the special 
## matrix created with the above funciton

cacheSolve <- function(x, ...) {
  ## get the inverse matrix
  m <- x$getinv()
  
  ## if the inverse matrix exists, get the value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if the inverse matrix does not exist, calculate the value
  data <- x$get()
  m <- solve(data, ...)
  
  ## cache the calcualted inverse matrix
  x$setinv(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
    
}
