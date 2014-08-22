## functions for assignment 2
## --------------------------
## makeCacheMatrix creates an object with several functions and stores 
## a matrix.
## cacheSolve uses these functions to read or write the result for
## inverting the matrix.


makeCacheMatrix <- function(x = matrix()) {
  
  ## test the matrix being invertible
  if(det(x)==0) {
    message("matrix is not invertible")
    message("try again :-)")
    return(x)
  }
  
  ## the globalInverse variable is set to null, when the object is created
  globalInverse <- NULL
  
  ## functions to work with the stored matrix
  set <- function(y) {
    x <<- y
    globalInverse <<- NULL
  }
  
  get <- function() {x}  
  
  setInverse <- function(inverse) {globalInverse <<- inverse}
  
  getInverse <- function() {globalInverse}
  
  ## creating the output list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  ## try to read the inverse from the makeCacheMatrix-object
  globalInverse <- x$getInverse()
  
  ## if the inverse has been calculated before...
  if(!is.null(globalInverse)) {
    
    ## ...read the result from the cache;...
    message("getting cached data")
    return(globalInverse)
  }
  
  ## ...if not...
  ## 1) get the matrix from the makeCacheMatrix-object
  data <- x$get()
  
  ## 2) calculate the inverse
  globalInverse <- solve(data, ...)
  
  ## 3) store the result in the makeCacheMatrix-object
  x$setInverse(globalInverse)
  
  ## 4) and show it to the user
  globalInverse
}
