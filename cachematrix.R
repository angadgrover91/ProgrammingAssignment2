makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # Used to set the value of m to null 
  y <- NULL # Used to set the value of y to null
  setmatrix <- function(y) { # Used to set the value of the matrix
    x <<- y
    m <<- NULL 
  }

  list(setmatrix = setmatrix, getmatrix = getmatrix, # Creates a list to store functions
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function (x=matrix(), ...) {
  # Compares the martrices
  m <- x$getinverse() # Checks if an inverse has already been calculated and gets it.
  if(!is.null(m)){ 
    if(x$setmatrix() == x$getmatrix()) { # Checks if the matrix has changed, and if it hasn't then sends a text message and returns the cached matrix.
      
      return(m)
    }
    
    y <- x$getmatrix() # Gets value of inout matrix
    x$setmatrix(y) 
    m <- solve(y, ...) 
    x$setinverse(m) # Runs the set inverse function on the inverse to cache the inverse
    m # Returns the inverse matrix
  }
