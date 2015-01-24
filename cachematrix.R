## The makeCacheMatrix function is used for storing the inverse of a matrix and cacheSolve function are used for
## calculating the inverse of a matrix (if necessary) or simply retrieve it from cache from a previous
# calculation. If the inverse is calculated, then the inverse of the matrix is cached for future use. 

## The makeCacheMatrix function accepts a matrix vector as an input
## and creates a storage space for storing the inverse of the matrix
## The matrix is passed as a parameter to the function. 
## Return Value : A list of methods that can be used for setting and retrieving
## the contents of the matrix and its inverse
## KEY ASSUMPTION : The matrix passed as an input must be a square, invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  ## The "set" function used when the row or column values of the original matrix need to be changed
  ## As a result of the changed row or column values, the original inverse is no longer valid
  ## and must be reset.
  
  set <- function(newMatrix){
    x <<- newMatrix
    inVerseMatrix <<- NULL
  }
  
  # The "get" function is used for retrieving the matrix created with makeCacheMatrix
  
  get <- function() x
  
  # The "setInverse" function is used for storing the inverse of the matrix. The inverse can then be
  # retrieved subsequently using the "getInverse" function
  
  setInverse <- function(mInverse) inverseMatrix <<- mInverse
  
  # The "getInverse" function is used for retrieving the inverse stored within the matrix created
  # via makeCacheMatrix. The inverseMatrix value may be NULL (if it has not yet been calculated) or 
  # an inverse matrix calculated via the cacheSolve function
  
  getInverse <- function() inverseMatrix
  
  # This is used for returning a list of functions that can be used for accessing the matrix
  # created via the makeCacheMatrix function and the inverse stored for that matrix
  
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## The cacheSolve function is used for returning the inverse of a matrix. The matrix 
## MUST HAVE BEEN CREATED via the makeCacheMatrix function. The inverse returned may either 
## (a) calculated using the solve function. This is necessary if the inverse has not yet 
## been calculated previously for the matrix or
## (b) retrieved from the cache of the matrix (if it has been calculated previously)

cacheSolve <- function(x, ...) {
  
  # get the inverse from the matrix X. This matrix should have been created via 
  # makeCacheMatrix function
  inverse <- x$getInverse()
  
  # check if inverse already has been cached. If it does, return the inverse
  if(!is.null(inverse)) {
    
    message("getting cached data")
    return(inverse)
  }
  
  # retrieve the matrix to calculate the inverse
  data <- x$get()
  
  # calculate the inverse of the matrix using the solve function
  # Matrix must be a square and Invertible matrix
  
  inverse <- solve(data,...)
  
  # set the inverse so that it can be cached and retrieved for future use
  
  x$setInverse(inverse)
  
  ## Return a matrix that is the inverse of 'x'
  inverse     
  
}
