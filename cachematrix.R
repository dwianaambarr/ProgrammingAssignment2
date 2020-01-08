## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  #initialize
  j <- NULL
  
  #Create function set to set the matrix
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  
  #Create function get to get the matrix
  get <- function() x
  
  #Function to set inverse of the matrix
  setInverse <- function(inverse) 
    j <<- inverse
  
  #Function to get inverse of the matrix
  getInverse <- function() 
    j 
  
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  
  ## Use condition to return inverse if matrix isn't NULL
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  
  ## Get matrix from object
  mat <- x$get()
  j <- solve(mat,...)
  
  ## Set inverse to the object
  x$setInverse(j)
  
  ##Return the matrix
  j
}
