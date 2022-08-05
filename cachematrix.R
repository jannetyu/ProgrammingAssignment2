## The makeCacheMatrix function gets and sets the elements of the matrix, while also
## getting and setting the elements of the matrix's inverse (which we want).
## The matrix must be a square matrix for this to work, and then it caches its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
  } 
  
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv 
  list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
        
}
 

## If the inverse of the matrix created above has already been calculated,
## the cacheSolve function will get the info from the cache rather than calculating it
## again. If it isn't already calculated, the solve functoin calculates it and stores it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
        
  original_matrix <- x$get()
  inv <- solve(original_matrix, ...)
  x$setinverse(inv)
  inv
}
