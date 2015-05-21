## This function creates a special "matrix" object that can cache its inverse.


## You need to input a matrix, and the function makeCacheMatrix
## returns a list of functions to help to set a inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get = function() {
    x
  }
  
  setinv = function(inverse) {
    inv <<- inverse 
  }
  
  getinv = function() {
    inv
  }
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated (and 
#the matrix has not changed), then the cachesolve should retrieve the inverse
#from the cache.

## x is the output of makeCacheMatrix()
## the return is the inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  
  
  inv = x$getinv()
  
  # check if the inverse has already been calculated. If so, skip the calculation.
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # if not, calculate the inverse.
  data = x$get()
  inv = solve(data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
  
}

##Richard Guggemos's explanation helped me a lot! Thank you, Richard!