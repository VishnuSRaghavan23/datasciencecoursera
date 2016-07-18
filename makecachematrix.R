makeCacheMatrix <- function(x = matrix()) {
  ## x creates a square invertible matrix
  ## the object is to return  a list containing functions to set and obtain a matrix, set and obtain its inverse.
  # This is the input for the cacheinverse step as follows
  inv = NULL
  set = function(y) {
    # << function is employed to allocate a value to an object in an external environment 
     
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  #x is output of makeCacheMatrix step
  # it returns the inverse of the original matrix input to makeCacheMatrix step
  
  inv = x$getinv()
  
  # if the inverse has already been calculated, it obtains it from cache and ignores the calculation
  if (!is.null(inv)){
   message("getting cached data")
    return(inv)
  }
  
  # or else it calculates the inverse
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}


