
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse as NULL
  inv <- NULL
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse
  }
  
  # Function to get the value of the matrix
  get <- function() x
  
  # Function to set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getInverse <- function() inv
  
  # Return a list of the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  # Get the cached inverse
  inv <- x$getInverse()
  
  # Check if the inverse is already computed
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not cached, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the computed inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}
