## These functions cut down on processing time by caching the 
## result of a matrix inversion so if subsequent requests for
## identical inversions are made then the cache can be retrieved
## saving the time it takes to recalculate the inversion.

makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special Matrix object that can cache
  # its inverse. The <<- operator is used to assign a value to the 
  # inversion object that is outside the current environment. 
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinversion <- function(inversion) i <<- inversion
  getinversion <- function() i
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


cacheSolve <- function(x, ...) {
  # This function checks the inversion object that was created
  # in the previous function. If it exists, the inversion matrix
  # is extracted from the cached data, otherwise it is calculated 
  # using the Solve function.
  i <- x$getinversion()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinversion(i)
  i
}

