## Functions to invert a marix.  If matrix was previously inverted, returns the result from prior calculation.


## This function makes a list of functions to calculate inversion.  

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setminv <- function(solve) m <<- solve
    getminv <- function() m
    list(set = set, get = get,
         setminv = setminv,
         getminv = getminv)  
  
}


## Looks for result from prior calculation.  If not found, inverts the matrix.  Returns the inverse x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

      m <- x$getminv()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setminv(m)
      m
}
