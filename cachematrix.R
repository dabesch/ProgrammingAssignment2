## Function for finding the invers of a matrix
# This is an example of caching a function to improve performance
#example of lexical scoping 

## makeCacheMatrix function creates a cache of the matrix calc

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setin <- function(mean) m <<- mean
  getin <- function() m
  list(set = set, get = get,
       setin = setin,
       getin = getin)
}


## cacheSolve produces the result - the inverse matrix

cacheSolve <- function(x, ...) {
  m <- x$getin()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setin(m)
  m
}
