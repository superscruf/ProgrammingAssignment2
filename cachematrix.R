# file cachematrix.R
# ver 1.0   Mo    6 Jun 2016
# Demonstrates lexical scoping, by creating an object that stores
# a cached value of a calculation. Data is created via one function,
# and stored for retrieval by another function (from a different
# scope)

# function makeCacheMatrix
# ver 1.0   Mo    6 Jun 2016
# Function takes a matrix as input parameter, then creates a bucket
# to store its inverse for when another function (cacheSolve())
# calculates it. Basically, we see how two functions can talk to
# each other through an intermediary object. In this case, not just
# the matrix, but the object created by this function which includes
# the matrix we pass, as well as buckets for the inverse, set(), get(),
# setMatrixInverse(), and getMatrixInverse(). 

makeCacheMatrix <- function(x = matrix()) {
  m   <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(solve) m <<- solve
  getMatrixInverse <- function() m
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}

# function cacheSolve
# ver 1.0   Mo    6 Jun 2016
# Given the output of makeCacheMatrix: 
#   first, check to see if cache has already been calculated
#     if yes, retrieve cached value from the object created
#       in makeCacheMatrix()
#     if no, perform inversion, then store it back in the object
#       created in makeCacheMatrix().
cacheSolve <- function(x, ...) {
  # Retrieve matrix inverse, using the function getMatrixInverse()
  # that was stapled on via the makeCacheMatrix() function above.
  m <- x$getMatrixInverse()
  
  # If the retrieval is not null, return it. return() bounces us
  # out of execution, so the rest of the function is not seen.
  # No need for ELSE clause.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Only gets here if m == NULL (or more properly, !is.null()). 
  # This part calculates the inverse.
  # The last line is there as a shortcut to return() - since it's
  # the last thing seen in the function, it's the default return value.
  # Lazy coders are happy coders!
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrixInverse(m)
  m
}
