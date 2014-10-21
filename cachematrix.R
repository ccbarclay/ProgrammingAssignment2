# Assignment 2 =================================================================

## Function Definitions ##

# Summary:
# makeCacheMatrix() and cacheSolve() allow one to cache the computation of a...
# ... matrix's inverse for future recall without recomputation.

# makeCacheMatrix():
# Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set_mtrx <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get_mtrx <- function() {x}
  set_inv <- function(inverse) {inv <<- inverse}
  get_inv <- function() {inv}
  list(set_mtrx = set_mtrx, get_mtrx = get_mtrx,
       set_inv = set_inv,
       get_inv = get_inv)
}

# cacheSolve():
# Computes the inverse of the special "matrix" returned by makeCacheMatrix above
# If the inverse has already been calculated (and the matrix has not changed)...
# ... then the cacheSolve() should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get_mtrx()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}


## Test Code ##

#Expected results...
solve(matrix(1:4, 2, 2))
solve(matrix(c(1,1,2,1), 2, 2))
solve(matrix(c(2,2,3,4), 2, 2))

# Modified Test Code...
mtrx <- makeCacheMatrix(matrix(1:4, 2, 2))
mtrx$get_mtrx()
mtrx$get_inv()
# NULL
cacheSolve(mtrx)
mtrx$get_inv()   # shows that the inverse has been stored
cacheSolve(mtrx)
# getting cached data
mtrx$set_mtrx(matrix(c(1,1,2,1), 2, 2))
mtrx$get_inv()
# NULL
cacheSolve(mtrx)
cacheSolve(mtrx)
# getting cached data
mtrx$get_mtrx()

# NOTE: do NOT call set_inv() directly despite it being accessible for the reason you will see next
mtrx$set_inv(matrix(c(2,2,3,2), 2, 2))
mtrx$get_inv()   # obviously non-sense since...
mtrx$get_mtrx()
cacheSolve(mtrx)
# as you can see the call to set_inv() effectively corrupted the functioning of the code

mtrx <- makeCacheMatrix(matrix(c(2,2,3,4), 2, 2))
mtrx$get_mtrx()
cacheSolve(mtrx)
cacheSolve(mtrx)
# getting cached data
