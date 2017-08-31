## Functions for cahing the invert of a matrix
## in order to avoid recomputing it because it 
## can be time consuming for large matrices

## Create a "kind of" matrix with support for 
## cache of its invert
makeCacheMatrix <- function(x = matrix()) {
  # i is going to be the invert of the matrix
  # initialize it with the same dimensions as 
  # input matrix x
  # Set the values to NA as default
  i<-matrix(NA,nrow(x),ncol(x))
  
  ## Support for setting and getting the invert
  set <- function(y) {
    x <<- y
    i <<- matrix(NA,nrow(y),ncol(y))
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)}

## Returns the invert of the matrix (taking in account 
## the matrix is invertible, as specified in the assignment)
## If the invert was already computed, then retrieve it
## If not, then compute it and set it 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  
  # if the cached date was already computed 
  if(!all(is.na(i))) {
    message("getting cached data")
    return(i)
  }
  # else compute the invert
  data <- x$get()
  i <- solve(data, ...)
  # memorize the invert in the cache
  x$setsolve(i)
  # return the invert
  i
}
