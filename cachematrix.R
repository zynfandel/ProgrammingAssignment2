## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Computes inverse of matrix
  get <- function() x
  setmatrix <- function(inv) m <<- inv
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## This function computes the inverse of the matrix above. 
## If the inverse has already been calculated, then the cacheSolve 
## should retrieve the inverse from the cache. If not, it computes the inverse and
## stores the value in cache.

cacheSolve <- function(x = matrix(), ...) {

  ## Checks to see if the result is already cached; if so, returns cached result
  m <- x$getmatrix()
  if (!is.null(m)){
    message ("getting cached data")
    return (m)
  }
  
  ## If not, returns a matrix that is the inverse of x
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
}
