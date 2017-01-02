##Functions that calculate inverse of matrix and stores it in cache if necessary
##Modified 1.2.2017 by ellielamberton

## Function for caching and retrieving matrix and matrix inverse 
## returns list of functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  ##stores matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##retrieves matrix from Cache
  get <- function() x
  ##stores inverse of matrix
  setinv <- function(mat_inv) inv <<- mat_inv
  ##retrieves inverse of matrix
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Takes in makeChacheMatrix argument, returns inverse of Matrix 
## Caches inverse, if not cached already

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  ##If inverse is not present, calculate inverse and store in cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}