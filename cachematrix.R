## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create a special matrix, that is able to store its Inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
### Get the inverse of the matrix, if it was calculated before, it gets the cached value, if not it calculate it, and store it. in the special element that we create in the previos function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data Inv")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
