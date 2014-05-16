## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # minv will store the cached matrix inverse
  minv <- NULL
  
  # sets the matrix
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  # gets the matrix
  get <- function() x
  
  # sets the matrix inverse
  setinv <- function(solve) minv <<- solve
  
  # gets the matrix inverse
  getinv <- function() minv
  
  # returns the matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  
  # If the inverse is already computed, return the inverse
  if (!is.null(minv)) {
    message("getting cached matrix data")
    return(minv)
  }
  
  # The inverse is not yet computed, compute the matrix inverse
  mdata <- x$get()
  minv <- solve(mdata, ...)
  
  # Cache the matrix inverse
  x$setinv(minv)
  
  # Return it
  minv
}
