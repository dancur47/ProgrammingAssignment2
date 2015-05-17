## These functions take as input an ivertible square matrix, calculates its inverse, and stores the inverse for easy future access.

## This function takes the input and creates functions that reference it. If the inverse matrix already exists it is stored here.
makeCacheMatrix <- function(x = matrix()) {
  inv_matx <- NULL
  set <- function(new_matx) {
    x <<- new_matx
    inv_matx <<- NULL
  }
  get <- function() x 
  setinv <- function(inv) inv_matx <<- inv
  getinv <- function () inv_matx 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## This function calculates the inverse matrix and stores it back in makeCacheMatrix. If the inverse matrix already exists
## in makeCacheMatrix, it returns the existing one. 
cacheSolve <- function(x, ...) {
  inv_matx <- x$getinv()
  if(!is.null(inv_matx)) {
    message("getting inverse matrix")
    return(inv_matx)
  }
  matx <- x$get()
  inv_matx <- solve(matx)
  x$setinv(inv_matx)
  inv_matx
}
