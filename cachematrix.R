## makeCacheMatrix and cacheSolve are funcitons that cache the inverse of a matrix 

##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv2 <- NULL
  set <- function(y) {
    x <<- y
    inv2 <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv2 <<- inverse
  getInverse <- function() inv2
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv2 <- x$getInverse()
  ## data is cached
  if (!is.null(inv2)) {
    print("data is cached")
    return(inv2)
  }
  matrix <- x$get()
  inv2 <- solve(matrix, ...)
  x$setInverse(inv2)
  return(inv2)
}
