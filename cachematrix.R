## the functions will get the matrix and reproduce a inverse cache matrix

## makeCacheMatrix get the matrix
makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve give the solution
cacheSolve <- function(x, ...) {
         inv <- x$getInverse()
  if (!is.null(inv)) {

    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
