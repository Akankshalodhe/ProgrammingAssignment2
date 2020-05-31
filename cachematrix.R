## caching the inverse of matrix

## the functions here are used to create a special object that stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <-  function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function()inver
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse
  )
}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  mat <- x$get()
  inver <-solve(mat, ...)
  x$setInverse(inver)
  inver
}
