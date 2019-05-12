# Create an object that can cache the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(matInv) inv <<- matInv
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv, 
       getInv = getInv)
}

# Compute the inverse of a matrix. Caches the inverse and reuses it whenever possible.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}
