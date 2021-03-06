# course_week: 2.3
# description: Use a function constructor to define get and set operations
# make generic: yes
makeCacheMatrix <- function (x) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinv <- function(inverse) inv <<-- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function (x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}