## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix caches the inverse of a matrix object.
#  It includes 4 functions (set, get, setinverse, getinverse). The set functions change the vector stored in the main function 
#  and the get functions return the stored values from the main function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function calculates the inverse matrix unless it is already cached in unchanged form in which case it will retrieve 
#  the cached version.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

