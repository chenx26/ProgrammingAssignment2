## Function to create cached Matrix that stores the original matrix
## and the inverse of the original matrix
## with get and set functions for both of them

## create cached Matrix

makeCacheMatrix <- function(x = matrix()){
  m = NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## a function with cached Matrix as input
## returns the inverse of the Matrix

cacheSolve <- function(x, ...){
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

## some test code

A = matrix(c(1,2,3,4), nrow = 2)
A_inv = solve(A)
res = makeCacheMatrix()
res$set(A)
res$get()
res$getInverse()
# no cached data, compute inverse
cacheSolve(res)

# use cached data instead of computing the inverse
cacheSolve(res)
