## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix generates a special matrix object which allows for caching the 
## inverse of the matrix...and easy accessing

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL # inv is the inverse of matrix x
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinv <- function(newInv) inv <<- newInv
   getinv <- function() inv
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## cacheSolve takes the special matrix object and calculates the inverse matrix
## if necessary, else it gets the inverse from the object, and then returns either

cacheSolve <- function(x, ...) {
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

## testing
# mat <- matrix(c(1,1,2,1,1,1,2,1,1), 3, 3)
# matSpec <- makeCacheMatrix(mat)
# cacheSolve(matSpec) # this should call solve within the function
# cacheSolve(matSpec) # this should not!