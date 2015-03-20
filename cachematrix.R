## These function allow us to cache the inverse of a matrix which is a fairly
## time-consuming operation.  Caching allows us to avoid the operation altogether
## if the inverse has previously been calculated.

## This is akin to what we might do with a Java class - lending an object-
## oriented aspect to R.

## Mathematically, a matrix's inverse is an matrix that when multiplied by the 
## original matrix yields the identity matrix.  Only square matrices can be
## inverted and not all square matrices are invertible.  The identity matrix is
## the matrix which when muliplied by any matrix A yields that matrix A.
## Finding the matrix inverse is a very important ability in fields such as
## linear algebra.

## IMPORTANT NOTE:  These functions assume that the provided matrix x is a
## square matrix which is invertible.  Results can not be guaranteed otherwise.

## makeCacheMatrix is creating the special "object" that holds a matrix and
## its inverse.  It allows us the following operations (internal functions):
##    set - allows us to create an object for the matrix x (no inverse yet)
##    get - allows us to get the whole object (matrix x and inverse inv)
##    setinverse - allows us to set the object's inverse
##    getinverse - allows us to get the object's inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## cacheSolve will always return the inverse to the matrix.
## If the solution was already solved and cached, cacheSolve just returns that
## cached inverse.  Otherwise, cacheSolve will find the inverse and cache it
## before returning it.  cacheSolve uses R's solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            ## There is a cached inverse
            message("getting cached data")
      } else {
            ## There is no cached inverse
            ## First get the original matrix from x
            orig <- x$get()
            ## Now solve it and cache it
            inv <- solve(orig)
            x$setinverse(inv)
      }
      ## We now have the inverse so return it
      inv
}
