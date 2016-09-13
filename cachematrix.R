
##This is caching the inverse of a matrix to maximize
##computation performance when the matrix does
##not change. The functions below create a matrix
##then caches its inverse for quick retrival later.

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


## This creates the inverse of the matrix created above
##and stores the result for quick retrival in the future.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)){
        message("retrieving cached matrix inverse")
        return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setinverse(inv)
        inv
        
}
