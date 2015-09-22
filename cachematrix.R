## Assignment: Caching the Inverse of a Matrix
## For this assignment, assume that the matrix supplied is always invertible.
## To Test:
##      > mat1 <- matrix(1:4, 2)
##      > cmat <- makeCacheMatrix(mat1)
##      > cacheSolve(cmat)
##      > cacheSolve(cmat)      ## the second call shows "getting cached data"
##      
## debug(cacheSolve) can also be used to step into the two calls above
## cacheSolve(cmat) %*% mat1 should return an identity matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invx) inv <<- invx
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
