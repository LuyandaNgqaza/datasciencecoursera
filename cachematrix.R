## the function creates a Matrix object that can cache its inverse
makeCacheMatrix <- function( q = matrix() ) {
    i <- NULL
set <- function( matrix ) {
    q <<- matrix
    i <<- NULL
}
get <- function() {
    q
}
sInverse <- function(inverse) {
    i <<- inverse
}
gInverse <- function() {
    i
}
list(set = set, get = get,
     sInverse = sInverse,
     gInverse = gInverse)
}
## this function computes the inverse function(makeCacheMatrix)
cacheSolve <- function(x, ...) {
    q <- x$gInverse()
    if( !is.null(q) ) {
        message("getting cached data")
        return(q)
    }
    dt <- x$get()
    q <- solve(dt)
    x$sInverse(q)
    q
}