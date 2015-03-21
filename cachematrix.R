# Below is an R function to cache the potentially time-consuming computation of an inverse.
# Then, if the contents of a vector have not changed, the inverse may be looked up in stead of recomputed.

## makeCacheMatrix does exactly what the name implies: it makes a special object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    RonJoninv <- NULL
    set <- function(y) {
        x <<- y
        RonJoninv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) RonJoninv <<- inverse
    getinverse <- function() RonJoninv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve gives you the inverse of the matrix.
# If it has been computed before, it will return that result and will not compute it again.
# Otherwise, it computes the inverse, and caches the result for future use.
cacheSolve <- function(x, ...) {
    RonJoninv <- x$getinverse()
    if (!is.null(RonJoninv)) {
        message("getting cached data.")
        return(RonJoninv)
    }
    data <- x$get()
    RonJoninv <- solve(data)
    x$setinverse(RonJoninv)
    RonJoninv
}
