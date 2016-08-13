## The functions below create an object that stores a matrix and caches its inverse.  This can save time and computing power.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <-function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inver <<- inverse
        getInverse <- function() inver
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the matrix created in makeCacheMatrix.  If the inverse has already been calculated, it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        mat <- x$get()
        inver <-solve(mat, ...)
        x$setInverse(inver)
        inver
}