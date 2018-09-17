## makeCacheMatrix finds the inverse of an invertible matrix x and caches result



makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
}


## cacheSolve finds inverse of invertible matrix x but first checks if result has already been cached

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
        if(!is.null(inv) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

