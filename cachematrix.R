## makeCacheMatrix finds the inverse of an invertible matrix x and caches result
## cacheSolve finds inverse of invertible matrix x but first checks if result has already been cached

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setcache <- function(solve) m <<- inverse
        getcache <- function() m
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getcache()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setcache(m)
        m
}

