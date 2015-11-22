
makeCacheMatrix <- function(m = matrix()) {
        ## Creates a special Matrix object with the methods:
        ##       get, set
        ##       setsolve, getsolve
        ## This special Matrix object is to be used with the
        ## cacheSolve function to compute the inverse of a
        ## Matrix with caching.
        i <- NULL
        set <- function(y) {
                m <<- y
                i <<- NULL
        }
        get <- function() m
        setsolve <- function(inverse) i <<- inverse
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'.
        ## 'm' needs to be created with makeCacheMatrix.
        ## If the inverse of the matrix was previously
        ## calculated using 'cacheSolve' it will immediately
        ## return a cached answer.
        i <- m$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- m$get()
        i <- solve(data, ...)
        m$setsolve(i)
        i
}
