## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
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



## Write a short comment describing this function

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
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
