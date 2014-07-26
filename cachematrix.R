## Below are two functions makeCacheMatrix and cacheSolve that enables us to inverse a matrix
## while keeping the inversed matrix in cache

# The first function makeCacheMatrix creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
## set the value of the matrix in cache
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
## used to retrieve or get the value of the matrix
## and then set the inverse, storing this in cache
## before getting the value of the inverse

        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## the function cacheSolve calculates the mean of the matrix created in makeCacheVector

cacheSolve <- function(x, ...) {
        
## first checks whether the mean has already been stored in cache
## if so, the inverse is retrieved from cache
## else, the inverse is calculated and uses setsolve to store this value in cache
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

