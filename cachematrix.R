#The functions below cache the inverse of a matrix to recall it later
#without the need of computing the inversion again

#makeCacheMatrix - creates a list with four functions
# 1 - set the value of the matrix
# 2 - get the value of the matrix
# 3 - set the inverse of the matrix
# 4 - get the inverse of the matrix


makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolv <- function(solve) m <<- solve(x)
        getsolv <- function() m
        list(set = set, get = get,
             setsolv = setsolv,
             getsolv = getsolv)
}

# cacheSolve - function to test if there is a cached value in another environment
# and if the value exists, the function return the cached value, otherwise it
# calculates the inverse of the matrix x

cacheSolve <- function(x, ...) {
        m <- x$getsolv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolv(m)
        m
}
