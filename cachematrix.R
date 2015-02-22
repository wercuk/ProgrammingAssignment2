## It's something about matrix

## Cache and check function

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        # Check for existing inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # Getting the matrix from the list
        data <- x$get()
        # Computing of inverse
        inv <- solve(data,...)
        x$setsolve(inv)
        ## Return a matrix that is the inverse of 'x'
        inv        
}


## Solve function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        # Create a special "matrix" object that can cache its inverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
