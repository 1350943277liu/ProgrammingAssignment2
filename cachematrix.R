## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        # fun1
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # fun2
        get <- function() x
        
        # fun3
        setsolve <- function(solve) m <<- solve
        
        # fun4
        getsolve <- function() m
        
        # return
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        # get and check if the inverse matrix m has been computed, 
        # if computed, return m
        m <- x$getsolve()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if m hasn't been computed, cacheSolve() executes computing, 
        # then cache m in the special matrix and print m
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
