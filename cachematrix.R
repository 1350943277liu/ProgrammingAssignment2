## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        #fun1
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #fun2
        get <- function() x
        
        #fun3
        setinverse <- function(inverse) m <<- inverse
        
        #fun4
        getinverse <- function() m
        
        #return
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        #get and check if m has been computed, if computed, return m
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #if m hasn't been computed, cacheSolve goes on computing, 
        # then return a matrix m that is the inverse of 'x'
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
