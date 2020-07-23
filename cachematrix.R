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
        # if computed, return, which promises inverse will just be calculated once
        m <- x$getsolve()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if m hasn't been computed, cacheSolve() executes computing, 
        # then caches m in the special matrix and print m
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}


# verify the two functions by test variable t

## generate a 5x5 squre numeric matrix
t <- matrix(c(9, 4, 10, 4), nrow=2)

## make t a special matrix that can cache its inverse
t <- makeCacheMatrix(t)

## inverse hasn't been computed, this will return NULL
t$getsolve()

## compute inverse and return it 
cacheSolve(t)
