## The makeCacheMatrix() function creates a special "matrix" object that can cache its inverse. It is 
## essentially a list of instructions to set and retrieve a matrix and its inverse.
## The function produces objects 'x' and 's' within the parent makeCacheMatrix() environment.
## setmatrix() creates objects 'x' and 's' in the global environment (free variables)

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        setmatrix <- function(y) {
                x <<- y
                s <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve() function returns a matrix that is the inverse of 'x' as defined by makeCacheMatrix.
## If the inverse has not yet been computed (i.e. s = NULL), it will compute it and return it
## as the result. 
## If the inverse has already been computed (i.e. s is not NULL) then it will retrieve it from 
## the cache without computing it again. 
## I have added the two 'print' commands to let the user know if, upon running cacheSolve, they are 
## retrieving cached data or calculating new data.  


cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                print("getting cached data")
                return(s)
        }
        data <- x$getmatrix()
        s <- solve(data, ...)
        x$setinverse(s)
        print("calculating new data")
        s    
}
