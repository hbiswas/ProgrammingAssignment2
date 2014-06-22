## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set  <- function(y){
                x <<- y
                m <<- NULL 
        }
        get  <- function() x
        setinverse  <- function(inverse) m  <<- inverse ## Gets the inverse of the matrix in m
        getinverse  <- function() m
        list(set= set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        m  <- x$getinverse()
        if (!is.null(m)){
                message("getting cached data") ##get m  from cache
                return(m)
        }
        data  <- x$get()
        m  <- solve(data, ...) ##calculate inverse otherwise
        x$setinverse(m)
        m
}
