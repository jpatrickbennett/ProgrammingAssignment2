## These functions creates a matrix that will allow for the caching of the inverse calculation.

## makeCacheMatrix allows you to create a Matrix that will allow for the cached calculation of it's Inverse.
## It returns a list that consists of functions to:
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the inverse
## 4) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix, 'x.' 
## Once the inverse is calculated, it stores it in the cache.
## If cacheSolve is called after the inverse was already calculated, it reads it from the cache. 

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        
        m  ## Return a matrix that is the inverse of 'x'
                
}
