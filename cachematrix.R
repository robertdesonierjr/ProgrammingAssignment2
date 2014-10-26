## This file contains two functions, makeCacheMatrix and cacheSolve that are 
## used to cache the inverse of a matrix.  The function solve is used to 
## find the inverse of the matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
                
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function () m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}

## This function fuction computes the inverse of the special "matrix" returned
## by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        
        ## Test to see if inverse has already been calculated
        if(!is.null(m)){
                message("getting cached data")
                return(m)
                
        }
        
        data <- x$get()
        
        ## Calculate the inverse of the data using the solve function
        m <- solve(data, ...)
        
        ## Set the value of the inverse matrix in the cache
        x$setinverse(m)
        m
        
}