## Put comments here that give an overall description of what your
## functions do

### Assignment: Caching the Inverse of a Matrix

## First, I am going to write a function called makeCacheMatrix to create 
## a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL     ## Set inv variable as NULL
    set <- function(y) {    ## Set value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x   ## Get value of the matrix
    setinverse <- function(inverse) inv <<- inverse   ## Sets value of the inverse
    getinverse <- function() inv     ## Gets the inverse
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the matrix returned
## by makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache, returning a message and skipping the computation. If not,
## it will compute the inverse with solve().

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()   ## Check if the inverse has been previously calculated
    if(!is.null(inv)) {  
        message("getting cached data") 
        return(inv)   ## Returns inverse of the cached matrix and display message
    }
    mat <- x$get()  
    inv <- solve(mat, ...)   ## Computes the inverse of the matrix with solve()
    x$setinverse(inv)   ##Sets the value of the inverse of the matrix
    inv   ## Returns the inverse of the matrix 'x'
}
