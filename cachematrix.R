## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly (there are also alternatives to matrix inversion that 
## we will not discuss here). Our goal is to write a pair of 
## functions that cache the inverse of a matrix.

## The makeCacheMatrix function will create a special "matrix" object
## and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## set the matrix 
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    ## get the matrix
    get <- function() x
    ## set the inverse of the matrix
    set_inv <- function(solve) inv <<- solve
    ## get the inverse of the matrix
    get_inv <- function() inv
    ## create a special list containing 4 functions above
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## The cacheSolve function will compute the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x) {
    inv <- x$get_inv()
    ## check if the inverse has already been calculated
    if(!is.null(inv)){
        message("Get the cached data successful!")
        return(inv)
    }
    ## calculate the inverse
    data <- x$get()
    inv <- solve(data)
    ## write the inverse into the memory
    x$set_inv(inv)
    inv
}
