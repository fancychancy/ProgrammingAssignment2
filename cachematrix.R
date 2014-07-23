## makeCacheMatrix function takes a matrix as input and returns a list of 4 functions
## output. The get and set functions returned by this function are used to get and set
## value of a matrix and on the same lines getInverse and setInverse return and set and 
## the values of the inverse of the matrix. The <<- operator which can be used to assign 
## a value to an object in the parent environment

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getInverse <- function() inverse
    setInverse <- function(inv) inverse <<- inv
    list (get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


## cacheSolve function retrieves a previously calculated inverse value by calling a 
## function defined in the makeCacheMatrix function. If the value is missing, the 
## cacheSolve function calcuates the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse
    if(! is.null(inv)){
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInverse(inv)
    inv
}
