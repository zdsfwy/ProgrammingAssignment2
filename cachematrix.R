## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this block creates a special matrix that is able to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 
    set <- function(new_x) {
        x <<- new_x
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(new_inv) inv <<- new_inv
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
# this block creates an object that is used to access the special matrix
# to get its inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("get cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
