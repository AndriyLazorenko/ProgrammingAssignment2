## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function is a list containing a function to
#set the value of a matrix
#get the value of a matrix
#set the value of a matrix inverse
#get the value of a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function () x
    setinv <- function (invers) inv <<- invers
    getinv <- function () inv
    list (set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

#Function checks whether the inverse of a matrix is already
#calculated. If so, it gets the inverse from the cache and 
#skips the computation. Otherwise, it calculates the inverse of
#a matrix and stores it using setinv method from makeCacheMatrix
#function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message ("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve (data)
    x$setinv(inv)
    inv
}
