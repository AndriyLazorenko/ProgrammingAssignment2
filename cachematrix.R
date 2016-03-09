## Put comments here that give an overall description of what your
## functions do
# These functions handle caching matrices when computing inverses.
#
# 1st creates a special object (list) given original matrix that has methods
# for reading and writing a matrix to it as well as reading and writing
# inverse of a matrix from it (kinda new class)
#
# The 2nd function knows how to deal with this object to compute inverse
# either from cache of 1st function's object, or using solve() function,
# if the inverse of a matrix is not cached yet.



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
    inv <- solve (data, ...)
    x$setinv(inv)
    inv
}
