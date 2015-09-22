## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse.

## The makeCacheMatrix function creates a special 'matrix', which is really
## a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                ## store a new value for the matrix and
                ## clear the previous inverse from cache
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invx <<- inv
        getinv <- function() invx
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}

## The cacheSolve function calculates the inverse of the special "matrix" created
## with the makeCacheMatrix function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invx <- x$getinv()
        if(!is.null(invx)) {
                ## inverse has already been calculated and cached; return that value 
                message("getting cached inverse")
                return(invx)
        }
        ## inverse has not been calculated, calculate it, cache it, and return that value
        data <- x$get()
        invx <- solve(data, ...) ## for the exercise, assume that matrix has inverse 
        x$setinv(invx)
        invx
}
