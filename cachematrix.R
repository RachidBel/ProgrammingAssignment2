## Put comments here that give an overall description of what your
## functions do

## The  function, makeCacheMatrix creates a list containing a function to
## 1. set the value of the vector,
## 2. get the value of the vector,
## 3. set the value of the mean,
## 4. get the value of the mean.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function calculates the inverse matrix of the special list created with the above function.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the mean from the cache and skips the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
