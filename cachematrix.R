## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                       #the inverse is NULL by now
    set <- function(y) {
        x <<- y
        i <<- NULL             #if a new value is entered, i must be NULL again
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve  #calculate inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)  #the result of makeCacheMatrix is a list
                                    #of functions
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {           #if there is stored an inverse matrix return it
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)     #calculate inverse and store it in cache
    x$setinverse(i)
    i
    ## Return a matrix that is the inverse of 'x'
}
