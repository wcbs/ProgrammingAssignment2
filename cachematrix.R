## The first function, makeCacheMatrix, creates a special matrix that can cache 
## its inverse. It contains a list of a function that does
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, cacheSolve, calculates the inverse of the matrix created
## with the previous function by
# 1) firstly checks to see if the inverse has already been calculated and stored 
#    in the cache. If so, it retrieves and returns the value of the inverse from 
#    the cache. The computation is skipped.
# 2) If not, it computes the inverse and use "setinverse" function to set the 
#    value of the inverse in the cache. Then returns the value of the inverse.

cacheSolve <- function(x, ...) {
        
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}