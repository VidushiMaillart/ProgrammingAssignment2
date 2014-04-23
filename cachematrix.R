## We calculate the inverse of a square invertible matrix 
## and cache the result. This saves unnecessary computations 
## since the stored result is automatically called upon request. 

##The function makeCacheMatrix is needed to 
# set and get the matrix as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        
        inv_x <- NULL           # inv_x is the inverse matrix
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv_x <<- solve
        getinv <- function() inv_x
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
        


## The function cacheSolve checks if the inverse of the 
## matrix is NULL. If yes, then it calculates the inverse. 
## If not, it returns the precalculated and cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv_x <- x$getinv()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        
        data <- x$get()
        inv_x <- solve(data)
        x$setinv(inv_x)        #sets the value of the inverse in the cache
        inv_x
}
