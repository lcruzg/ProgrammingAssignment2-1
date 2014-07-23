## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function makeCacheMatrix defines a way to set and get variables.
##In this particular case the inverse of the matrix is calculated in the function
##cacheSolve and the values are set in makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
      
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        
        get <- function() x
        ## Definition of variable setInverse
        setInverse <- function(inv) m <<- inv
        ## Return m
        getInverse <- function() m
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##Check if there is already a value for the inverse, if it exist return it
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##solve de Matrix
        m <- solve(x$get())
        ## pass value of inverse 
        x$setInverse(m)
        ##Print Inverse
        m
        
}
