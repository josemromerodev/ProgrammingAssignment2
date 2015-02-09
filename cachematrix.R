## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        # initialize inverted matrix in memory
        inverseMatrix <- NULL
        
        # Create fuction to set the matrix
        set <- function(y) {
                # Save the passed in value as the matrix
                x <<- y
                # Since we are changing the matrix, clear the cache
                inverseMatrix <<- NULL
        }
        # Returns the current matrix
        get <- function() x
        # Save the cached inverted matrix
        setinverse <- function(i) inverseMatrix <<- i
        # Get the cached inverted matrix
        getinverse <- function() inverseMatrix
        # Saves the functions in a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Get the cached inverted matrix
        inverseMatrix <- x$getinverse()
        # Check if the cache is empty
        if(!is.null(inverseMatrix)) {
                # Let user know that we will be using cached data
                message("getting cached data")
                # return cached inverted matrix
                return(inverseMatrix)
        }
        # Since the cache is empty, get the matrix
        data <- x$get()
        # Invert it...
        inverseMatrix <- solve(data, ...)
        # Save the result in cache
        x$setinverse(inverseMatrix)
        # return the cached inverted matrix
        inverseMatrix
}