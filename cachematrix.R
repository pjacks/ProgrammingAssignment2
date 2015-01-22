##############################################################
## Two functions to provide cache-enabled matrix solving
## To use, create an instance of the solving object by calling
## makeCacheMatrix, then call cacheSolve on this instance to 
## return either a calculated or cached solution.

## Create a set of functions to solve a matrix.
## Example usage: 
##  m<-matrix(c(1,7,2,3,1,6,0,4,1), nrow=3, ncol=3)
##  mcm<-makeCacheMatrix(m)
##  cacheSolve(mcm)             ## < calculated result
##  cacheSolve(mcm)             ## < cached result
##############################################################

##############################################################
## makeCacheMatrix creates the 'solving object' with a cached
## result
##############################################################
makeCacheMatrix <- function(baseMatrix = matrix()) {
    # variable to hold value of solution, initially null
    resultMatrix <- NULL
    # create a function to set values
    set <- function(y) {
        baseMatrix <<- y
        resultMatrix <<- NULL
    }
    # function to return the orignal value
    get <- function() baseMatrix
    
    # function to allow the cached value to be set
    setinverse <- function(solve) resultMatrix <<- solve
    # function to return the cached value (null on first call)
    getinverse <- function() resultMatrix
    
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

##############################################################
## Function to solve a matrix and return a cached result if 
## available
##############################################################
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Get the cached result from makeCacheMatrix
    result <- x$getinverse()
    ## If it is not null (i.e. cached), just return it,
    ## with a message
    if(!is.null(result)) {
        message("getting cached data")
        return(result)
    }
    ## Otherwise solve the matrix:
    ## Get the original matrix over which the solver was created
    data <- x$get()
    ## Do the calculation
    result <- solve(data, ...)
    ## Set the result in the cache
    x$setinverse(result)
    ## return the result
    result
    
}
