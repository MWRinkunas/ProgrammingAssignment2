## This pair of functions cache the inverse of a matrix.
## Using these functions avoids having to compute 
## the inverse of a matrix repeatedly.


## The makeCacheMatrix function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
	  #define variable to set the value of the matrix
	  set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
	  #define variable to get the value of the matrix
	  get <- function() x
        
	  #define variable to set the value of the inverse
	  setinverse <- function(solve) inverse <<- solve

        
	  #define variable to get the value of the inverse
	  getinverse <- function() inverse
        
        ## Function to set the value of the matrix, get the 
        ## value of the vector, set the value of the inverse, 
	  ## and get the value of the mean
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special 
## "matrix" returned by the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
	  inverse <- x$getinverse()
       
        ## Checks to see if the inverse has already been calculated	
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
	  ## Return a matrix that is the inverse of 'x'
	  data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse

        
}
