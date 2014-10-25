## There are two functions written below
## (1) makeCacheMatrix : Creates a special matrix object (list of functions)
##                       that has the capability to access cache the inverse
##                       of a given input matrix
##
## (2) cacheSolve : This is the function that accepts the special matrix object
##                  and returns the inverse of the matrix. If the inverse is
##                  available in the cache, it'll return the inverse from the
##                  cache. If it's not available, it recalculates the inverse
##                  and returns the inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ##Initialize the inverse to NULL
        inverse <- NULL  
        
        ## When a new matrix is assigned, inverse is re-initialized to NULL
        set <- function(matrix)
        {
                x <<- matrix 
                inverse <- NULL
                
        }
        
        ##Simply returns the matrix that was intialized
        get <- function() x 
        
        ##This function should be mainly used by cacheSolve function
        ##Basically, used to set the variable "inverse"
        setInverse <- function(inv) 
        {
                inverse <<- inv
        }
        
        ##Simply returns the variable, inverse
        getInverse <- function() inverse
        
        ##The following list is returned when the function makeCacheMatrix is called, which
        ##contains the fullowing 4 functions
        list(get=get, set=set, setInverse = setInverse, getInverse=getInverse)
        
}


## This is the function that accepts the special matrix object
## and returns the inverse of the matrix. If the inverse is
## available in the cache, it'll return the inverse from the
## cache. If it's not available, it recalculates the inverse
## and returns the inverse.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        
        ##If inverse is not null, that means it's available in the cache
        if(!is.null(inverse))
        {
                message("Getting cached matrix")
                return(inverse)
        }
        
        ##The following part gets executed when inverse is not available in cache
        #########################################################################
        
        ##Retrieving the matrix for which inverse needs to be calculated
        data <- x$get()
        
        ##Adding this additional condition to see if the determinant is zero
        ##If det() is zero then such a matrix doesn't have an inverse
        if(det(data)==0)
        {
                message("Inverse doesn't exist for this matix, determinant is zero")
                inverse = NaN
        }
        else
        {       ##solve function calculate the inverse of a matrix
                inverse <- solve(data,...)
        }
        
        ##setting the inverse variable with the calculated value.
        x$setInverse(inverse)
        
        ##return the calcluated inverse value (not from the cache)
        inverse
}
