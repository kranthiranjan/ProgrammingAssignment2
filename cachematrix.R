## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(matrix)
        {
                x <<- matrix
                inverse <- NULL
                
        }
        
        get <- function() x
        
        setInverse <- function(inv)
        {
                inverse <<- inv
        }
        
        getInverse <- function() inverse
        list(get=get, set=set, setInverse = setInverse, getInverse=getInverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse))
        {
                message("Getting cached matrix")
                return(inverse)
        }
        
        data <- x$get()
        if(det(data)==0)
        {
                message("Inverse doesn't exist for this matix, determinant is zero")
                inverse = NaN
        }
        else
        {
                inverse <- solve(data,...)
        }
        x$setInverse(inverse)
        inverse
}
