##Dustin Raymond
##Programming Assignemnt 2: Lexical Scoping

## This function is used to get values of a matrix, calculate the 
## inverse and save that value for later use
makeCacheMatrix <- function(x = matrix()) 
{
        ##Inverse of the matrix    
        m <- NULL
        
        ##Set the matrix object
        set <- function(y) 
        {
            x <<- y
            m <<- NULL
        }
        
        ##Returns the input Matrix
        get <- function() x
        
        ##Solve for the inverse save value to setInverse
        setInverse <- function(solve) m <<- solve
        
        ##Return the inverse value
        getInverse <- function() m
        
        ##List of functions used
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function checks if the inverse of the matrix has already be
## been calculated, if the value was not previously calculated 
## it is done so now and the value is returned and saved

cacheSolve <- function(x, ...) {
    
        ##Return a matrix that is the inverse of x
        m <- x$getInverse()
       
        ##If inverse was already calculated return that value 
        if(!is.null(m)) 
        {
            message("getting cached matrix")
            return(m)
        }
        
        ##Calculate inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        
    
        ## Return a matrix that is the inverse of 'x'
        m
}
