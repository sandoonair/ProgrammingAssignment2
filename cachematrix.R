## The following two functions are used to calculate the inverse of a matrix and store it in cache

## makeCacheMatrix takes a matrix as an input and creates a list with the matrix and its inverse which 
## stored in cache

makeCacheMatrix <- function(x = matrix(),...) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
                
        }
        
        get <- function() x
        setinverse <- function(inv) i<<-inv
        getinverse <- function() i
        list(set = set, get =get, setinverse = setinverse, getinverse=getinverse)
}


## This function calculates the inverse of the matrix and if it has already been calculated, 
## then fetch it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("Getting Cached Data")
                return(i)
        }
        data <- x$get()
        i<- solve(data,...)
        x$setinverse(i)
        i
}


