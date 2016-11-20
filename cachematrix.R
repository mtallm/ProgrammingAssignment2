



makeCacheMatrix <- function(x = matrix()) {
	## intialize the inverse property
        i <- NULL
        ## method to set the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## method to get the matrix
        get <- function() x
       
        ## method to set the inverse of the matrix
        
        setInverse<- function(inverse) {
        	i <<- inverse
        	
        	##method to get the inverse of a matrix
        getInverse <- function() i  
        ## returning the inverse property
        
        ##return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
	##return a matrix that is a inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## getthe matrix fronm our object
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        ##return the matrixs
        m
}
