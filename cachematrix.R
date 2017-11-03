makeCacheMatrix <- function(x = matrix()) {
        
        ## x: my square invertible matrix
        ## returns: list containing functions to
        ## 1. set the matrix
        ## 2. get the matrix
        ## 3. set the inverse
        ## 4. get the inverse
        ## this list is used as the input to cacheSolve()
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                
        }
        
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        ## x: output of the function makeCacheMatrix()
        ## returns: inverse of the original matrix input to makeCacheMatrix() function
        
        inv <- x$getInverse()
        
        # if the inverse has already been calculated:
        
        if (!is.null(inv)) {
                # get it from the cache: 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, we calculate the inverse 
        mat <- x$get()
        inv <- solve(mat, ...)
        # setting the value of the inverse in the cache, via setinv function
        x$setInverse(inv)
        inv
}

