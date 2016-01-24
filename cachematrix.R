## Processing Matrix Inverses can be ressource intensive. These 2 functions define a new matrix object which  
## allows for its inverse to be cached. These functions assume that the initial matrix is invertible 

## makeCacheMatrix defines a new 'makeCacheMatrix' matrix object, its paramerts and its 4 functions to handle it
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Define/assign a new matrix to the 'makeCacheMatrix' object
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Return/display the matrix stored in the 'makeCacheMatrix' object
    get <- function() x
    
    # Sets the inverse parameter of the 'makeCacheMatrix' object
    setinverse <- function(inverse) inv <<- inverse
    
    # Return/Display the inverse parameter of the 'makeCacheMatrix' object
    getinverse <- function() inv
    
    # Return the 'makeCacheMatrix' object as a list of 4 functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Given an object returned by the makeCacheMatrix function, process the inverse of the matrix, cache it and returns it
cacheSolve <- function(x, ...) {
    
    ## Get the inverse parameter of the 'makeCacheMatrix' object 
    inv <- x$getinv()
    
    # if was set previously, return the cached value
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # otherwise process the matrix inverse, cache it in the 'makeCacheMatrix' object and return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
