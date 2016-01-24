## Processing Matrix Inverses can ressource intensive. These 2 functions define a new matrix object which  
## allows for its inverse to cached. These function assume that the matrix is invertible 

## makeCacheMatrix defines a new matrix object and its 4 functions to handle it
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # define/assign a new matrix to our new matrix object
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Return/display the matrix stored in the new matrix object
    get <- function() x
    
    # Sets the inverse parameter of the new matrix object
    setinverse <- function(inverse) inv <<- inverse
    
    # Return/Display the inverse parameter of the new matrix object
    getinverse <- function() inv
    
    # Return the new matrix object as a list of 4 functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Given a object returned by the makeCacheMatrix function, process the inverse of the matrix, cache it and returns it
cacheSolve <- function(x, ...) {
    
    ## Get the inverse parameter of the 'makeCacheMatrix' object 
    inv <- x$getinv()
    
    # if was set previously, return the cached value
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    #otherwise process the matrix inverse, cache it in the 'makeCacheMatrix' object and return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
