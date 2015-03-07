## The functions, makeCacheMatrix and cacheSolve, are used cache potentially time-consuming 
## inverse matrix computations. 
## It uses function makeCacheMatrix to create a list functions and uses cacheSoft function
## to access and compute the inverse matrix.


## The makeCacheMatrix function creates a special "matrix", which is really a list containing 
## a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse matrix
##    get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) inv <<- inverse
        
        getInverse <- function() inv
  
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## calculates the inverse matrix of the special "matrix" created with the makeCacheMatrix function.
## The function first checks to see if the inverse matrix has already been calculated.
## If so, it gets the inverse matrix from the cache and skips the computation. Otherwise, it 
## calculates the inverse matrix of the original matrix and sets the value of the inserse in 
## the cache via the setInverse function.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
