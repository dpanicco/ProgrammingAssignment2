## This are two functions that are used to create a special object that stores a square matrix and caches its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

## I have added some controls to verify if the argument of the function is a square matrix

makeCacheMatrix <- function(x = matrix()) {

        if(!is.matrix(x)) {
                stop("argument must be a square matrix")
        }
        
        if(nrow(x) != ncol(x)) {
                stop("matrix must be square")
        }
        
        m <- NULL

        set <- function(y) {
                message("I SET the matrix")
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m

        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse matrix in the cache via the setSolve function.

cacheSolve <- function(x, ...) {

        ## Try to get the inverse of the matrix 'x'

        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }

		## If don't find the inverse do the computation and cache the inverse matrix
		
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)

        m
}
