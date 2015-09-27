## Catching the inverse of a matrix
## Assuming input matrix is always invertible.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL
        # Set the value of matrix
        set <- function(y) {
                x <<- y
                inversematrix <<- NULL
        }
        # Get the value of matrix
        get <- function() {
                x
        }
        # Set the value of inverse matrix
        setinverse <- function(inverse) {
                inversematrix <<- inverse
        }
        # Get the value of inverse matrix 
        getinverse <- function() {
                inversematrix
        }
        # Create the list
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversematrix <- x$getinverse()
        # Check if the inverse matrix already calculated
        if(!is.null(inversematrix)) {
                message("getting cached data")
                return(inversematrix)
        }
        # Otherwise calculate the inverse matrix
        data <- x$get()
        inversematrix <- solve(data)
        x$setinverse(inversematrix)
        inversematrix
}
