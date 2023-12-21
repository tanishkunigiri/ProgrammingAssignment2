## Function: makeCacheMatrix
## Description: This function creates a special "matrix" object that can cache its inverse.
## Parameters:
##   - matrix: Initial matrix (default is an empty matrix)
## Returns: A list of functions for setting and getting the matrix, setting and getting the cached inverse.

makeCacheMatrix <- function(matrix = matrix()) {
        inv <- NULL

        # Function to set the matrix
        set <- function(mat) {
        matrix <<- mat
        inv <<- NULL
        }

        # Function to get the matrix
        get <- function() matrix

        # Function to set the cached inverse
        setInverse <- function(inverse) inv <<- inverse

        # Function to get the cached inverse
        getInverse <- function() inv

        # Return a list of functions
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}

## Function: cacheSolve
## Description: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##              If the inverse has already been calculated (and the matrix has not changed), then
##              the cacheSolve function retrieves the inverse from the cache.
## Parameters:
##   - x: A special "matrix" object created by makeCacheMatrix
##   - ...: Additional parameters passed to the solve function
## Returns: The inverse of the matrix.
## Note: It also caches the inverse for future use.

# Function to compute the inverse of the special "matrix"
# If the inverse has already been calculated, retrieve it from the cache
cacheSolve <- function(cacheMatrix, ...) {
        inverse <- cacheMatrix$getInverse()

        # If the inverse is already cached, return it
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

        # If not, calculate the inverse using solve function
        matrix <- cacheMatrix$get()
        inverse <- solve(matrix, ...)

        # Cache the calculated inverse
        cacheMatrix$setInverse(inverse)

        # Return the inverse
        inverse
}
