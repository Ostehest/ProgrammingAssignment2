## To save time and computation resources I will make use of the scoping rules  
## of R to cache information/values for preservation of data in an environment
## where it is available for retrieval, thereby bypassing unnecessary recomputing.

## I will write two functions which create a special matrix that serves as a 
## list of functions to cache the inverse of a matrix and evaluate if recomputing
## is needed to obtain the inverse values of new inputs.

# The first function creates a matrix that serves as a list for at set of functions to:
# 1. Set the values of the matrix
# 2. Get the values of the matrix
# 3. Set the values of the inverse of the matrix
# 4. Get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i = NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# The second function retrieves the inverse value and checks if the inverse of
# the matrix has already been calculated and set in the cache (if it is not NULL). 
# If calculations have already been made, no further calculations are needed and 
# the inverse of the matrix will simply be retrieved from the cache.
# If the inverse of the matrix has not been calculated before (it is NULL), then
# the inverse is calculated and the value is set in the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("Getting cached data for the inverse values")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}