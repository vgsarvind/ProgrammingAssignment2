## These functions construct a special matrix object and return the inverse value of a matrix object respectively. 
## Since matrix inversion is costly, we cache the inverse value of a matrix once it has been computed 
## and return the cached value in case the inverse is asked for again (provided there is no change to the original matrix)

## This function provides a list of functions which do the following:
## 1. set: Sets the value of a matrix
## 2. get: Gets the value of a matrix
## 3. setCachedMatrix: Caches the value of a matrix
## 4. getCachedMatrix: Fetches the cached value of a matrix

makeCacheMatrix <- function(matrix = matrix()) {
        cachedMatrix <- NULL
	
	## To set the value of a matrix
        set <- function(newMatrix) {
                matrix <<- newMatrix
                cachedMatrix <<- NULL
        }
	
	## To get the value of a matrix
        get <- function() matrix
	
	## To set the value of a cached matrix
        setCachedMatrix <- function(matrixToCache) cachedMatrix <<- matrixToCache
	
	## To get the value of a cached matrix
        getCachedMatrix <- function() cachedMatrix

	## List of operations in "makeCacheMatrix" class
        list(set = set, get = get,
             setCachedMatrix = setCachedMatrix,
             getCachedMatrix = getCachedMatrix)
}

## This function returns the inverse of a matrix.
## For doing so, it fetches the cached inverse value in case it is already available
## and if not, computes the inverse and caches it so that it can be used in the future

cacheSolve <- function(matrix, ...) {
        cachedMatrix <- matrix$getCachedMatrix() ## Look for a cached matrix
        if(!is.null(cachedMatrix)) {
                message("Getting cached data")
                return(cachedMatrix) ## Fetch the cached inverse value
        }
	## If no cached value is available, 
        data <- matrix$get()
        matrixToCache <- solve(data, ...) ## Compute the inverse of the "data" matrix
        matrix$setCachedMatrix(matrixToCache) ## Cache the newly computed value
        matrixToCache
}

