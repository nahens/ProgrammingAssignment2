# makeCacheMatrix function returns a list of functions
# 
# *  The makeCacheMatrix function creates a matrix or returns the matrix via the following functions:
# * setMat      	set the value of a matrix
# * getMat      	get the value of a matrix

# * The below functions performs the Inverse or returns the cached Matrix
# * cacheInverse   	
# * getInverse     	
#  
# 
# makeCacheMatrix function
makeCacheMatrix <- function(x = numeric()) {
        
        # stores the cached value or NULL if not cached yet
        # initially value is NULL

        cache <- NULL
        
        # store a matrix
        setMat <- function(newValue) {
                x <<- newValue
                # since the matrix is assigned a new value, flush the cache
                cache <<- NULL
        }

        # returns the stored matrix
        getMat <- function() {
                x
        }

        # cache the given argument 
        cacheInverse <- function(solve) {
                cache <<- solve
        }

        # get the cached value
        getInverse <- function() {
                cache
        }
        
        # return a list. Each named element of the list is a function
        list(setMat = setMat, getMat = getMat, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(y, ...) {
        # get the cached value
        inverse <- y$getInverse()

        # Check if the cached value exists; if so then return the cached matrix
        if(!is.null(inverse)) {
                message("Retrieving the cached data")
                return(inverse)
        }

        # caclulate the inverse and cache it

        data <- y$getMat()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # return the inverse
        inverse
}
