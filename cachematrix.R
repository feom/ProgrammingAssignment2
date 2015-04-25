## Programming Assignment 2: Lexical Scoping - Caching the Inverse of a Matrix

## The makeCacheMatrix() function returns a list that contains 4 functions that
## cache and return a square invertible matrix and its inverse matrix.
##
## The cacheSolve() uses the list of the makeCacheMatrix() function to
## calcluate the inverse of an input matrix considering a potentially cached version.
## In case the inverse is cached, this cached version is returned and a new calcluation
## is avoided.
##
## The testCacheSolve function tests both functions described above by creating a
## square invertible matrix and testing if the cached version is used for a
## subsequent calculation.


## This function returns a list containing 4 functions to cache a matrix and its inverse.
##     set() Sets the input matrix and initializes the inverse variable
##     get() Returns the input matrix
##     setinverse() Cache the inverse of the input matrix
##     getinverse() Return the cached inverse of the input matrix
makeCacheMatrix <- function(x = matrix()) {
	
	# The cached inverse of the matrix 
	inv <- NULL
	
	# Set the value of the matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	# Get the value of the matrix
	get <- function() x
	
	# Set the value of the inverse of the matrix
	setinverse <- function(matrixInverse) inv <<- matrixInverse
	
	# Get the cached inverse of the matrix
	getinverse <- function() inv
	
	# the list that is returned
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}


## This function utilizes the list created by the 'makeCacheMatrix' function
## in order to calculate the inverse of an input matrix considering a possibly
## cached result from a precedent calculation.
##
## First, the function checks if a cached inverse is existing. If yes, this cached
## version is returned. Otherwise the inverse of the input matrix is calculated and
## the result is cached. Finally the inverse is returned.
##
## The assumption for this function is that the input matrix is always invertible
## and square in order to be a valid input for the solve function!
cacheSolve <- function(x, ...) {
        	
		# First get a possibly cached inverse of x
		inverseOfX <- x$getinverse()
		
		# Check if 'inverseofX' is not null; i.e. the inverse is cached due to a precedent calculation
		if(!is.null(inverseOfX)) {
			message("getting cached inverse of matrix")
			return(inverseOfX)
		}
		
		# Get the input matrix data
		data <- x$get()
		
		# Calculate the inverse of the input matrix
		inverseOfX <- solve(data, ...)
		
		# Set, i.e. cache the calculated inverse of x
		x$setinverse(inverseOfX)
		
		# Return the resulting inverse of matrix x
		return(inverseOfX)
}


# This function tests the makeCacheMatrix and cacheSolve functions.
# It creates a square test matrix.
##
## First the minput matrix is printed.
## Then the inverse is calculated and printed.
## Finally the calcluation is repeated to ensure the cached version is returned
## from the precedent calculation.
testCacheSolve <- function() {
	# The test matrix
	testMatrix = matrix(c(2, 4, 3, 1, 5, 7, 5, 8, 6), nrow=3, ncol=3)
	
	# Create the matrix list
	m <- makeCacheMatrix(testMatrix)
	
	# Print the input matrix
	print(m$get())
	
	# Calculate the inverse of the input matrix
	print(res <- cacheSolve(m))
	
	# Calcluate again -> this time the cached version is returned indicated via the respective message
	print(res <- cacheSolve(m))
}
