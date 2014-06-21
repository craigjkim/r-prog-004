##
## cachematrix.R
##
## Matrix inversion is a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
##

## makeCacheMatrix
## Class to cache inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
			setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	m <- x$getsolve()			# attempt to get the previous inverse
	if (!is.null(m)) {			# do we have the inversion?
		message('getting cached inverse')
		return(m)				# return the stored inverse
	}
	data <- x$get()				# the matrix to invert
	m <- solve(data, ...)		# invert the matrix
	x$setsolve(m)				# store the inverse
	m							# return the inverse
}
