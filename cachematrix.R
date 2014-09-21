## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly. These two functions 


## Function to create special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	## Set inverse matrix m to null
	m <- NULL
	
	## Function to set new value of unsolved matrix x
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## Function to get value of unsolved matrix x
	get <- function() x
	
	## Function to set the solved matrix to m, ie cache m
	setInverse <- function(solve) m <<- solve
	
	## Function to get solved matrix m
	getInverse <- function() m
	
	## Return list of functions
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function to compute inverse of special matrix returned by makeCacheMatrix.
## If inverse has already been calculated, it is retrieved from the cache.
cacheSolve <- function(x, ...) {
	## m will be null if inverse of x not calculated and cached
	m <- x$getInverse()
	
	## If m does not return null, retrieve and return m
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	## If m is null, this means m has not been calculated and cached
	data <- x$get()
	## Solve for m
	m <- solve(data, ...)
	## Store m
	x$setInverse(m)
	## Return m
	m
}
