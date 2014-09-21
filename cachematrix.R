## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly. These two functions 


## Function to create special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function to compute inverse of special matrix returned by makeCacheMatrix.
## If inverse has already been calculated, it is retrieved from the cache.
cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	
	## Return a matrix that is the inverse of 'x'
	m
}
