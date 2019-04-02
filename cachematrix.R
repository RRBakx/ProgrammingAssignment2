## These functions are meant to calculate the inverse of a given matrix and cache the value for future reference. 
## If the matrix inverse has been calculated before, the cached value will be retrieved. If not, it will be calculated.

## This function will make the given matrix into a CacheMatrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y = matrix()) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) m <<- inv
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function will calculate the inverse of the matrix, or retrieve it from cache if calculated before.

cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setInverse(m)
	m
}

	