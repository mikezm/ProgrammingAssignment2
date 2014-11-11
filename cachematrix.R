## Creates a matrix that is capable of setting and getting it's value and inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInv <- function(solve) m <<- solve
	getInv <- function(solve) m
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Gets the inverse of a matrix made with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'	
	m <- x$getInv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInv(m)
	m
}
