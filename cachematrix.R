## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    mInverse <- NULL
    set <- function(y) {
	    if(!identical(m,x)) {
		    m <<- x
			mInverse <<- NULL
		}
	}
	get <- function() m
	setInverse <- function(inverse) mInverse <<- inverse
	getInverse <- function() mInverse
	matrix(list(set, get, setInverse,getInverse),4,1,
	        dimnames = list(c("set","get","setInverse","getInverse")))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inverseM <- x[["getInverse",1]]()
	if (!is.null(inverseM)) {
		message("Getting the Inverse from Cache")
		return(inverseM)
	}
	matrixToInverse <- x[["get",1]]()
	inverseM <- solve(matrixToInverse)
	x[["setInverse",1]](inverseM)
	inverseM
}
