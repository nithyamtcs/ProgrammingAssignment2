## There are couple of function in this file.
## makeCacheMatrix - It creates a special matrix with function objects.
## cacheSolve - It performs the matrix inverse using the special matrix passed as an argument

## It returns a special (4 X 1) matrix containing function as values.
## These functions are used to store & retreive a invertible matrix & its inverse from cache.
## The functions should be accessed something like m[["<func_name>",1]](...)
## It reset the matrix and its inverse in cache only if the new matrix is not identical

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


## This function uses the special matrix to perform the inverse of matrix.
## If the inverse already available in cache, it returns it
## Otherwise, performs the inverse using solve() method.

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
