## These functions handle memory issues while computing a matrix inverse. The issues are handled by caching the matrix inverse once it's computed. So whenever the running program needs the computed inverse again, it can take it just from the cache. 

## This function creates a list containing 4 functions: set(), get(), setinv(), getinv(). Two of them (set() and setinv()) are actually used to cache the matrix and matrix inverse state.

makeCacheMatrix <- function(X = matrix()) {
	inv <- NULL
	set <- function(Y) {
		X <<- Y
		inv <<- NULL
	}
	get <-  function() X
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set=set, get=get,
		 setinv = setinv,
		 getinv = getinv)
}


## This function calculates an inverse of the given matrix. If the inverse hasn't been calculated yet, it calculates and cache it. If the inverse has already been calculated, it takes it from the cache. 

cacheSolve <- function(X, ...) {
	inv <- X$getinv()
	if(!is.null(inv)) {
		message("getting cached data...")
		return(inv)
	}
	data <- X$get()
	inv <- solve(data, ...)
	X$setinv(inv)
	inv
}