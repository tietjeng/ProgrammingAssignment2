## Following 2 functions will allow user to cache the inverse of
## a provided matrix

## This function creates a list of functions to set, get, setinverse,
## getinverse of a provided matrix.
## set - sets 2 parent variables available to other functions
## get - returns parent variable x
## setinverse - sets parent variable m to inverse available to other functions
## getinverse - returns parent variable m

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<-NULL
	}
	get <- function()x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set=set, get=get, 
		setinverse = setinverse,
		getinverse = getinverse) 
}


## Function returns inverse of provided matrix
## if inverse has already been cached, it will return parent variable without
## performing any logic.  If not cached, it will call appropriate functions
## to set inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
}
