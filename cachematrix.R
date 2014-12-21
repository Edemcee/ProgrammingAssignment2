## The makeCacheMatrix and cacheSolve functions below cache and calculate the 
## inverse of a matrix
## Assumption: the inverse of the matrix supplied exits



## The makeCacheMatrix function generates a special "matrix". The special matrix
## is a list of functions that set and get the value of the matrix as well as
## set and get the its inverse

makeCacheMatrix <- function(x = matrix()) {
	I <- NULL
	set <- function(y){
		x <<- y
		I <<- NULL
 	}
	get <- function() return(x)
	setinverse <- function(inverse) I <<- inverse
	getinverse <- function() return(I)
	return(list(set = set,get = get,setinverse = setinverse,getinverse = getinverse))
}



## The cachesolve function calculates the inverse of the special "matrix"
## returned by the makeCacheMatrix function above. It first checks to see
## whether the inverse has already been calculated, if it has been claculated,
## then the function gets the inverse from the cache, if it has not been 
## calculated, the the function calculates it and sets its result in the cache 
## through the setinverse function

cacheSolve <- function(x, ...) {
	I <- x$getinverse()
	if(!is.null(I)){
		message("Getting cached data ...")
		return(I)
	}
	data <- x$get()
	I <- solve(data,...)
	x$setinverse(I)
	
        ## Return a matrix that is the inverse of 'x'
	return(I)
}
