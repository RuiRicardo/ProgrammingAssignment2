## Put comments here that give an overall description of what your
## functions do
## 		makeCacheMatrix - creates a list object that contains a matrix and an inverte method
## 
## 		cacheSolve - inverts a matrix caching the result
## 

## Write a short comment describing this function
## Write a short comment describing this function
## 
## 		description: creates a special matrix object (list) with methods:
##						- set - defines matrix content
##						- get - gets matrix content
##						- setinvert - define/calculate inverted matrix
##						- getinvert - gets inverted matrix 
## 		
##		Parameters:	 x as matrix, must be a square invertible matrix
## 		
##		usage example:	 
## 			define matrix: 	m = makeCacheMatrix( rbind(c(1, 2, 3), c(0,4,5), c(1,0,6)) )
## 			get matrix content:	m$get()

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
	get <- function() { x }
	setinvert <- function(solve) { m <<- solve }
	getinvert <- function() { m }
	list(set = set, get = get,
		 setinvert = setinvert,
		 getinvert = getinvert)
}


## Write a short comment describing this function
## 
## 		description: calculate inverted matrix, caching it for future use
## 
## 		Parameters:	 x as makeCacheMatrix, must contain a square invertible matrix
## 		
##		returns:	 matrix object containing the inverse of the matrix
## 		
##		usage example:	 
##				m = makeCacheMatrix( rbind(c(1, 2, 3), c(0,4,5), c(1,0,6)) )
##				cacheSolve(m)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinvert()
    if(!is.null(m)) {
        message("getting cached data")
    }else{
		message("cache is empty...")
		data <- x$get()
		m <- solve(data)
		x$setinvert(m)
		message("... cache was filled")
	}
	return(m)
}
