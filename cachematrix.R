## Put comments here that give an overall description of what your
## functions do
these functions are used to cache the inverse of a matrix rather than compute it evry time. Matrix inversion is costly and these 2 functions make it easier
to work with matrix inversion.
## Write a short comment describing this function
#this function creates a list that has 4 other functions , it makes the variables private so that they are not accessible to others., the matrix object can 
##cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	Xinv <- NULL #store the inversion, null as default
	set <- function(y){
		x <<- y
		Xinv <<- NULL
	}
	get <- function() x  #get the input matrix
	setInv <- function(inv)  Xinv <<- inv  #set the inverse matrix
	getInv <- function() Xinv #return the inverse matrix
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
##this function computes the inverse of the matrix returned by the main function(class) above, if the matrix has not changed(inverse has already been calculated)
##this function would fetch the inverse from the cache memory
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getInv()  
		if(!is.null(m){  #this is if it has been calculated get it
			message('getting cached data')
			return(m)
		}
		data <- x$get()  #get the matrix object
		m <- solve(data)
		x$setInv(m) #set the class/object to what solve calculated for us
}


