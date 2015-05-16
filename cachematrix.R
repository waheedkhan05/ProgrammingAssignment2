## Put comments here that give an overall description of what your
## functions do
# The code is inspired from the makeVector and cachemean functions given in
# the Assignment 2 page at Coursera. makeCacheMatrix provides getters and
# and setters for value of matrix and inverse of matrix.
# Matrix inversion is costly computation and there is benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used for this purpose.

## Write a short comment describing this function?
# As already explained in the overall description, makeCacheMatrix provides getters and
# and setters for value of matrix and inverse of matrix. get and set function
# are used for getting and setting the matrix value while setmatrix and 
# getmatrix are used from inverste of matrix
# The solve function is a generic function that solves the equation 
# a %*% x = b for x, where b can be either a vector or a matrix.

	makeCacheMatrix <- function(x = matrix()) {
		m<-NULL
		set<-function(y){
			x<<-y
			m<<-NULL
		}
		get<-function() x
		setmatrix<-function(solve) m<<- solve
		getmatrix<-function() m
			list(set=set, get=get,
			   setmatrix=setmatrix,
			   getmatrix=getmatrix)
	}


## Write a short comment describing this function?
# This function used the above function to calculate inverse of matrix
# and if the inverse has already been computed previously, it is retrieved
# from cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m<-x$getmatrix()
    if(!is.null(m)){
      message("getting inverse of matrix from cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}