## R Programming Assignment 2
## Gerald McGarvin April 3, 2017
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(origX = matrix()) {
	invX <- NULL					## clear variable when cache is instantiated.
	setOrig <- function(origX) {	## set function used to change origX and clear invX in cache.
		x <<- origX					## override x in parent env with new value.
		invX <<- NULL				## since origX has changed, previous value of invX must be removed.
	}
	getOrig <- function() origX		## return value of the original matrix to caller.
									## was set when makeCacheMatrix object was instantiated or setOrig was called.
	setInv <- function(passedInv) invX <<- passedInv  ## set invX to value passed to setInv().
	getInv <- function() invX		## return value of the inverse matrix to caller.

	## Return named list of setter and getter functions to caller:
	list( setOrig = setOrig, getOrig = getOrig, setInv = setInv, getInv = getInv)
}


## Call cacheSolve passing in a makeCacheMatrix object.
## Return the inverse of the matrix, origX already in the makeCacheMatrix object.
## Call getters and setters in makeCacheMatrix as needed.

cacheSolve <- function(x, ...) {	## pass a makeCacheMatrix object.
		invX <- x$getInv()  	## attempt to get cached inverse of x, the original matrix.
		if(!is.null(invX)) {	## not NULL means cache provided inverse of x.
			message("Retrieved inverse of matrix from cache")
			return(invX)		## exit function and return invX, the matric inverse.
		}
		invX <- solve(x$getOrig()) ## only executed if invX is NULL. Determine inverse.
		x$setInv(invX)			## set inverse in cache in case needed later.
		invX					## return inverse of X to the caller.
}
