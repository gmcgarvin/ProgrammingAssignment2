## R Programming Assignment 2
## Gerald McGarvin April 3, 2017

## Call makeCacheMatrix to store a matrix and its inverse, if setInv is called.
## makeCacheMatrix expects a square matrix as input.
## It returns an R list object containing get and set functions provided by makeCacheMatrix.
## setOrig() can be called to define the original matrix in makeCacheMatrix.
## setInv() can be called to store the inverses of the original matrix in makeCacheMatrix.
## getOrig() returns the original matrix.
## getInv() returns the inverse of the original matrix.
## Test usintg the following statements in RStudio:
## > source("cachematrix.R")
## > m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## > my_matrix <- makeCacheMatrix(m1)  ## creates a makeCacheMatrix object containing m1 matrix.
## > cacheSolve(my_matrix)
## The first time the last statement is executed the inverse matrix is displayed.
## If repeated for the same input matrix, the inverse matrix is displayed along with the following message:
## "Retrieved inverse of matrix from cache"

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
## Call get and set functions in makeCacheMatrix as needed.

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
