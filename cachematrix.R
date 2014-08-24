## Program Assignment 2: Caching the Inverse of a Matrix
## rprog-006 - Coursera
## Author: Ariful Mondal
## August 2014
##---------------------------------------------------------------------
##Motivation:
##Caching matrix inversion coputation because Matrix inversion is usually a costly 
## computation and their may be some benefit to caching the inverse of a matrix 
##rather than compute it repeatedly
##---------------------------------------------------------------------
## Program Name: cachematrix.R
## Required: makeCacheMatrix & cacheSolve
## Assumptions: This program assumes all matrices invertible
##---------------------------------------------------------------------

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) minv <<- inv
        getinv <- function() minv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##above. If the inverse has already been calculated (and the matrix has not changed), then the
##cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Checking availability of the inverse of matrix x (minv) computed in earlier run.
        minv <- x$getinv()
        if(!is.null(minv)) {
                message("getting inverse from cached data")
                return(minv)
        }
        data <- x$get()
        ## Calculate inverse using solve()
        minv <- solve(data, ...)
        x$setinv(minv)
        
        ## Return a matrix that is the inverse of 'x'
        minv
}


##----------------------------------------------##
## How to use cacheSolve and  makeCacheMatrix

##myMat<-matrix(rnorm(16), 4)
##myMat<- matrix(c(1,0,0,2),2)
## Caching matrix
##chMat = makeCacheMatrix(myMat)
## Calculte Inverse and put in Cache
##chMat$get()
## Run1
##cacheSolve(chMat)

## Run 2
##myInvMat<-cacheSolve(chMat)
##myInvMat
##  Check
##myInvMat%*%myMat
##----------------------------------------------##



