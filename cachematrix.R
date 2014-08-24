## Program Assignment 2
## rprog-006 - Coursera
## Author: Ariful Mondal
## August 2014
##-------------------------------------------------------

## cachematrix.R
## 
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinv()
        if(!is.null(minv)) {
                message("getting inverse from cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
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



