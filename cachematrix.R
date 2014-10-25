## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        mInv <-NULL
        setMatrix <- function(y) {
                x <<- y
                mInv <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(Inv = matrix()) {mInv <<- Inv }
        getInverse <- function() mInv
        list(setMatrix=setMatrix, getMatrix=getMatrix, setInverset=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mInv <- x$getInverse()
        if(!is.null(mInv)) {
                message("geting cached Matrix")
                return(mInv)
        }
        message("no cache: solving for Inverse")
        matrix <- x$getMatrix()
        mInv <- solve(matrix)
        x$setInverse(mInv)
        mInv
}
