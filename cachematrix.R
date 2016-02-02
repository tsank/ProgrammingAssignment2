## Put comments here that give an overall description of what your
## functions do

## The overall objective of these two functions is to reduce computer processing time
##   for calculating inverse of a matrix with the help of a cache. Thus, whenever the results are
##   available in the cache, the program logic can bypass the processor intensive calculation and 
##   directly return the cached results. When the result is not available in the cache, the inverse of the 
##   matrix is computed and stored in the cache to avoid recomputation while running next time.

## Write a short comment describing this function

## The first function 'makeCacheMatrix() creates the cache. It has following four functions:
## 1. setMatrix: This function takes a matrix as an argunement and sets it to a matrix 'x' in its 
##               parent environment (which is in the execution envronment of makeCacheMatrix 
##               where the function was defined) through a deep assignment arrow operator.
## 2. getMatrix: This function returns the Matrix 'x' which was set by setMatrix previously
## 3. setInverse: This function sets the inverse of matrix 'x' in its parent environment 
##               through deep assignment arrow operator (similar to setMatrix)
## 4. getInverse: This function returns the inverse of the matrix set previously by setInverse 
##                function.
## The function also takes the matrix 'x' (which can be set using setMatrix) as an arguement
## The function returms functions setMatrix, getMatrix, setInverse and getInverse as a list so that
##    these can be called by the '$' of the returned object capturing the list.

makeCacheMatrix <- function(x = matrix()) {
        mInv <-NULL
        setMatrix <- function(y) {
                x <<- y         ## Deep arrow assignment operator to modify 'x' in the parent environment
                mInv <<- NULL   
        }
        getMatrix <- function() x
        setInverse <- function(Inv = matrix()) {mInv <<- Inv }
                                ## Deep arrow assignment operator to modify mInv in the parent environment
        getInverse <- function() mInv
        list(setMatrix=setMatrix, getMatrix=getMatrix, setInverset=setInverse, getInverse=getInverse)
        ## output as list
}


## Write a short comment describing this function

## The second function 'cacheSolve' takes an object created by makeCacheMatrix function and by
##    calling getInverse(), it first checks whether the inverse exists (or it is NULL)
## If the inverse does exist and the inverseMatrix mInv is not NULL, it prints a message that
##    it is returning the result from cache and retruns the matrix and stops execution. Actual
##    calculation using solve() function is avoided in this case.
## However, if getInverse() returns a NULL mInv object, it bypasses the 'if' loop and calculates
##    the inverse of the matrix using solve() function.
## Then 'cacheSolve' calls the setInverse function of the makeCacheMatrix object and sets the 
##    inverse matrix using a deep assignemnt arrow operator in the parent environment of
##    setInverse, so that in the next call of cacheSolve, the inverse matrix can be returned
##    from the cache without calculating its value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mInv <- x$getInverse()
        if(!is.null(mInv)) {     ## checking if the Inverse matrix in makeCacheMatrix is NULL
                message("geting cached Matrix")
                return(mInv)     ## returnng mInv from cache
        }
        message("no cache: solving for Inverse")
        matrix <- x$getMatrix()  ## getting the Matrix to be inversed by calling getMatrix
        mInv <- solve(matrix)    ## Inverse computation
        x$setInverse(mInv)       ## setting inverse Matrix by calling setMatrix
        mInv                     ## returning Inverse Matrix
}

## Running of the functions:
##   > h1 <- matrix(c(1,0,0,1), nrow=2)
##   > h1
##        [,1] [,2]
##   [1,]    1    0
##   [2,]    0    1
##   > h <- makeCacheMatrix(h1)
##   > h$getMatrix()
##         [,1] [,2]
##   [1,]    1    0
##   [2,]    0    1
##   > h_i <- cacheSolve(h)            ## h_i holds the inverse matrix returned by 'cacheSolve'
##   no cache: solving for Inverse     ## This message indicates that there is no cache and the 
##                                     ##   inverse is calculated using solve()
##                                     ## This is correct since this is the first time the
##                                     ##   the function is run and there is no cache available
##   > h_i
##         [,1] [,2]
##   [1,]    1    0
##   [2,]    0    1
##   > h1 %*% h_i                      ## Actual matrix multiplication to test the inverse mathematically
##         [,1] [,2]
##   [1,]    1    0
##   [2,]    0    1                    ## Matrix multiplication gives unit matrix demonstrating mathematical correctness

##   > h_i2 <- cacheSolve(h)           ## Running cacheSolve for the second time 
##   geting cached Matrix              ## This time the inverse matrix is fetched from cache
##   > h_i2                            ## and recomputation is avoided
##         [,1] [,2]
##   [1,]    1    0
##   [2,]    0    1
##   > identical(h_i, h_i2)            ## First and second run results (h_i and h_i2) are identical
##   [1] TRUE
