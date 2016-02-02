## Put comments here that give an overall description of what your
## functions do

## The overall objective of these two functions is to reduce computer processing time
##   for calculating inverse of a matrix with the help of a cache. Thus, whenever the results are
##   available in the cache, the program logic can bypass the processor intensive calculation and 
##   directly return the cached results. When the result is not available in the cache, the inverse of the 
##   matrix is computed and stored in the cache to avoid recomputation while running next time.

## Write a short comment describing this function

## The first function 'makeCacheMatrix() creates the cache. It has following four functions:
## 1. set: This function takes a matrix as an argunement and sets it to a matrix 'm' in its 
##               parent environment (which is in the execution envronment of makeCacheMatrix 
##               where the function was defined) through a deep assignment arrow operator.
## 2. get: This function returns the Matrix 'm' which was set by setMatrix previously
## 3. setinverse: This function sets the inverse of matrix 'm' in its parent environment 
##               through deep assignment arrow operator (similar to setMatrix)
## 4. getinverse: This function returns the inverse of the matrix set previously by setInverse 
##                function.
## The function also takes the matrix 'm' (which also can be set using setMatrix) as an arguement
## The function returms functions setMatrix, getMatrix, setInverse and getInverse as a list so that
##    these can be called by the '$' notation on the returned object which captured the list returned
##    makeCacheMatrix

makeCacheMatrix <- function(m = matrix()) {
        im <- NULL
        set <- function(mx = matrix()) {
                m <<- mx       ## Deep assignment arrow operator to modify 'm' in the parent environment
                im <<- NULL
        }
        get <- function() m    ## returns original matrix
        setinverse <- function(inverse_mat = matrix())  im <<- inverse_mat
                               ## Deep assignment arrow operator to modify 'im' in the parent environment
        getinverse <- function() im    ## returns inverse matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
                               ## return functions as list
}

## Write a short comment describing this function

## The second function 'cacheSolve' takes an object created by makeCacheMatrix function and by
##    calling getinverse(), it first checks whether the inverse exists (or it is NULL)
## If the inverse does exist and the inverse matrix 'm_inverse' is not NULL, it prints a message that
##    it is returning the result from cache and retruns the same and stops execution. Actual
##    calculation using solve() function is avoided in this case.
## However, if getinverse() returns NULL, control logic bypasses the 'if' loop and calculates
##    the inverse of the matrix using solve() function and also prints a message.
## Then 'cacheSolve' calls the setinverse function of the makeCacheMatrix object and sets the 
##    inverse matrix using a deep assignemnt arrow operator in the parent environment of
##    setInverse, so that in the next call of cacheSolve, the inverse matrix can be returned
##    from the cache without calculating its value.


cacheSolve <- function(mat, ...) {
        m_inverse <- mat$getinverse()   ## m_inverse captures the makeCacheMatrix object
        if(!is.null(m_inverse)) {       ## checks if the Inverse matrix in makeCacheMatrix is NULL
                message("getting cached data")
                return(m_inverse)
        }
        m <- mat$get()                  ## gets the original matrix by calling get()
        message("No cache: calculating inverse")
        m_inverse <- solve(m, ...)      ## Inverse computation
        mat$setinverse(m_inverse)       ## sets inverse Matrix by calling set()
        m_inverse                       ## returning Inverse Matrix
}

## Running of the functions:
##   > h1 <- matrix(c(1,0,0,2), nrow=2)
##   > h1
##        [,1] [,2]
##   [1,]    1    0
##   [2,]    0    2
##   > h <- makeCacheMatrix(h1)
##   > h$get()                         ## to verify that matrix h1 is captured in m
##         [,1] [,2]
##   [1,]    1    0
##   [2,]    0    2
##   > h_i <- cacheSolve(h)            ## h_i holds the inverse matrix returned by 'cacheSolve'
##   No cache: calculating inverse     ## This message indicates that there is no cache and the 
##                                     ##   inverse is calculated using solve()
##                                     ## This is correct since this is the first time the
##                                     ##   the function is run and there is no cache available
##   > h_i
##         [,1] [,2]
##   [1,]    1   0.0
##   [2,]    0   0.5
##   > h1 %*% h_i                      ## Actual matrix multiplication to test the inverse mathematically
##         [,1] [,2]
##   [1,]    1    0
##   [2,]    0    1                    ## Matrix multiplication gives unit matrix demonstrating mathematical correctness

##   > h_i2 <- cacheSolve(h)           ## Running cacheSolve for the second time 
##   geting cached data                ## This time the inverse matrix is fetched from cache
##   > h_i2                            ## and recomputation is avoided
##         [,1] [,2]
##   [1,]    1   0.0
##   [2,]    0   0.5
##   > identical(h_i, h_i2)            ## First and second run results (h_i and h_i2) are identical
##   [1] TRUE
