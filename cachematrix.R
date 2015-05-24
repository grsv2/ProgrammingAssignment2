## 
## This .R file  creates a cached variable in a "different environmnet"
## and uses the variable to reduce repetitive time-consuming computations.
##
## 
## SAMPLE SHOWING HOW PROGRAM CAN BE RUN
##
## mtrx<-matrix(c(1,1,0.2,0.2, 0.4,0,0,0, 0.4,0,2,2 ,6,0,3,0.8),4,4)
## > source("cachematrix.R");cmtrx<-makeCacheMatrix(mtrx);cacheSolve(cmtrx)
## finding inverse
## Storing value in different environment
## [,1] [,2]       [,3]       [,4]
## 1,] 0.000000e+00  1.0  0.0000000  0.0000000
## [2,] 2.500000e+00 -2.4 -6.6363636  6.1363636
## [3,] 1.387779e-17 -0.1 -0.1818182  0.6818182
## [4,] 0.000000e+00  0.0  0.4545455 -0.4545455
## > cacheSolve(cmtrx)
## getting cached data
## [,1]          [,2] [,3] [,4]
## [1,]  1.0  4.000000e-01  0.4  6.0
## [2,]  1.0  0.000000e+00  0.0  0.0
## [3,]  0.2 -1.110223e-17  2.0  3.0
## [4,]  0.2 -1.110223e-17  2.0  0.8
## > cacheSolve(cmtrx)
## getting cached data
## [,1]          [,2] [,3] [,4]
## [1,]  1.0  4.000000e-01  0.4  6.0
## [2,]  1.0  0.000000e+00  0.0  0.0
## [3,]  0.2 -1.110223e-17  2.0  3.0
## [4,]  0.2 -1.110223e-17  2.0  0.8


## This functiontakes a matrix and creates a data structure that 
## contains pointers to four function calls 
## and it stores the value to an object in an environment 
## that is different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  setv <- function(y) {
    message ("Storing the original matrix in different environment. Kill inverse value")
    x <<- y
    inv <<- null
  }

  getv <- function() x
  
  setvinv <- function(x) inv <<- solve(x)
  
  getvinv <- function() inv
  
  list(setv = setv, 
       getv = getv,
       setvinv = setvinv,
       getvinv = getvinv)
}


## This function calcuates and stores the inverse only when necessary

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getvinv()
     if(!is.null(inv)) {
         message("getting cached data")
         return(inv)
      }
     
      # GET THE oRIGINAL VALUE OF THE MATRIX
      data <- x$getv()
      message ("finding inverse")
      inv <- solve(data, ...)
      message ("storing inverse")
      x$setvinv(inv)
      inv
}

