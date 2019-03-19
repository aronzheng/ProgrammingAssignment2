## Caching the Inverse of a Matrix:
## Advantage of caching the inverse of matrix is do not repeated it again. 
## First, the program store the matrix, and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set<-function(y){
       x<<- y 
       inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## It is a cacheslove function computes the inverse of matrix from makeCacheMatrix funciton 

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

## Uesing a exam to test

matri<-matrix(c(1,2,3,4),2,2)         ## creat a  size of  2 x2  matrix. 
data<-makeCacheMatrix(matri)          ## plug into mackecache function 

data$get()            ## for the first function run out is a list, so, the data $get()  is run get() is x which is input matrix
data$getInverse()    ##   which is NUll because we didnot define the any value in this get inverse function
cacheSolve(data)     ##  run cacheSolve function to get a inverse of matrix 
cacheSolve(data)     ## run it agian, privious we got inverse then second time jump into if condation 
data$getInverse()    ## get inverse of matrix 
