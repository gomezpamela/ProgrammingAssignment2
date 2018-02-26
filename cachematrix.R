

## The function makeCacheMatrix sets the input data to a matrix, gets the matrix, sets the value of the inverse, gets the value of the inverse. 

makeCacheMatrix <- function(x = matrix()){
	minv <- NULL
  set <-function(y) {
         x <<- y
         minv <<- NULL
      }
      get <-function ()x
      setinverse <- function(inverse)  minv <<- inverse
      getinverse <- function() minv
      list(set=set, get=get, 
           setinverse=setinverse,
           getinverse=getinverse)
   

}


## This function calculates the inverse of the square matrix (data) that was inputed if it hasn't been found cached data. 

cacheSolve <- function(x, ...) {minv <-x$getinverse()
  if(!is.null(minv)) {
          message ("getting cached data")
          return (minv)
  }
  data <- x$get()
  minv <-solve(data,...)
  x$setinverse(minv)
  minv
}
aMatrix<-makeCacheMatrix(matrix(1:4,2,2))
aMatrix$get()
cacheSolve(aMatrix)
        ## Return a matrix that is the inverse of 'x'
}
