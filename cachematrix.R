## Put comments here that give an overall description of what your
## functions do

## This function creates a list of functions to set the value of a matrix, to get
## the value of a matrix, to set the value of the inversed matrix and to get
## the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function () i
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function will check if there is inverted matrix cashed and
## will only do the inversion if there is no cached one.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
##calculating if the matrix hasn't changed
    data <- x$get()
    n <- nrow(data)
    e <- diag(n) 
    if (identical(x%*%i,e)){
    message("getting cached data")
    return(i)
    }
  }
  i <- solve(x)
  x$setinverse(i)
  i
}
