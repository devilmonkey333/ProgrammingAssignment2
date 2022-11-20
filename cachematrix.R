
## makeCacheMatrix takes an invertible matrix as an input
## and returns a list containing a function to set, get, setinv, and getinv the values of the matrix
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse)inv <<- inverse
  getinv <- function(){
    inver<-ginv(x)
    inver%*%x
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns a matrix that is the inverse of 'x', an invertible matrix

cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    
    if(!is.null(inv)){
      print("retrieving cached inverse")
      return(inv)
    }
    
    data<-x$get()
    inv<-solve(data,...)
    x$setinv(inv)
    print("cacheing inverse matrix")
    inv
}
