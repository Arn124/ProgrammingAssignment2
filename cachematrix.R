## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function is used to create a special "matrix" object that can cache its inverse
##I first used a variable to store the variable of the matrix, which is initially stored as null
## I used another function to set values of the matrix which would get the value of the matrix, then set the value inverse of the matrix and get the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}




cacheSolve <- function(x, ...) {
## This checks if the inverse has already been calculated
inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
## If it isn't cached then compute using the function below   
  data <- x$get()
  inv <- solve(data, ...)
  
  x$setInverse(inv)
  
  inv
        ## Return a matrix that is the inverse of 'x'
}
