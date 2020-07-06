## Put comments here that give an overall description of what your
## functions do

##Below are two functions that are used to create a special object that stores a matrix  and cache's its inverse.

## Write a short comment describing this function

## The function creates a special matrix, which is a list containing a function to 
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## The following function computes the inverse of the special matrix specified in the
## makeCacheMatrix function (above). The first step is to check if the inverse has 
## already been computed. If the inverse is already calculated, it gets the result from  
## the cache, skiping the computation.
## If the inverse has not been calculated, it computes the inverse and sets the result of
## the inverse in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
