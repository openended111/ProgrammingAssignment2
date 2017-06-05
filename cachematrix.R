## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## creating the function that would take a matrix as an input and enable it to cache its own object

makeCacheMatrix <- function(x = matrix()) {  ##defines the input (with default matrix)
  
    inv <- NULL
    set <- function(y) {    ##set the matrix
      x <<- y
      inv <<- NULL
    }
    
    get <- function() x     ##get the matrix
    setinv <- function(inverse) inv <<- inverse  ##set invertible matrix
    getinv <- function() inv                     ##get invertible matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Write a short comment describing this function

## Takes the result of the makeCacheMatrix function and does the following:
## 1.  If the resulting matrix is not empty, it prints the message of "getting cashed data"
## and captures the cached object
## 2.  If it is empty, by using the solve function, it sets the invertible matrix and returns the value


cacheSolve <- function(x, ...) {
  x$getinv()
  if(!is.null(inv)) {                           ##if the inverse matrix is not null
    message("getting cached data")              ##Print "getting cashed data" and return the value
    return(inv)
  }
  data <- x$get()                               ##otherwise, get the orignal matrix
  inv <- solve(data, ...)                       ##calculate, set and return the inverse the matrix
  x$setinv(inv)
  inv
}
