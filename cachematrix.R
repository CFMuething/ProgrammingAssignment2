# CFMuething

# Part 1: makeCacheMatrix: 
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # sets the inverse matrix to NULL as a placeholder for a future value
  
  set <- function(y){
    x <<- y 
    i <<- NULL #defines a function to set the matrix x to a new matrix y and reset the matrix i to NULL
  }
  
  get<-function() x
  setinv<-function(solve) i<<- solve # setinv overrides the previous value of i
  getinv<-function() i
  
  # creates a list of the functions
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# Part 2: cacheSolve
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), then 
# cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinv() 
  if(!is.null(i)){
    message("getting cached data")
    return(i)   
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i 
}