## Put comments here that give an overall description of what your
## functions do

## this function creates and returns a list that contains 4 functions to set and 
## get values for a matrix and its inverse
## those functions can then be used in other code to cache and retrieve the value of the
## original matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    #initialize matrix inverse i
    i <- NULL
    #create function to set scope of variable representing original matrix
    #so that value inside and outside this function are 'synced'
    #which scopes x to the calling environment (accessible outside of this function)
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    #create function to return value of original (non-inverted) matrix
    get <- function() x
    #create function to set the value of the matrix inverse
    #scoping the variable i to the calling (parent) environment
    setinv <- function(inv) i <<- inv
    #create function to return the value of the matrix inverse
    getmean <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
}


## checks for a non-null value for the variable i
##if i has been populated, return the value
##if i is null, then calculate i (as the inverse of the matrix x),
##then set the value of i, and return i

cacheSolve <- function(x, ...) {
#check to see if the value of i has been set
  i <- x$getinv()
  #if i has been set then return i
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
  #if i has not been set then set it and return variable i
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
    
}
