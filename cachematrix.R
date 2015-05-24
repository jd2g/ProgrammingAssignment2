## This code is for Coursera Data Science Specialization 2015.
## It is based in the Example: "Caching the Mean of a Vector" 
## provided in the readme to introduce the assign operator `<<-`
## Contains a pair of functions to compute the inverse of a matrix
## To get the result the function includes a cache to store the value
## to speed calculations.


## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ##Set the variable to store the value of the inverse value of a matrix
  mInv <- NULL
  ## The Set and Get methods to create the matrix in the environment
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(mInverse) inv <<- mInverse
  getinverse <- function() mInv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function return the inverse of a matrix
## If the result is in cache, returns its value so we can return the result faster
## If the result is not in the cache, compute its value
cacheSolve <- function(x, ...) {
    ## Get the inverse of the matrix 'x'
    mInverse <- x$getinverse()
    if(!is.null(mInverse)) {
        message("Getting cached data...")
        ## REturn the stored value instead of the cumputed value.
        return(mInverse)
    }
    #Get the Inverse of the Matrix
    m <- x$get()
    #Solve the Inverse of The Matrix m
    mInverse <- solve(m, ...)
    #Set the Inverse of the Matrix
    x$setinverse(mInverse)
    #Return the Inverse of the matrix m
    return(mInverse)
}
