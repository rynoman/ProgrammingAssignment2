## Function: makeCacheMatrix
## This function is based on example function - "makeVector"
## Matrix function is used to create matrix from the given set of values.
## Matrix replaces Numeric function in the example code.

## Function: cacheSolve
## This function is based on example function - "cachemean"
## Solve function is used to solve equation containing a square numeric or complex matrix.
## Solve replaces Mean function in the example code.


## Function: makeCacheMatrix - Takes an argument x of type Matrix
## It returns a list vector of 4 functions: set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function()
    x
  setinverse <- function(inverse)
    i <<- inverse
  getinverse <- function()
    i
  list(
    set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
 
}


## Function: cacheSolve - Output of "makeCacheMatrix" is an argument for cacheSolve.
## It returns an inverse matrix as output either by computing the solve function or fetching the cashed data. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}



############### Test data ###############
# > a<-matrix(11:14,2,2)
# > b<-makeCacheMatrix(a)

#### Execute cacheSolve for 1st time ####
# > cacheSolve(b)    <--- The function computes the inverse of Matrix
# [,1] [,2]
# [1,]   -7  6.5
# [2,]    6 -5.5
# > cacheSolve(b)
# getting cached data <--- Repeating the cacheSolve function gets the cached data.
# [,1] [,2]
# [1,]   -7  6.5
# [2,]    6 -5.5
#########################################
