## Icaro Agostino - https://github.com/icaroagostino

## The functions below aim to reduce the computational cost when 
## it is necessary to calculate the inverse of the same matrix
## repeatedly. 

## At the end of this code you will find a short example

## This function creates a special "matrix" object that
## can save its inverse for a future call

makeCacheMatrix <- function(x = matrix()) {
  
  # Save the matrix and create a empty object to save the inverse
  inv <- NULL
  set <- function(y) {
    x   <<- y
    inv <<- NULL
  }
  # Methods to computes the inverse of the matrix and save
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function computes the inverse of the special "matrix"
## using the data and methods of provided by makeCacheMatrix
## and save inside the special object. If the inverse has
## already been calculated then this function retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # Check if the special object already has an inverted matrix
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverted matrix")
    return(inv)
  }
  # Computes the inverse of the special "matrix"
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv ## Return a matrix that is the inverse of 'x'
}

#### Example ####

## If you want to run this example you need to run the
## funcions makeCacheMatrix() and cacheSolve() above and
## remove one "#" from the beginning of each line below

### Creating a square inversible matrix
#
#myMatrix <- matrix(c(5, 1, 0,
#                     3,-1, 2,
#                     4, 0,-1), nrow=3, byrow=TRUE)
#
### Creating a special "matrix" object using makeCacheMatrix()
#
#myCacheMatrix <- makeCacheMatrix(myMatrix)
#
### Computes the inverse of the special "matrix"
#
#cacheSolve(myCacheMatrix)
#
##>        [,1]    [,2]   [,3]
##> [1,] 0.0625  0.0625  0.125
##> [2,] 0.6875 -0.3125 -0.625
##> [3,] 0.2500  0.2500 -0.500
#
### Running cacheSolve() again!
#
#cacheSolve(myCacheMatrix)
#
##> getting cached inverted matrix
##>        [,1]    [,2]   [,3]
##> [1,] 0.0625  0.0625  0.125
##> [2,] 0.6875 -0.3125 -0.625
##> [3,] 0.2500  0.2500 -0.500
#
### In the second run cacheSolve() only retrieve the
### inverse from the cache