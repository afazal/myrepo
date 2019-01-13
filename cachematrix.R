## The purpose of this assignment is to write functions 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input provided by us 

makeCacheMatrix <- function(x = matrix()) {

  
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  
  get <- function() x
  setinver <- function(inverse) inver <<- inverse
  getinver <- function() inver
  list(set = set, get = get,        
       setinver = setinver, 
       getinver = getinver)
  
}


## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. We check the condition that if the inverse 
##has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the 
## inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached result")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinver(inver)
  inver
  
  
  
}
##if we choose a square matrix (3x3, and random normal 9 values) as our input
## m <- matrix(rnorm(9),3,3)
## check <- makeCacheMatrix(m)
## cacheSolve(check)

##          [,1]        [,2]        [,3]       
## [1,] -0.40990528  -0.83132912  0.478656942 
## [2,]  0.05055042   0.04968844  1.256305834  
## [3,]  0.23250167  -0.35161853 -0.009251055 
