## makeCacheMatrix: creates a special vector
## which is a list containing functions to
## a) set the value of the vector (set)
## b) get the value of the vector (get)
## c) get the value of the matrix inverse (getinverse)
## d) set the value of the matrix inverse (setinverse)

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse to null
  i <- NULL

  # set: set the new value of a matrix
  set <- function(y) {
    # store the value in x
    x <<- y
    # initialize inverse to NULL since we have
    # a new matrix to invert
    i <<- NULL
  }
  
  # get: retrieve the current matrix
  get <- function() x
  
  # setinverse: store the inverse, caching it
  setinverse <- function(inverse) i <<- inverse
  
  # getinverse: retrieve the current inverse
  getinverse <- function() i
  
  # return the special list containing the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: takes in the special list created
## by makeCacheMatrix and returns the inverse of the
## matrix from the cache, if already cacheed or after
## computing using solve(), if not cached. In addition,
## the inverse is cached, if not already present in
## the cache

cacheSolve <- function(x, ...) {
  # retrieve the inverse of the matrix from the cache
  i <- x$getinverse()
  
  # check if hit was found in the cache
  if(!is.null(i)) {
    message("getting cached data")
    # cache had our matrix inverse, return it
    return(i)
    # function execution stops here
    # if inverse is already cached
  }
  
  # cache did not have our matrix inverse
  # 1) retrieve the matrix
  data <- x$get()
  # 2) compute its inverse
  i <- solve(data, ...)
  # 3) cache the inverse
  x$setinverse(i)
  # 4) return the inverse
  i
}


###################### Sample run ############################
### > B <- matrix(c(1,0,5,2,1,6,3,4,0), nrow = 3 , ncol = 3)
### > z = makeCacheMatrix(B)

### Round 1: inverse not yet cached ###

### > cacheSolve(z)
### [,1] [,2] [,3]
### [1,]  -24   18    5
### [2,]   20  -15   -4
### [3,]   -5    4    1

### Round 2: inverse already cached ###

### > cacheSolve(z)
### getting cached data
### [,1] [,2] [,3]
### [1,]  -24   18    5
### [2,]   20  -15   -4
### [3,]   -5    4    1
##############################################################
