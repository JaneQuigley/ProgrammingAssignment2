
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## the first function makeCacheMatrix returns nothing, 
## just creates a 'box' into which go a list of subfunctions, a variable 'm' which is initialized to be NULL, and a function, 'set'

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x   ## this function simply returns x (a matrix).
  setinverse <- function(solve) m <<- solve   ## this just sets the value of the input (solve) into a variable (m) and stores it in 'the box' 
  getinverse <- function() m   ## this just returns the value of m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}   ## this just creates a list/object that is a list of the four subfunctions

cacheSolve <- function(x) {
  myInverse <- makeCacheMatrix(x)
  m <- myInverse$getinverse()  ## gets 'm' from the box to check it.
  if(!is.null(m)) {
    message("getting inverse matrix from cache")
    return(m)
  }  ## here, if an inverse to the matrix is already stored in 'm',(if 'm' is not null), this step returns it.  
  
##otherwise,if m is NULL, it creates an object, NewMatrix, and gets the value of x to be the new input matrix
  NewMatrix <- myInverse$get()
  m <- solve(NewMatrix)  ## this solves for the inverse of 'x' and sets m to be that inverse 
  myInverse$setinverse(m)  ## this passes the new value of m to the $setinverse function, which replaces the value of the old m (in the cache) with the new value for m.
  m ## this returns the value of m
}