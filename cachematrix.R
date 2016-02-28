## The first function, makeCacheMatrix, takes the value of the matrix it is passed
## and calculates the inverse and caches that value.  In the second function,
## cacheSolve, the functions first checks to see if a value for that inverse matrix  
## already exists.  If it does already exist in the cache, the function gives a 
## message that it exists in the cache and retrieves the value from the cache.
## If it does not exist in the cache, it calculates the inverse of the matrix.

## makeCacheMatrix - use this function to set the value of the matrix and assign 
## it's values to the parent enviorment.  Then get the value of the matrix and 
## solve for its inverse; cache the inverse.  

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
 ## get the value of the matrix
    get <- function() x
  ## set the value of the inverse
    setinverse <- function(solve) m <<- solve
  ## get the value of the inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

## cacheSolve:  This function first checks to see if the inverse of the matrix is
## cached.  If it is, it returns the value of the cached matrix.  If it isn't, the 
## second half calculates the inverse (m <- solve(data, ...)), and returns the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
