## Together, these will make a special matrix that can cache its 
## inverse, then the inverse is either computed or retrieved from the cache

## Creates list with functions to set/get a matrix's value and 
## set/get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Computes special matrix inverse, but will instead 
## retrieve from cache if already calculated

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get
  inv <- solve(data())
  x$setinverse(inv)
  inv
        
}
