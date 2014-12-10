## set function, stores a value in object x   
## get function gives values stores in object
## setinverse function stores inverse in object inv
## getinverse function gives inverse of matix
##
##
## makeCacheMatrix function stores matrix and returns list object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() { x }
  setinverse <- function(inverse) { inv <<- inverse }
  getinverse <- function() { inv }
  list(set = set, get = get,setinverse = setinverse, 
      getinverse = getinverse)
}


## cacheSolve function evaluate inverse of matrix and 
## stores it in cache
##
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  s <- x$get()
  inv <- s^-1
  x$setinverse(inv)
  inv
}
