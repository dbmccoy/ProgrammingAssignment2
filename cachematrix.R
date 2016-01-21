## Pass a inversible square matrix as the argument into
## makeCacheMatrix, store the result.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {x}
  setinverse <- function(solve) {i <<- solve}
  getinverse <- function() {i}
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## pass the result of the function above as an argument
## into cacheSolve to either solve the inverse of the 
## matrix, or retrieve it from the cache if available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  message("cache is null, solving for inverse")
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
