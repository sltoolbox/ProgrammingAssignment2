##    makeCacheMatrix is a function that returns a list of functions
##      set =  set the value of the matrix
##      get =  get the value of the matrix
##      setinverse = sets the cache value
##      getinverse = gets the cache value

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## Calculate the inverse of the "matrix" created with the above function
## it uses the cache if not null or calculates the matrix inverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}
