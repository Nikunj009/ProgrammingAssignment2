makeCacheMatrix <- function(x = matrix()) {
      m <- NULL                                       ##initializing as NULL
      set <- function(y)  {                           ##defining the set function
            x <<- y
            m <<- NULL
      }
      get <- function() x                             ##returns x
      setinverse <- function(solve) m <<- solve       ##solve used to calculate inverse
      getinverse <- function() m                      ##returns m
      list(set = set, get = get,                      ##list
           setinverse = setinverse,
           getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
      
      m <- x$getinverse()
      if(!is.null(m)) {             ##Checking whether inverse already calculated or not
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m                             ## Returns a matrix that is the inverse of 'x'
}


