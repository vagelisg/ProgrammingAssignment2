## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix puts a matrix to a cached memory, in order to 
## have the result ready after the first calcultion
## set and get puts the matrix to cache
## setinverse, and getinverse gets and puts an inversed matrix to cache

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve return cashed matrix if exists or calculate inversed 
## matrix and also cache the results

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      #put to M local variable the inverse matrix
      m <- x$getinverse()
      # check if exist the invert matrix in cache
      if(!is.null(m)) {
            message("getting cached data")
            #return the cashed matrix and exit from the function
            return(m)
      }
      # put to local variable data the matrix and calculate the invert
      data <- x$get()
      m <- solve(data, ...)
      #save to the cashdata the invert matrix
      x$setinverse(m)
      #Return the invert matrix
      m
}
