#creates the list of functions that will plug into the solvecache function
#if an inverse of the matrix as been calculated, it will store here for future use
#thus, the cacheSolve function will not have to recompute the inverse if it can pull from here

makeCacheMatrix <- function(x = matrix()) {
      
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinv <- function(solve) i <<- solve
      getinv <- function() i
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinv()
      #if statement looks to see if the value of inverted matrix exists in the cache
      #if it already exists, then it pulls it in and displays
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      #if it doesn't exist, then it will calculate it using solve and then display
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}

#I observed that the cacheSolve function will not work on its own, the MakeCacheMatrix function needs to be defined in a new variable with the matrix
# or it needs to be included embedded within the CacheSolve function in order to work properly
# cacheSolve(makeCacheMatrix(B))
#b3 <- makeCacheMatrix(B2) where B2 is a matrix
#cacheSolve(b3)
