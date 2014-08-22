## The first function "makeCacheSolve" takes a matrix and creates a four element list of functions that will be called when running the second function "cacheSolve".  
## The second function "cacheSolve" takes the list that was returned by "makeCacheMatrix" and runs them in order to compute the inverse of the matrix that was initially passed to "makeCacheSolve".  
## Once the inverse matrix is created and stored, it is saved in cache and used in subsequent runs of cacheSolve (as long as the matrix is not changed).

## This function creates a list (of functions) that will be passed to cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
      
      ## Set the inverse matrix to NULL.
      inverse_M <- NULL
      set <- function(y) {
            x <<- y
            inverse_M <<- NULL
      }
      get <- function() x
      setMatrix <- function(mean) inverse_M <<- mean
      getMatrix <- function() inverse_M
      list(set = set, get = get,
           setMatrix = setMatrix,
           getMatrix = getMatrix)
}

## This function will check to see if there if the inverse matrix is stored in cache.
## If the inverse matrix is not cached, it computes and returns the inverse of the matrix passed to makeCacheMatrix.
cacheSolve <- function(x = matrix(), ...) {
      
      inverse_M <- x$getMatrix()
      ## Look for solution in cache.
      if(!is.null(inverse_M)) {
            message("getting cached data")
            return(inverse_M)
      }
      ## If not cached, run through the functions in the list that was passed.
      ## Due to lexical scoping, the x when calling the functions below is
      ## the x passed to makeCacheMatrix.
      matrix <- x$get()
      inverse_M <- solve(matrix, ...)
      x$setMatrix(inverse_M)
      ## Return the inverse of the matrix.
      inverse_M
}
