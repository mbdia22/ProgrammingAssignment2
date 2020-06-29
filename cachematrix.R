# Functions to compute the inverse of a matrix with caching.


# Caching function for a matrix argument.  Returns a vector 
# of getters and setters for the matrix itself, as well as 
# placeholders for the matrix inverse calculation.

makeCacheMatrix <- function(x = matrix()) {
                  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

# Calculates the inverse of an assumed square matrix, or retrieves
# a previously calculated inverse from cache, given a list argument
# from the function makeCacheMatrix().

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}


