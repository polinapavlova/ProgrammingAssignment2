## Matrix inversion is usually a costly computation. 
## The following pair of functions provides possibility of caching the inverse of a matrix rather than computing 
## it repeatedly.
## It is assumed that the matrix supplied is a square invertible matrix.

## This function creates a special "matrix" object, which is really a list containing a function to:
## 1) set the matrix
## 2) get the matrix
## 3) set the inversed matrix
## 4) get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  inversed_matrix <- NULL
  
  set_matrix <- function(y) {
           x <<- y
           inversed_matrix <<- NULL
  }
  
  get_matrix <- function() x
  set_inversed_matrix <- function(current_inversed_matrix) inversed_matrix <<- current_inversed_matrix
  get_inversed_matrix <- function() inversed_matrix
  list(set_matrix = set_matrix, get_matrix = get_matrix, set_inversed_matrix = set_inversed_matrix, get_inversed_matrix = get_inversed_matrix)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inversed_matrix <- x$get_inversed_matrix()
  
  if(!is.null(inversed_matrix)) {
    message("Getting cached data...")
    return(inversed_matrix)
  }
  
  data <- x$get_matrix()
  inversed_matrix <- solve(data, ...)
  x$set_inversed_matrix(inversed_matrix)
  
  inversed_matrix
}
