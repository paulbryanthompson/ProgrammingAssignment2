makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y){ ## defining code to set value of the 
    x <<- y
    m <<- NULL
  }
  
  get <- function() x ## returns value of x
  setmatrix <- function(solve) m <<- solve ## sets inverse matrix using Solve
  getmatrix <- function() m ## returns matrix
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix) ## list of methods for the function
}

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
