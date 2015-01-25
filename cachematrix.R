makeCacheMatrix <- function(x = matrix()) {
  ## This function creates the space within memory of the parent environment to hold 
  ## a list of items including the original matrix and the inverted matrix
    
  m <- NULL ## set this first to halt m being searched in other environments which could cause conflicts
  
  set <- function(y){ 
    x <<- y
    m <<- NULL
  }
  
  get <- function() x ## returns value of x
  setmatrix <- function(solve) m <<- solve ## sets inverse matrix using Solve
  getmatrix <- function() m ## returns matrix
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix) ## create a list of methods for the function
}

cacheSolve <- function(x=matrix(), ...) {
  ## This function will evaluate the cached memory space created with function makeCacheMatrix
  ## to see if the matrix is already present in the list output from makeCacheMatrix
  ## Items in the list are extracted by name using $
  ## If matrix exsits, the inverted matrix will be pulled from cache and NOT calculated again
  ## If it is not stored, then the inverse matrix is calculated
  
  m <- x$getmatrix() ## returns the named item from the list (a matrix)
  if(!is.null(m)){ ## checks to see if the matrix is already in cache, if it is then matrix is returned
    message("getting cached data")
    return(m)
  }
  matrix <- x$get() ## if m is not in cache, calc inverse of matrix m
  m <- solve(matrix, ...) ## calc inverse matrix
  x$setmatrix(m) ## set this value in cache using 'setmatrix' so next time run it will pull from cache
  m ## return inverse matrix
}
