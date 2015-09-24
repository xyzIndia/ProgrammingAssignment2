## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	 i <- NULL
       set <- function(y) {
             x <<- y
             i <<- NULL
         }
       get <- function() x
       setInverseMatrix <- function(inverseMatrix) i <<- inverseMatrix
       getInverseMatrix <- function() i
      list(set = set, get = get,
                      setInverseMatrix = setInverseMatrix,
                       getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		  i <- x$getInverseMatrix()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data,tol = 1e-20)
  x$setInverseMatrix(i)
  i
}
