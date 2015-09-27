##makecacheMatrix creates a special "matrix", which is really a list containing a function to
  #set the value of the matrix
  #get the value of the matrix
  #set the value of the inverseMatrix
  #get the value of the inverseMatrix

makeCacheMatrix <- function(x = matrix()) {
	 i <- NULL
       set <- function(y) { ##For Setting the value of the matrix
             x <<- y
             i <<- NULL
         }
       get <- function() x ##For Getting the value of the matrix
       setInverseMatrix <- function(inverseMatrix) i <<- inverseMatrix ##For Setting value of inverse Matrix
       getInverseMatrix <- function() i ## For Getting value of inverse Matrix
      list(set = set, get = get,
                      setInverseMatrix = setInverseMatrix, 
                       getInverseMatrix = getInverseMatrix) 
}


## cacheSolve function calculates the inverseMatrix of the special "matrix" created with makeCacheMatrix function.
## However, it first checks to see if the inverseMatrix has already been calculated.
## If so, it gets the inverseMatrix from the cache using getInverseMatrix and skips the computation. 
##Otherwise, it calculates the inverseMatrix of the matrix and sets the value of the inverseMatrix in the cache via the setInverseMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		  i <- x$getInverseMatrix() ##Getting the value of the inverse matrix
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  data <- x$get() ##For Getting the value of the matrix
  i <- solve(data,tol = 1e-20) ##Setting the tolerance low to accomodate smaller numbers in the matrix
  x$setInverseMatrix(i) ##For Setting value of inverse Matrix so that it can be cached and used next time
  i
}
