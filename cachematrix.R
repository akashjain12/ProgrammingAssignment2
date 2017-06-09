makeCacheMatrix <- function(x = matrix()){
  inverse <- NULL   # This variable will hold the inverse matrix, but for now is being set to null
  set <- function(y) {
    x <<- y     # update the old matrix to the new matrix
    inverse <<- NULL      # reset the inverse of the matrix of the new matrix.
    }
   get <- function()x        # This function Will get the actual matrix
   setinverse <- function(solve)      # This function will set the value of the inverse of the matrix
     inverse <<- solve             
   getinverse <- function() inverse    # This function will get the inverse of the matrix  
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    # This list with the available functions
  
  
}

# the second part of the assignment begins here

# The following function calculates the inverse of the special "matrix" created with the above function (makeCacheMatrix). 
# However, it first checks to see if the inverse of the matrix has already been calculated. 
# If so, it gets the inverse of the matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix of the data and sets the value 
# of the inverse of the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...){
  inverse <- x$getinverse()    # holds the inverse of the matrix
  if(!is.null(inverse)){       # check if this inverse of the matrix has been calculated
    message("getting matrix data")       # if so, prints this message "getting cached data" and
    return(inverse)             # returns the inverse of the matrix and skips the computation.
  }
  data <- x$get()
  inverse <- solve(data, ...)     # calculating the inverse of the matrix using solve
  x$setinverse(inverse)         # updating the variable that holds the inverse of the matrix
  inverse
}

