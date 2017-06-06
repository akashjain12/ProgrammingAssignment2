makeCacheMatrix <- function(x = matrix()){
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
    }
   get <- function()x
   setinverse <- function(solve) 
     inverse <<- solve
   getinverse <- function() inverse
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
  
}

# the second part of the assignment begins here

cacheSolve <- function(x, ...){
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting matrix data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

