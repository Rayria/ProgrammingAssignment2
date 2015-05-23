## The two following functions are used to create a special 
## object to store a matrix, calculate the inverse of the matrix,
## and cache the inverse

## The following function defines 'x' which is a list of 4 
## functions to store the matrix and cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) { ##  store matrix
    x <<- y
    matinv <<- NULL
  }
  get <- function() x ## extract matrix
  setinverse <- function(inverse) matinv <<- inverse ## store inverse
  getinverse <- function() matinv  ## extract inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function computes the inverse of the matrix
## stored by the function above, assuming matrix is invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matinv <- x$getinverse()
  if(!is.null(matinv)) {  ## checks if inverse is already calculated
    message("getting cached data") 
    return(matinv)   ## previously caclculated inverse
  }
  data <- x$get()
  matinv <- solve(data, ...) ## otherwise compute inverse
  x$setinverse(matinv)  ## store inverse
  matinv   
}
