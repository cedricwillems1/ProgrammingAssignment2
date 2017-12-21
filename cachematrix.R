## First of all you want this formula to work with any matrix, so I created a matrix for myself to test with
## Now first you need to set/get the value and later set/get the inverse of the matrix
## Why would you cache the matrix? Because it saves a lot op RAM/space

## Cache the inverse we have to write 
## Then set the function for y 
## Then set, use <<- because R needs to search not in global environment, but local
## Then get the function and set the inverse

## Function is a list containing
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    INV <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}

## Now we need to solve the cached matrix, 
## if it is not calculated it could retrieve the inverse of the data.
## This function assumes that the matrix is always invertible.
## The following function returns the inverse of the matrix. 
## It first checks if the inverse has already been computed. 
## If so, it gets the result and skips the computation. 
## If not, it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

