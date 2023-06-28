## Put comments here that give an overall description of what your
## functions do

## for this assignment the <<- operator is introduced to assign a value to an object in 
# an environment different from the current one. 
# makeCacheMatrix creates a matrix that can cash itself and get and set the inverse 

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) invmatrix <<- solve
  getinverse <- function() invmatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve computes the inverse of the matrix we created with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  data <- x$get()
  invmatrix <- solve(data, ...)
  x$setinverse(invmatrix)
  invmatrix
  
}

# now I test if my function works 

my_matrix <- makeCacheMatrix()

my_matrix$set(matrix(1:4,2,2))

my_matrix$get()

cacheSolve(my_matrix)

my_matrix$getinverse()
