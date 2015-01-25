## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a function which creates a special "matrix"
## object that can cache its inverse

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL  # Makes a NULL variable called m within the function
  set <- function(y) {  # 'set' is a function that assigns x and m 
    x <<- y             # in the global environment
    m <<- NULL
  }
  get <- function() x  # 'get' is a function that stores the input matrix
  setinverse <- function(solve) m <<- solve  # uses the 'solve' function
  getinverse <- function() m  # and assigns the inverse matrix to the getinverse and setinverse
  list(set = set, get = get,  
       setinverse = setinverse,
       getinverse = getinverse)  # creates a list that can be accessed
}


## Write a short comment describing this function

## The cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()  # 'm' is assigned the 'getinverse' 
  if(!is.null(m)) {  # checks if there is a cached matrix called 'm'
    message("getting cached data")
    return(m)  # if it finds a cached inverse matrix, it returns the matrix
  }
  data <- x$get()  # if there is no cached inverse matrix, it retrieves the stored matrix
  m <- solve(data, ...)  # creates an inverse matrix and stores in 'm'
  x$setinverse(m)  # Returns inverse of 'x' and cache the matrix
  m  # displays the inverse matrix
}
