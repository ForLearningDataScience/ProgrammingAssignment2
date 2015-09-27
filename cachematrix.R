## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

### makeCacheMatrix function creates a special matrix,
### which is a list containing a function to
### 1. set the value of the matrix
### 2. get the value of the matrix
### 3. set the value of the inverse
### 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Write a short comment describing this function

### cacheSolve function calculates the mean of the special "matrix"
### created with the makeCacheMatri function.
### It will first checks to see if the inverse has already been calculated.
### If so, it gets the mean from the cache and skips the computation.
### Otherwise, it calculates the inverse of the data and sets the value
### of the inverser in the cache via the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
