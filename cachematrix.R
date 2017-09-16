##-----------------------------------------
## R-programming by Coursera (Week 3)
## Caching the inverse of matrix
##-----------------------------------------


##------------------------------------------
## function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse, 
## which is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() {
      x
    }
    
    setinverse <- function(inverse) {
      m <<- inverse 
    }
    
    getinverse <- function() {
      m
    }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

##------------------------------------------



##------------------------------------------
## function "cacheSolve" computes the inverse of the special "matrix" created with "makeCacheMatrix" function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)%*% data
  x$setinverse(m)
  m
}

##------------------------------------------
