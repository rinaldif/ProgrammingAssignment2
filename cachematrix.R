## Hi, below you will find two functions that allow you to create a special object that 
## stores a matrix and caches it's inverse. This way, if you need it again, you can use
## the cached one instead of calculating it over and over again. If the inverse has not
## already been stored, it is calculated.

## This function, makeCacheMatrix, creates a special matrix which contains a list of 
## functions that allow you to set the value of the matrix (set) , get the value of 
## the matrix (get), set the value of the inverse (setinv) and get the value of the 
## inverse (getinv).

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## This second function, cacheSolve, first checks to see if the inverse has already 
## been calculated (by checking if inv is NULL or not) and, if so, it gets the inverse 
## from the cache (with the command return(inv)) and skips the remaining steps. If not, 
## it calculates the inverse of the matrix (using the solve() function) and sets that 
## value in the cache via the setinv function (which was defined in the first function).

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}
