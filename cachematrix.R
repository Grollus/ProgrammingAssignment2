## Functions create a matrix and store it, allowing you to retrieve 
## and solve for the inverse. cacheSolve will check for an existing
## solution and if none exists, will solve for and store the inverse.


## Function creates and caches a matrix.  Once created, the function 
## allows you to solve and cache the inverse matrix.  Able to check
## for the existence of matrix and inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list( set = set, get = get, 
              setsolve = setsolve,
              getsolve = getsolve)
}


## Checks for a solved matrix. If cache exists, returns the solution
## If the solution does not exist, solves for the inverse and caches 
## it.

cacheSolve <- function(x, ...) {
          m <- x$getsolve()
          if(!is.null(m)) {
                  message("getting cached data")
                  return(m)  
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setsolve(m)
          m
}

