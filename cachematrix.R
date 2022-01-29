## The two functions (1) makeCacheMatrix and (2) cacheSolve cache the inverse of matrix x

## (1) This function creates a special "matrix" object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL                        ##inv is is the inverse of matrix x, which is initially NULL
      set <- function(y){                ##set():
            x <<- y                         ##1. assigns matrix x's new value to parent environment
            inv <<- NULL                    ##2. if there is a new matrix x, resets value of inv to NULL in parent environment
      }
      get <-function(){                  ##get() returns value of matrix x where called
            x
      }
      setinv <- function(inverse) {      ##setinv() assigns value of inv to parent environment
            inv <<- inverse
      }
      getinv <- function() {             ##getinv() returns value of inv where called
            inv
      }
      list(set = set, get = get,         ##special vector (list) containing all four functions so they can be called with $
           setinv = setinv, 
           getinv = getinv)             
                                                                        
}


## (2) This function computes the inverse of the special "matrix" returned by makeCacheMatrix above 
## If the inverse has already been calculated and the matrix has not changed, cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
      inv <- x$getinv()                       ##retrieves inv through getinv(), which could be NULL if it hasn't been calculated
      if(!is.null(inv)) {                     ##if inv is not NULL, returns the existing inv 
            message("getting cached data")
            return(inv)
      }
      
      ##If inv is NULL, program will continue here:
      
      data <- x$get()                        ##retrieves matrix x through get() and assigns it to data
      inv <- solve(data, ...)                ##computes the inverse of matrix x and assigns it to inv (in this environment)
      x$setinv(inv)                          ##assigns value of inv to parent environment through setinv() - thus caching it
      inv                                    ##prints inv (the inverse of matrix x)
}
