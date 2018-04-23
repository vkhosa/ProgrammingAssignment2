#Assignment: Caching the Inverse of a Matrix
# 1. make a catch matric

makeCacheMatrix <- function(x = matrix()) {
  	inv <- NULL
    set <- function(y) {
    x <<- y
    inv <<- NULL
   }
    get <- function() x
    set_inv <- function(inverse) inv <<- inverse
    get_inv <- function() inv
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}	 

# 2. make a catche matrix

cacheSolve <- function(x, ...) {
            inv <- x$get_inv()
            if(!is.null(inv)) {
            message("Getting Cached Result")
            return(inv)
        }
            data <- x$get()
            inv <- solve(data, ...)
            x$set_inv(inv)
            inv
}	 
