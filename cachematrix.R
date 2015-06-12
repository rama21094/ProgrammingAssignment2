## Put comments here that give an overall description of what your
## functions do
#
#
# The following two functions help in calculating inverse of a matrix. Since
# this calculation is  computationally intensive the result can be stored in the
# cache memory. These functions store the value in the cache the first it is
# calculated and later the value is accessed from the cache

## Write a short comment describing this function
#
# The first function, `makeCacheMatrix` creates a special "vector", which is
# a list containing 4 functions to
# 
# 1.  set the value of the vector
# 2.  get the value of the vector
# 3.  set the value of the inverse of the matrix
# 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
#
# The following function calculates the inverse of the special "vector" created
# with the above function. However, it first checks to see if the inverse has
# already been calculated. If so, it `get`s the inverse from the cache and skips
# the computation. Otherwise, it calculates the inverse of the data and sets the
# value of the inverse in the cache via the `setinv` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
        
