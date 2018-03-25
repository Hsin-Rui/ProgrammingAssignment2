## Put comments here that give an overall description of what your
## functions do

## The following function produces a list of functions which could:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # a function that sets the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # a function that gets the value of the matrix
        get <- function() {
                x
        }
        # a function that set the inverse of the matrix
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        # a function that get the inverse of the matrix
        getInverse <- function() {
                inv
        }
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the inversion of matrix "x", if it has not been calculated yet.
## If there the inversion has been calculated, then it just returns the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        # if inv matrix has already been there, then the function returns this value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # if inv matrix has not been calculated, it will be calculated using function solve()
        mat <- x$get()
        inv <- solve(mat, ...)
        # set the inv matrix value
        x$setInverse(inv)
        # return matrix
        inv
}
