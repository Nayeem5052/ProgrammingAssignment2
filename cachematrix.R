## This function takes a matrix as its argument

## It contains four functions
## 01 It caches a new matrix (by set function) in the variable 'x' as well as clears the stored inverse
## 02 It returns the cached matrix (by get function)
## 03 It caches the inverse provided as argument (into the setinverse function) in the variable 'inverse'
## 04 It returns the cached value of inverse (by getinverse function)

## It returns a list containing these four functions


makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inv) {
        inverse <<- inv
    }
    getinverse <- function() {
        inverse
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function takes a list containing the four functions returned by the 'makeCachedMatrix' as its argument
## If the list already contains the inverse of the matrix, then it returns the inverse without computing it
## Otherwise it computes the inverse and caches it in the list as well as returns the inverse


cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}


## Example

cached_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(cached_matrix)
cacheSolve(cached_matrix)

## Updating the matrix

cached_matrix$set(matrix(c(1:8, 0), 3, 3))
cacheSolve(cached_matrix)
cacheSolve(cached_matrix)
