## Coursera R Programming Assignment 2: Lexical Scoping
## Caching inverse of a matrix

## The makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

##Function to calculate inverse of spl matrix created by makeCachematrix
##First checks if inverse is already computed
##If yes, uses value in cache
##If no, computes and sets inverse value via setinverse

cacheSolve <- function (x, ...) {
       i <- x$getinverse()
       if(!is.null(i)) {
            message("Getting cached data...")
            return(i)
       }
       data <- x$get()
       i <- solve(data, ...)
       x$setinverse(i)
       i
}

