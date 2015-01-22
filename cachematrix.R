## An example of using these functions would be
## A <- matrix(c(1, 2, -2, -3), nrow = 2, ncol = 2, byrow = TRUE)
## which produces
##      [,1] [,2]
## [1,]    1    2
## [2,]   -2   -3
## Next enter
## B <- makeCacheMatrix(A)
## then
## cacheSolve(B) will return the inverse
##      [,1] [,2]
## [1,]   -3   -2
## [2,]    2    1


makeCacheMatrix <- function(x = matrix()) {
    ## This takes a solvable square matrix as input
    ## and stores its inverse in a cache
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}



cacheSolve <- function(x, ...) {
    ## cacheSolve takes an object created by makeCacheMatrix above
    ## as an input and produces the inverse of the matrix
    ## used as an input for makeCacheMatrix
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



