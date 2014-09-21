## These two functions were created as part of the Coursera R programming course 007.

## This function creates a marix then solves for the inverse of the matrix
## and caches the inverse for future use.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){
          x <<- y
          s <<- NULL
    }
    get<- function () x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set=set,get=get,
        setinverse=setinverse, 
        getinverse=getinverse)
}

## This function is used to solve for the inverse of a matrix.
## If the inverse of the matrix has already been solved and cached
## the cached results will be returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
    if(!is.null(s)){
          message("getting cached data")
          return(s)
          
    }
    data <- x$get()
    s <- solve(data,...)
    x$setinverse(s)
    s

}

