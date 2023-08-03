## write s short coomment describing this function
## this function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <-function(x= matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <-function(solveMatrix) inv <<-solveMatrix
    getInverse <-function()inv
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## write a short comment describing this function
## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## if the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <-function(x,...){
    ##return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    ##just return the inverse if its already been calculated
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    ##otherwise, get the matrix, calculate the inverse, and set the inverse to the cache
    data <- x$get()
    m <- solve(data,...)
    x$setInverse(m)
}