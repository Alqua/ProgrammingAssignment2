## Put comments here that give an overall description of what your
## functions do

## This function create a special matrix which we can use later to cache its invert.
## Inverting a matrix can take a lot of ressources to calculate, and in some
##case it can be handy to be able to have it directly in the cache

makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    set <- function(y){
        x <<- y
        j <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) j <<- inverse
    getInverse <- function() j 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}





## This is the actual fonction the first calculate the invert of the matrix
##and if it already has been done, it directly look for the informations in the cache


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    j <- x$getInverse()
    if(!is.null(j)){
        message("getting cached data")
        return(j)
    }
    mat <- x$get()
    j <- solve(mat,...)
    x$setInverse(j)
    j
}
