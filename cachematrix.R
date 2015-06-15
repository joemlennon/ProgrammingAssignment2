## Put comments here that give an overall description of what your
## functions do

## Initialize JML test
## ## @x: assume an invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## 1. Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## 2. Get the matrix called x
    get <- function() x
    ## 3. Set the inverse using solve
    setinverse <- function(solve) inv <<- solve
    ## 4. Get the inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Cache the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## @x: output of makeCacheMatrix()
    ## return: inverse of the original matrix input to makeCacheMatrix()
    
    ## If Inverse is in cache, get it and return it, otherwise calc it
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    ## Calc it using the input matrix called x
    datamatrix <- x$get()
    inv <- solve(datamatrix, ...)
    ## set value of the inverse in cache using setinverse
    x$setinverse(inv)
       return(inv)
}
