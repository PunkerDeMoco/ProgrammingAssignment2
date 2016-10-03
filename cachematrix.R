## The functions cached the inverse of a computed matrix.  
## If the inverse matrix was already calculated, then it just retrieves its value from the cache. Otherwise, the inverse is calculated and retrieved.

## The first function creates an object list with the 'getters and setters' of our matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The second function takes the result of the first function and checks if the inverse matrix was calculated already. If so, it just cache the value of the inverse. If not, the function calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)){
        message("getting cached inverse matrix")
        return(m)
     }
    invx <- x$get()
    m <- solve(invx, ...)
    x$setinverse(m)
    m        
}
