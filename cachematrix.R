### The two functions below use the <<- operator to create a 
## special object that stores a numeric matrix and cache's its
## inverse

## The function makeCacheMatrix creates a special matrix that 
## consists of a list of four functions: set, get, setinverse
## and getinverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get= get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of the matrix
## created using makeCacheMatrix. It checks if the inverse has
## been previously calculated and if so, gets the inverse from
## the cache. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache with the 
## setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
