## My functions should get the inverse of the matrix in makeCacheMatrix and produce it in cacheSolve

## It will produce the inverse of the matrix and store it

makeCacheMatrix <- function(x = matrix()) {
                s <- NULL
                set <- function(y) {
                        x <<- y
                        s <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) s <<- solve
                getinverse <- function() s
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## Produce the stored version of the matrix

cacheSolve <- function(x, ...) {
        s<-x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data<-x$get()
        s<-solve(data,...)
        x$setinverse(s)
        s
}
