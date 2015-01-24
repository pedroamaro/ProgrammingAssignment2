## With these functions, its possible to save a matrix that can be invertible and determine its inverse.

## With the first function (makeCacheMatrix), we create a matrix and we can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setmatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)  
}


## With the second function, we calculate the inverse of matrix x. If the inverse is already in cache, 
## then it is returned.

cacheSolve <- function(x, ...) {
        i<- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
