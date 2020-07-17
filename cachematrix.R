makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The functions here are used to make a special object that saves a matrix and caches its inverse. 
##The first function, makeCacheMatrix make a special “matrix”, This is really a list with the function to:

#get the value of the matrix

#set the value of the matrix

#get the value of the inverse

#set the value of the inverse

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
        ## Return a matrix that is the inverse of 'x'
R <- matrix(c(2:6),2,2)
R1 <- makeCacheMatrix(R)
cacheSolve(R1)
