#This assigment creates 2 functions to cache the operation of getting the inverse of a matrix
#The first function makeCacheMatrix is the function that creates the cache
#It stores the value of the matrix and its inverse from the previous computation
#cacheSolve is the function that computes the inverse
#It first checks the cache, and if there's no value, computes the inverse


#function to create the cache
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#function to get the inverse of the matrix (from cache or afresh)
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}


