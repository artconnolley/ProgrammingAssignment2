## Creates a matrix for later use by cacheSolve and then caches an inverse once called in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrix <- function(a) m <<- a
    getMatrix <- function() m
    list(set = set, get = get, 
         setMatrix = setMatrix, 
         getMatrix = getMatrix)
}


## Takes object created by makeCacheMatrix and solves the inv storing it in a cache for later use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getMatrix()
    if((!is.null(m))) {
        message("data stream incomming!")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data)
    x$setMatrix(m)
    m
}