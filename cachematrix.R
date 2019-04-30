## makeCacheMatrix is a special function that stores a matrix and its inverted version

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        ## set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL ## clear the cache (inverted matrix)
        }
        get <- function() x ## return the matrix
        setinv <- function(inverse) inv <<- inverse ## set the cache (inverted matrix)
        getinv <- function() inv ## return the cache (inverted matrix)
        list(set = set, get = get, ## funtion returns
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve invert a matrix and cache it if the it doesn't have the inverted matrix in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        ## there is a cached inverted matrix
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## there is not a cached inverted matrix
        data <- x$get()
        ## inverted the matrix (data)
        inv <- solve(data, ...)
        ## set the cached inverted matrix
        x$setinv(inv)
        inv
}
