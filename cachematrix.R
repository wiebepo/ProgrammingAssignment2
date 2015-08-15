## leverage the lexical nature of the namespace to improve performance via caching

## Create functions and namespace associated with the matrix for caching the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
         x <<- y
         i <<- NULL
       }
     get <- function() x
     setinv <- function(inv) i <<- inv
     getinv <- function() i
     list(set = set, get = get,
                   setinv = setinv,
                   getinv = getinv)
}

## Return the cached matrix inversion if available, otherwise calculate

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
     i <- x$getinv()
     if(is.null(i)) {
       data <- x$get()
       i <- solve(data)
       x$setinv(i)
       }
     i
}
