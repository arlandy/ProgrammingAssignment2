makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(inverse) {
        m <<- inverse
    }
    getInverse <- function() {
        m
    }
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x = matrix(), ...) {
    m <- x$getInverse();
    if(!is.null(m)){
        message("getting cached data")
        return(m)        
    }
    
    data <- x$get();
    if(nrow(data) != ncol(data)){
        stop("Must be a square matrix")
    } else if(det(data) == 0){
        stop("Must be an invertible matrix")
    }
    m <- solve(data, ...)
    x$setInverse(m);
    m
}

cacheMatrix<-makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
cacheSolve(cacheMatrix)
#FROM CACHE
cacheSolve(cacheMatrix)
